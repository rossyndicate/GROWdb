delineate_from_flowacc <- function(site,
                                   flow_acc_raster,
                                   flow_dir_raster,
                                   rivers            = NULL,
                                   snap_method       = c("point", "radius", "river"),
                                   snap_radius_cells = 1,
                                   snap_river_m      = 5000,
                                   search_buffer_m   = 5000,
                                   id_col            = "rowid",
                                   file_path         = "data/intl_watersheds_refined/") {
  
  stopifnot(inherits(site,            "sf"))
  stopifnot(inherits(flow_acc_raster, "SpatRaster"))
  stopifnot(inherits(flow_dir_raster, "SpatRaster"))
  
  snap_method <- match.arg(snap_method)
  
  if (snap_method == "river" && is.null(rivers))
    stop("snap_method = 'river' requires a rivers sf object.")
  
  site      <- sf::st_transform(site, 4326)
  out_rowid <- site[[id_col]][1]
  acc_crs   <- sf::st_crs(flow_acc_raster)
  
  # ---------------------------------------------------------------------------
  # 1. Crop rasters to buffered bbox around site
  # ---------------------------------------------------------------------------
  site_buf_vect <- site %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(search_buffer_m) %>%
    sf::st_transform(acc_crs) %>%
    terra::vect()
  
  acc_local <- terra::crop(flow_acc_raster, site_buf_vect)
  fdr_local <- terra::crop(flow_dir_raster, site_buf_vect)
  acc_col   <- names(acc_local)[1]
  
  message(sprintf("  [%s] Local raster: %d x %d cells | snap_method = '%s'",
                  out_rowid, terra::nrow(fdr_local), terra::ncol(fdr_local), snap_method))
  
  # ---------------------------------------------------------------------------
  # 2. Helper — get cell index from an sf point in the local acc raster
  # ---------------------------------------------------------------------------
  get_cell_from_xy <- function(pt_4326) {
    pt_proj <- sf::st_transform(pt_4326, acc_crs)
    terra::cellFromXY(acc_local, terra::crds(terra::vect(pt_proj)))
  }
  
  # ---------------------------------------------------------------------------
  # 3. Determine pour point cell based on snap_method
  # ---------------------------------------------------------------------------
  if (snap_method == "point") {
    # Exact cell the raw site coordinate falls in
    best_cell <- get_cell_from_xy(site)
    snap_note <- "exact point cell"
    
  } else if (snap_method == "radius") {
    # Highest accumulation cell within snap_radius_cells of the point
    seed_cell <- get_cell_from_xy(site)
    rc        <- terra::rowColFromCell(acc_local, seed_cell)
    row_range <- (rc[1] - snap_radius_cells):(rc[1] + snap_radius_cells)
    col_range <- (rc[2] - snap_radius_cells):(rc[2] + snap_radius_cells)
    row_range <- row_range[row_range >= 1 & row_range <= terra::nrow(acc_local)]
    col_range <- col_range[col_range >= 1 & col_range <= terra::ncol(acc_local)]
    
    window_cells <- terra::cellFromRowCol(
      acc_local,
      rep(row_range, each  = length(col_range)),
      rep(col_range, times = length(row_range))
    )
    
    window_vals <- terra::extract(acc_local, window_cells)
    best_cell   <- window_cells[which.max(window_vals[[acc_col]])]
    snap_note   <- sprintf("highest acc within %d cell radius", snap_radius_cells)
    
  } else if (snap_method == "river") {
    # Snap to nearest HydroRIVERS line geometry, then use exactly
    # the raster cell that the snapped point falls in
    rivers_wgs    <- sf::st_transform(rivers, 4326)
    nearest_idx   <- sf::st_nearest_feature(site, rivers_wgs)
    nearest_river <- rivers_wgs[nearest_idx, ] %>%
      sf::st_make_valid() %>%
      sf::st_cast("LINESTRING", warn = FALSE) %>%
      dplyr::slice(1)
    
    snap_geom  <- sf::st_nearest_points(site, nearest_river)
    snap_pts   <- sf::st_cast(snap_geom, "POINT")
    river_snap <- sf::st_sfc(snap_pts[[2]], crs = 4326) %>% sf::st_as_sf()
    
    river_snap_dist_m <- as.numeric(sf::st_distance(
      sf::st_transform(site,       3857),
      sf::st_transform(river_snap, 3857)
    ))
    
    message(sprintf("  [%s] Snapped to nearest river: %.1f m away",
                    out_rowid, river_snap_dist_m))
    
    best_cell <- get_cell_from_xy(river_snap)
    
    if (is.na(best_cell)) {
      message(sprintf(
        "  [%s] River snap point outside raster extent — falling back to exact point cell.",
        out_rowid
      ))
      best_cell <- get_cell_from_xy(site)
      snap_note <- "river snap fallback to point cell"
    } else {
      snap_note <- sprintf("river snap cell (%.1f m from site)", river_snap_dist_m)
    }
  }
  
  if (is.na(best_cell))
    stop(sprintf("[%s] Pour point cell is NA — site may be outside raster extent.", out_rowid))
  
  snap_acc_val <- terra::extract(acc_local, best_cell)[[acc_col]][1]
  snap_xy      <- terra::xyFromCell(acc_local, best_cell)
  
  snapped_point <- sf::st_point(snap_xy) %>%
    sf::st_sfc(crs = acc_crs) %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326)
  
  snap_dist_m <- as.numeric(sf::st_distance(
    sf::st_transform(site,          3857),
    sf::st_transform(snapped_point, 3857)
  ))
  
  message(sprintf("  [%s] Pour point: acc = %.0f | snap dist = %.1f m | %s",
                  out_rowid, snap_acc_val, snap_dist_m, snap_note))
  
  # ---------------------------------------------------------------------------
  # 4. Load cropped flow direction matrix into memory
  # ---------------------------------------------------------------------------
  fdr_matrix <- terra::as.matrix(fdr_local, wide = TRUE)
  nrows      <- nrow(fdr_matrix)
  ncols      <- ncol(fdr_matrix)
  
  # HydroSHEDS D8 encoding — opposite direction lookup:
  # a neighbour drains INTO the current cell if its flow dir equals this value
  drains_into <- c(
    "1"   = 16L,
    "2"   = 32L,
    "4"   = 64L,
    "8"   = 128L,
    "16"  = 1L,
    "32"  = 2L,
    "64"  = 4L,
    "128" = 8L
  )
  
  neighbour_offsets <- list(
    "1"   = c( 0L,  1L),
    "2"   = c( 1L,  1L),
    "4"   = c( 1L,  0L),
    "8"   = c( 1L, -1L),
    "16"  = c( 0L, -1L),
    "32"  = c(-1L, -1L),
    "64"  = c(-1L,  0L),
    "128" = c(-1L,  1L)
  )
  
  # ---------------------------------------------------------------------------
  # 5. BFS upstream traversal within the local matrix
  # ---------------------------------------------------------------------------
  visited <- matrix(FALSE, nrow = nrows, ncol = ncols)
  pour_rc <- terra::rowColFromCell(fdr_local, best_cell)
  visited[pour_rc[1], pour_rc[2]] <- TRUE
  
  frontier    <- list(c(pour_rc[1], pour_rc[2]))
  upstream_rc <- list(c(pour_rc[1], pour_rc[2]))
  
  repeat {
    new_frontier <- list()
    
    for (cur in frontier) {
      r <- cur[1]
      c <- cur[2]
      
      for (dir_name in names(neighbour_offsets)) {
        off <- neighbour_offsets[[dir_name]]
        nr  <- r + off[1]
        nc  <- c + off[2]
        
        if (nr < 1 || nr > nrows || nc < 1 || nc > ncols) next
        if (visited[nr, nc]) next
        
        nbr_fdr <- fdr_matrix[nr, nc]
        if (is.na(nbr_fdr)) next
        
        if (nbr_fdr == drains_into[dir_name]) {
          visited[nr, nc] <- TRUE
          new_frontier    <- c(new_frontier, list(c(nr, nc)))
          upstream_rc     <- c(upstream_rc,  list(c(nr, nc)))
        }
      }
    }
    
    if (length(new_frontier) == 0) break
    frontier <- new_frontier
  }
  
  message(sprintf("  [%s] Upstream cells: %d", out_rowid, length(upstream_rc)))
  
  # ---------------------------------------------------------------------------
  # 6. Vectorize upstream cells to polygon
  # ---------------------------------------------------------------------------
  rc_mat         <- do.call(rbind, upstream_rc)
  upstream_cells <- terra::cellFromRowCol(fdr_local, rc_mat[, 1], rc_mat[, 2])
  
  ws_rast       <- terra::rast(fdr_local)
  ws_rast[]     <- NA
  ws_rast[upstream_cells] <- 1L
  
  ws_poly <- tryCatch({
    terra::as.polygons(ws_rast) %>%
      sf::st_as_sf() %>%
      sf::st_set_crs(acc_crs) %>%
      sf::st_transform(4326) %>%
      sf::st_make_valid() %>%
      sf::st_union() %>%
      sf::st_as_sf()
  }, error = function(e) {
    message(sprintf("  [%s] Vectorization failed: %s", out_rowid, conditionMessage(e)))
    NULL
  })
  
  if (is.null(ws_poly) || nrow(ws_poly) == 0)
    stop(sprintf("[%s] Could not produce a watershed polygon.", out_rowid))
  
  names(ws_poly)[1]        <- "geometry"
  sf::st_geometry(ws_poly) <- "geometry"
  
  ws_refined <- ws_poly %>%
    dplyr::mutate(
      "{id_col}"   := out_rowid,
      Latitude     = site$Latitude,
      Longitude    = site$Longitude,
      watershed    = "HydroSHEDS flow direction BFS",
      split_method = sprintf("bfs_%s", snap_method),
      snap_dist_m  = snap_dist_m,
      snap_acc_val = snap_acc_val,
      snap_note    = snap_note
    )
  
  if (!identical(sf::st_crs(ws_refined), sf::st_crs(site)))
    ws_refined <- sf::st_transform(ws_refined, sf::st_crs(site))
  
  saveRDS(ws_refined, file.path(file_path, paste0(out_rowid, ".RDS")))
  
  invisible(list(
    watershed_refined = ws_refined,
    snapped_point     = snapped_point,
    snap_dist_m       = snap_dist_m,
    snap_acc_val      = snap_acc_val,
    snap_note         = snap_note
  ))
  
  mapview(ws_refined) + site
  
}