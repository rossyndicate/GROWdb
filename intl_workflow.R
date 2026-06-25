delineate_ws <- function(sites,
                         flowline_selections,
                         flowlines_network,
                         basins,
                         flow_acc_raster,
                         flow_dir_raster,
                         flowline_id_col      = "HYRIV_ID",
                         downstream_col       = "NEXT_DOWN",
                         snap_method          = c("point", "radius"),
                         snap_radius_cells    = 5,
                         local_basin_pct      = 0.80,
                         id_col               = "rowid",
                         hybas_id_col         = "HYBAS_ID",
                         file_path            = "data/watersheds/",
                         use_s2               = FALSE) {
  
  stopifnot(inherits(sites,               "sf"))
  stopifnot(inherits(flowline_selections, "sf"))
  stopifnot(inherits(flowlines_network,   "sf"))
  stopifnot(inherits(basins,              "sf"))
  stopifnot(inherits(flow_acc_raster,     "SpatRaster"))
  stopifnot(inherits(flow_dir_raster,     "SpatRaster"))
  stopifnot(id_col          %in% names(sites))
  stopifnot(id_col          %in% names(flowline_selections))
  stopifnot(flowline_id_col %in% names(flowline_selections))
  stopifnot(flowline_id_col %in% names(flowlines_network))
  stopifnot(downstream_col  %in% names(flowlines_network))
  stopifnot(hybas_id_col    %in% names(basins))
  
  snap_method <- match.arg(snap_method)
  acc_crs     <- sf::st_crs(flow_acc_raster)
  
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(use_s2)
  
  sites_wgs  <- sf::st_transform(sites,               4326)
  basins_wgs <- sf::st_transform(basins,              4326) %>% 
    sf::st_make_valid()
  fl_sel_wgs <- sf::st_transform(flowline_selections, 4326) %>% 
    sf::st_make_valid()
  fl_net_wgs <- sf::st_transform(flowlines_network,   4326) %>% 
    sf::st_make_valid()
  
  # ---------------------------------------------------------------------------
  # Pre-build flowline lookup objects once
  # ---------------------------------------------------------------------------
  message("Building flowline network adjacency ...")
  
  fl_ids  <- as.character(fl_net_wgs[[flowline_id_col]])
  fl_down <- as.character(fl_net_wgs[[downstream_col]])
  
  fl_idx_lookup <- stats::setNames(seq_along(fl_ids), fl_ids)
  
  adj_fl_idx <- split(seq_along(fl_ids), fl_down)
  adj_fl_idx[["0"]]  <- NULL
  adj_fl_idx[["NA"]] <- NULL
  
  message(sprintf("  Network: %d flowlines indexed.", length(fl_ids)))
  
  # ---------------------------------------------------------------------------
  # Pre-split selected flowlines by site ID.
  # This avoids filtering all selected flowlines once per site.
  # ---------------------------------------------------------------------------
  message("Indexing selected flowlines by site ...")
  
  fl_sel_by_site <- split(
    fl_sel_wgs,
    as.character(fl_sel_wgs[[id_col]])
  )
  
  # Prime basin spatial index once
  message("Priming basin spatial index ...")
  invisible(sf::st_intersects(basins_wgs, basins_wgs[1, ]))
  
  dir.create(file_path, showWarnings = FALSE, recursive = TRUE)
  results <- vector("list", nrow(sites_wgs))
  
  # Static lookup objects for raster BFS
  drains_into <- c(
    "1"   = 16L, "2"   = 32L, "4"   = 64L,  "8"   = 128L,
    "16"  = 1L,  "32"  = 2L,  "64"  = 4L,   "128" = 8L
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
  
  dir_names <- names(neighbour_offsets)
  
  for (idx in seq_len(nrow(sites_wgs))) {
    
    site       <- sites_wgs[idx, ]
    out_rowid  <- site[[id_col]][1]
    out_rowid_chr <- as.character(out_rowid)
    
    message(sprintf(
      "\n[%d / %d] Processing %s: %s",
      idx, nrow(sites_wgs), id_col, out_rowid_chr
    ))
    
    # -------------------------------------------------------------------------
    # 1. Get selected flowline for this site
    # -------------------------------------------------------------------------
    site_flowline <- fl_sel_by_site[[out_rowid_chr]]
    
    if (is.null(site_flowline) || nrow(site_flowline) == 0) {
      warning(sprintf("  [%s] No flowline selection — skipping.", out_rowid_chr))
      next
    }
    
    site_flowline <- site_flowline %>%
      sf::st_cast("LINESTRING", warn = FALSE) %>%
      dplyr::slice(1)
    
    start_fl_id  <- as.character(site_flowline[[flowline_id_col]][1])
    start_fl_idx <- fl_idx_lookup[[start_fl_id]]
    
    if (is.null(start_fl_idx) || is.na(start_fl_idx)) {
      warning(sprintf(
        "  [%s] Selected flowline not found in network — skipping.",
        out_rowid_chr
      ))
      next
    }
    
    message(sprintf("  Pour point flowline: %s", start_fl_id))
    
    # -------------------------------------------------------------------------
    # 2. BFS upstream through integer flowline indices
    # -------------------------------------------------------------------------
    n_fl       <- length(fl_ids)
    visited_fl <- logical(n_fl)
    visited_fl[start_fl_idx] <- TRUE
    
    upstream_idx    <- integer(n_fl)
    upstream_idx[1] <- start_fl_idx
    up_ptr          <- 1L
    frontier_idx    <- start_fl_idx
    
    repeat {
      children_idx <- unlist(
        adj_fl_idx[fl_ids[frontier_idx]],
        use.names = FALSE
      )
      
      if (length(children_idx) == 0) break
      
      new_idx <- children_idx[!visited_fl[children_idx]]
      
      if (length(new_idx) == 0) break
      
      visited_fl[new_idx] <- TRUE
      
      upstream_idx[(up_ptr + 1L):(up_ptr + length(new_idx))] <- new_idx
      up_ptr       <- up_ptr + length(new_idx)
      frontier_idx <- new_idx
    }
    
    upstream_idx       <- upstream_idx[seq_len(up_ptr)]
    upstream_flowlines <- fl_net_wgs[upstream_idx, ]
    upstream_ids       <- fl_ids[upstream_idx]
    
    message(sprintf("  Upstream flowlines: %d", up_ptr))
    
    # -------------------------------------------------------------------------
    # 3. Get basins intersecting confirmed upstream flowlines
    # -------------------------------------------------------------------------
    fl_bbox <- sf::st_bbox(upstream_flowlines) %>%
      sf::st_as_sfc()
    
    bbox_hits <- which(lengths(sf::st_intersects(basins_wgs, fl_bbox)) > 0)
    
    if (length(bbox_hits) == 0) {
      warning(sprintf("  [%s] No basin bbox hits — skipping.", out_rowid_chr))
      next
    }
    
    basin_cands <- basins_wgs[bbox_hits, ]
    
    basin_x_upstream <- sf::st_intersects(
      basin_cands,
      upstream_flowlines
    )
    
    keep_basin <- lengths(basin_x_upstream) > 0
    ambiguous  <- which(keep_basin)
    
    # Keep original confirmation logic, but use vectorized membership instead
    # of an environment and nested vapply calls.
    if (length(ambiguous) > 0) {
      basin_x_network <- sf::st_intersects(
        basin_cands[ambiguous, ],
        fl_net_wgs
      )
      
      confirmed <- vapply(seq_along(ambiguous), function(k) {
        net_hits <- basin_x_network[[k]]
        if (length(net_hits) == 0) return(FALSE)
        
        hit_ids <- fl_ids[net_hits]
        any(hit_ids %in% upstream_ids)
      }, logical(1))
      
      keep_basin[ambiguous[!confirmed]] <- FALSE
    }
    
    upstream_basins <- basin_cands[keep_basin, ] %>%
      sf::st_make_valid()
    
    if (nrow(upstream_basins) == 0) {
      warning(sprintf("  [%s] No upstream basins confirmed — skipping.", out_rowid_chr))
      next
    }
    
    message(sprintf(
      "  Candidate basins (upstream confirmed): %d",
      nrow(upstream_basins)
    ))
    
    # -------------------------------------------------------------------------
    # 4. Identify bottommost basin using selected flowline overlap
    # -------------------------------------------------------------------------
    fl_geom      <- sf::st_geometry(site_flowline)
    fl_total_len <- as.numeric(sf::st_length(site_flowline))
    
    cand_idx <- which(
      lengths(sf::st_intersects(upstream_basins, site_flowline)) > 0
    )
    
    if (length(cand_idx) == 0) {
      cand_idx <- sf::st_nearest_feature(site_flowline, upstream_basins)
    }
    
    overlap_lengths <- vapply(cand_idx, function(j) {
      tryCatch({
        clipped <- sf::st_intersection(
          fl_geom,
          sf::st_geometry(upstream_basins[j, ])
        )
        
        if (length(clipped) == 0 || all(sf::st_is_empty(clipped))) {
          return(0)
        }
        
        sum(as.numeric(sf::st_length(clipped)), na.rm = TRUE)
      }, error = function(e) {
        0
      })
    }, numeric(1))
    
    best_cand_pos   <- which.max(overlap_lengths)
    local_basin_idx <- cand_idx[best_cand_pos]
    overlap_pct     <- overlap_lengths[best_cand_pos] / fl_total_len
    
    message(sprintf(
      "  Bottom basin flowline overlap: %.1f%%",
      overlap_pct * 100
    ))
    
    if (overlap_pct < local_basin_pct) {
      message(sprintf(
        "  Overlap < %.0f%% threshold — excluding bottom basin, using upstream basins only.",
        local_basin_pct * 100
      ))
      
      upstream_without_local <- upstream_basins[-cand_idx, ]
      local_basin            <- NULL
      start_id               <- as.character(
        upstream_basins[[hybas_id_col]][local_basin_idx]
      )
      do_flowdir_clip        <- FALSE
      
    } else {
      local_basin <- upstream_basins[local_basin_idx, ] %>%
        sf::st_make_valid()
      
      start_id <- as.character(local_basin[[hybas_id_col]][1])
      
      upstream_without_local <- upstream_basins[-cand_idx, ]
      do_flowdir_clip        <- TRUE
      
      message(sprintf("  Local basin %s: %s", hybas_id_col, start_id))
    }
    
    message(sprintf(
      "  Upstream basins excl. local: %d",
      nrow(upstream_without_local)
    ))
    
    # -------------------------------------------------------------------------
    # 5. Snap site to selected flowline for pour point
    # -------------------------------------------------------------------------
    snap_geom  <- sf::st_nearest_points(site, site_flowline)
    snap_pts   <- sf::st_cast(snap_geom, "POINT")
    pour_point <- sf::st_sfc(snap_pts[[2]], crs = 4326) %>%
      sf::st_as_sf()
    
    snap_dist_m <- as.numeric(sf::st_distance(
      sf::st_transform(site,       3857),
      sf::st_transform(pour_point, 3857)
    ))
    
    message(sprintf("  Snapped to flowline: %.1f m", snap_dist_m))
    
    # -------------------------------------------------------------------------
    # 6-8. Flow direction clip of local basin
    # -------------------------------------------------------------------------
    if (do_flowdir_clip) {
      
      local_basin_vect <- local_basin %>%
        sf::st_transform(3857) %>%
        sf::st_buffer(5000) %>%
        sf::st_transform(acc_crs) %>%
        terra::vect()
      
      acc_local <- terra::crop(flow_acc_raster, local_basin_vect)
      fdr_local <- terra::crop(flow_dir_raster, local_basin_vect)
      acc_col   <- names(acc_local)[1]
      
      message(sprintf(
        "  Local raster: %d x %d cells",
        terra::nrow(fdr_local),
        terra::ncol(fdr_local)
      ))
      
      pour_proj <- sf::st_transform(pour_point, acc_crs)
      best_cell <- terra::cellFromXY(
        acc_local,
        terra::crds(terra::vect(pour_proj))
      )
      
      if (snap_method == "radius" && !is.na(best_cell)) {
        rc <- terra::rowColFromCell(acc_local, best_cell)
        
        row_range <- (rc[1] - snap_radius_cells):(rc[1] + snap_radius_cells)
        col_range <- (rc[2] - snap_radius_cells):(rc[2] + snap_radius_cells)
        
        row_range <- row_range[
          row_range >= 1 & row_range <= terra::nrow(acc_local)
        ]
        
        col_range <- col_range[
          col_range >= 1 & col_range <= terra::ncol(acc_local)
        ]
        
        window_cells <- terra::cellFromRowCol(
          acc_local,
          rep(row_range, each  = length(col_range)),
          rep(col_range, times = length(row_range))
        )
        
        window_vals <- terra::extract(acc_local, window_cells)
        best_cell   <- window_cells[which.max(window_vals[[acc_col]])]
      }
      
      if (is.na(best_cell)) {
        warning(sprintf(
          "  [%s] Pour point outside raster — using full local basin.",
          out_rowid_chr
        ))
        
        local_upstream <- local_basin %>%
          sf::st_set_geometry("geometry") %>%
          dplyr::select(geometry)
        
        split_method <- "pour_point_outside_raster"
        
      } else {
        
        snap_acc_val <- terra::extract(acc_local, best_cell)[[acc_col]][1]
        message(sprintf("  Pour point acc = %.0f", snap_acc_val))
        
        fdr_masked <- terra::mask(
          fdr_local,
          local_basin %>%
            sf::st_transform(acc_crs) %>%
            terra::vect()
        )
        
        fdr_matrix <- terra::as.matrix(fdr_masked, wide = TRUE)
        nrows      <- nrow(fdr_matrix)
        ncols      <- ncol(fdr_matrix)
        
        visited <- matrix(FALSE, nrow = nrows, ncol = ncols)
        
        pour_rc <- terra::rowColFromCell(fdr_local, best_cell)
        visited[pour_rc[1], pour_rc[2]] <- TRUE
        
        max_cells <- nrows * ncols
        
        up_r    <- integer(max_cells)
        up_c    <- integer(max_cells)
        up_r[1] <- pour_rc[1]
        up_c[1] <- pour_rc[2]
        
        cell_ptr <- 1L
        frontier <- matrix(c(pour_rc[1], pour_rc[2]), nrow = 1)
        
        repeat {
          max_new <- nrow(frontier) * 8L
          new_r   <- integer(max_new)
          new_c   <- integer(max_new)
          new_ptr <- 0L
          
          for (fi in seq_len(nrow(frontier))) {
            r <- frontier[fi, 1]
            c <- frontier[fi, 2]
            
            for (dir_name in dir_names) {
              off <- neighbour_offsets[[dir_name]]
              nr  <- r + off[1]
              nc  <- c + off[2]
              
              if (nr < 1 || nr > nrows || nc < 1 || nc > ncols) next
              if (visited[nr, nc]) next
              
              nbr_fdr <- fdr_matrix[nr, nc]
              if (is.na(nbr_fdr)) next
              
              if (nbr_fdr == drains_into[dir_name]) {
                visited[nr, nc] <- TRUE
                
                new_ptr <- new_ptr + 1L
                new_r[new_ptr] <- nr
                new_c[new_ptr] <- nc
              }
            }
          }
          
          if (new_ptr == 0L) break
          
          new_r <- new_r[seq_len(new_ptr)]
          new_c <- new_c[seq_len(new_ptr)]
          
          n_new <- new_ptr
          
          up_r[(cell_ptr + 1L):(cell_ptr + n_new)] <- new_r
          up_c[(cell_ptr + 1L):(cell_ptr + n_new)] <- new_c
          
          cell_ptr <- cell_ptr + n_new
          frontier <- cbind(new_r, new_c)
        }
        
        up_r <- up_r[seq_len(cell_ptr)]
        up_c <- up_c[seq_len(cell_ptr)]
        
        message(sprintf("  Local basin upstream cells: %d", cell_ptr))
        
        upstream_cells <- terra::cellFromRowCol(fdr_local, up_r, up_c)
        
        ws_rast   <- terra::rast(fdr_local)
        ws_rast[] <- NA
        ws_rast[upstream_cells] <- 1L
        
        local_upstream <- tryCatch({
          terra::as.polygons(ws_rast, dissolve = TRUE) %>%
            sf::st_as_sf() %>%
            sf::st_set_crs(acc_crs) %>%
            sf::st_transform(4326) %>%
            sf::st_make_valid() %>%
            sf::st_union() %>%
            sf::st_as_sf() %>%
            dplyr::rename(geometry = 1) %>%
            sf::st_set_geometry("geometry")
        }, error = function(e) {
          message(sprintf(
            "  Vectorization failed: %s — using full local basin.",
            conditionMessage(e)
          ))
          
          local_basin %>%
            sf::st_set_geometry("geometry") %>%
            dplyr::select(geometry)
        })
        
        split_method <- "flowline_snap_flowacc_bfs"
      }
      
    } else {
      local_upstream <- NULL
      split_method   <- "bottom_basin_excluded_low_overlap"
    }
    
    # -------------------------------------------------------------------------
    # 9. Union components into final watershed
    # -------------------------------------------------------------------------
    components <- list()
    
    if (nrow(upstream_without_local) > 0) {
      components <- c(components, list(
        upstream_without_local %>%
          sf::st_set_geometry("geometry") %>%
          dplyr::select(geometry)
      ))
    }
    
    if (!is.null(local_upstream)) {
      components <- c(components, list(
        local_upstream %>%
          sf::st_set_geometry("geometry") %>%
          dplyr::select(geometry)
      ))
    }
    
    if (length(components) == 0) {
      warning(sprintf(
        "  [%s] No watershed components — skipping.",
        out_rowid_chr
      ))
      next
    }
    
    ws_components <- dplyr::bind_rows(components)
    
    ws_refined <- tryCatch({
      ws_components %>%
        dplyr::summarize(geometry = sf::st_union(geometry)) %>%
        sf::st_make_valid()
    }, error = function(e) {
      message(sprintf(
        "  Union failed before make_valid: %s — retrying with make_valid first.",
        conditionMessage(e)
      ))
      
      ws_components %>%
        sf::st_make_valid() %>%
        dplyr::summarize(geometry = sf::st_union(geometry)) %>%
        sf::st_make_valid()
    })
    
    ws_refined <- ws_refined %>%
      dplyr::mutate(
        "{id_col}"       := out_rowid,
        "{hybas_id_col}" := start_id,
        Latitude         = site$Latitude,
        Longitude        = site$Longitude,
        watershed        = "flowline network + flowdir BFS",
        split_method     = split_method,
        snap_dist_m      = snap_dist_m
      )
    
    if (!identical(sf::st_crs(ws_refined), sf::st_crs(sites))) {
      ws_refined <- sf::st_transform(ws_refined, sf::st_crs(sites))
    }
    
    saveRDS(
      ws_refined,
      file.path(file_path, paste0(out_rowid_chr, ".RDS"))
    )
    
    results[[idx]] <- ws_refined
    
    message(sprintf(
      "  Done. Area: %.1f km²",
      as.numeric(sf::st_area(ws_refined)) / 1e6
    ))
  }
  
  out <- dplyr::bind_rows(Filter(Negate(is.null), results))
  
  message(sprintf(
    "\nCompleted %d / %d watersheds.",
    nrow(out),
    nrow(sites_wgs)
  ))
  
  out
}


hyd_glo_acc_15s <- terra::rast(
  "data/hyd_glo_acc_15s/hyd_glo_acc_15s.tif"
)

hyd_glo_dir_15s <- terra::rast(
  "data/hyd_glo_dir_15s/hyd_glo_dir_15s.tif"
)

watersheds <- delineate_ws(
  sites               = intl_unique %>% filter(!rowid %in% c(to_redo_big$rowid, to_redo$rowid)),
  flowline_selections = flowline_selections,
  flowlines_network   = hydro_rivers,
  basins              = basins,
  flow_acc_raster     = hyd_glo_acc_15s,
  flow_dir_raster     = hyd_glo_dir_15s,
  flowline_id_col     = "HYRIV_ID",
  downstream_col      = "NEXT_DOWN",
  snap_method         = "point",
  local_basin_pct     = 0.80,
  id_col              = "rowid",
  hybas_id_col        = "HYBAS_ID",
  file_path = "data/intl_watersheds_refined/",
  use_s2 = FALSE
)