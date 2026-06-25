sf_use_s2(FALSE)
review_watersheds_global <- function(watersheds,
                                     sites,
                                     basins = NULL,
                                     rivers = NULL,
                                     site_buffer_m = 2500,
                                     id_col = "rowid",
                                     review_col = "review") {
  
  stopifnot(inherits(watersheds, "sf"))
  stopifnot(inherits(sites, "sf"))
  stopifnot(id_col %in% names(watersheds))
  stopifnot(id_col %in% names(sites))
  
  if (!is.null(basins)) {
    stopifnot(inherits(basins, "sf"))
    stopifnot(id_col %in% names(basins))
  }
  
  if (!is.null(rivers)) {
    stopifnot(inherits(rivers, "sf"))
    stopifnot(id_col %in% names(rivers))
  }
  
  if (!review_col %in% names(watersheds)) {
    watersheds[[review_col]] <- NA_character_
  }
  
  remaining <- which(is.na(watersheds[[review_col]]))
  
  if (length(remaining) == 0) {
    message("All international watersheds have been reviewed!")
    return(watersheds)
  }
  
  message(sprintf("Starting review. %d of %d international watersheds remaining.\n",
                  length(remaining), nrow(watersheds)))
  message("Workflow:")
  message("  1. Point-scale map: watershed + site + rowid-matched HydroBASINS / HydroRIVERS layers")
  message("  2. Watershed-scale map: full upstream watershed")
  message("Commands: y = Yes | s = Small | b = Big | m = Maybe/manual | q = Quit & save\n")
  
  # ---------------------------------------------------------------------------
  # Pre-project all layers once to WGS84 before the loop
  # ---------------------------------------------------------------------------
  watersheds_wgs <- sf::st_transform(watersheds, 4326)
  sites_wgs      <- sf::st_transform(sites, 4326)
  basins_wgs     <- if (!is.null(basins)) sf::st_transform(basins, 4326) else NULL
  rivers_wgs     <- if (!is.null(rivers)) sf::st_transform(rivers, 4326) else NULL
  
  for (i in remaining) {
    
    watershed <- watersheds_wgs[i, ]
    site_id   <- watershed[[id_col]][1]
    
    site <- sites_wgs %>%
      dplyr::filter(.data[[id_col]] == site_id)
    
    # -------------------------------------------------------------------------
    # Basins and rivers are already subset globally, but still need to be matched
    # to the current watershed/site by rowid.
    # -------------------------------------------------------------------------
    local_basin <- NULL
    if (!is.null(basins_wgs)) {
      local_basin <- basins_wgs %>%
        dplyr::filter(.data[[id_col]] == site_id)
      
      if (nrow(local_basin) == 0) {
        local_basin <- NULL
      }
    }
    
    nearby_rivers <- NULL
    if (!is.null(rivers_wgs)) {
      nearby_rivers <- rivers_wgs %>%
        dplyr::filter(.data[[id_col]] == site_id)
      
      if (nrow(nearby_rivers) == 0) {
        nearby_rivers <- NULL
      }
    }
    
    # -------------------------------------------------------------------------
    # Get a readable lat/lon from the site geometry if available
    # -------------------------------------------------------------------------
    if (nrow(site) > 0) {
      site_coords <- sf::st_coordinates(sf::st_geometry(site))[1, ]
      site_lon <- round(site_coords[["X"]], 6)
      site_lat <- round(site_coords[["Y"]], 6)
    } else {
      site_lon <- NA_real_
      site_lat <- NA_real_
    }
    
    # -------------------------------------------------------------------------
    # Bounding boxes
    # -------------------------------------------------------------------------
    if (!is.null(local_basin) && nrow(local_basin) > 0) {
      bb1 <- sf::st_bbox(local_basin)
    } else if (nrow(site) > 0) {
      bb1 <- site %>%
        sf::st_transform(3857) %>%
        sf::st_buffer(site_buffer_m) %>%
        sf::st_transform(4326) %>%
        sf::st_bbox()
    } else {
      bb1 <- sf::st_bbox(watershed)
    }
    
    bb2 <- sf::st_bbox(watershed)
    
    # -------------------------------------------------------------------------
    # Map 1: basin-centered / site-centered view
    # -------------------------------------------------------------------------
    m1 <- mapview::mapview(
      watershed,
      alpha.regions = 0.2,
      color = "orange",
      layer.name = "Full upstream watershed"
    )
    
    # if (!is.null(local_basin) && nrow(local_basin) > 0) {
    #   m1 <- m1 + mapview::mapview(
    #     local_basin,
    #     alpha.regions = 0.15,
    #     color = "purple",
    #     layer.name = "rowid-matched HydroBASINS"
    #   )
    # }
    
    if (!is.null(nearby_rivers) && nrow(nearby_rivers) > 0) {
      m1 <- m1 + mapview::mapview(
        nearby_rivers,
        color = "steelblue",
        lwd = 3,
        layer.name = "rowid-matched HydroRIVERS"
      )
    }
    
    if (nrow(site) > 0) {
      m1 <- m1 + mapview::mapview(
        site,
        col.regions = "red",
        cex = 8,
        layer.name = "Sample location"
      )
    }
    
    m1@map <- m1@map %>%
      leaflet::fitBounds(
        lng1 = bb1[["xmin"]], lat1 = bb1[["ymin"]],
        lng2 = bb1[["xmax"]], lat2 = bb1[["ymax"]]
      )
    
    print(m1)
    
    message(sprintf("[Map 1/2] [%d / %d] %s: %s | lat/lon: %s, %s",
                    match(i, remaining), length(remaining),
                    id_col, site_id,
                    site_lat, site_lon))
    
    if (!is.null(local_basin) && nrow(local_basin) > 0) {
      message(sprintf("HydroBASINS polygons shown: %d", nrow(local_basin)))
    }
    
    if (!is.null(nearby_rivers) && nrow(nearby_rivers) > 0) {
      message(sprintf("HydroRIVERS segments shown: %d", nrow(nearby_rivers)))
    }
    
    continue_key <- "INVALID"
    while (!continue_key %in% c("", "y", "s", "b", "m", "q")) {
      continue_key <- tolower(trimws(readline(
        "Press Enter to continue, or enter y/s/b/m/q now: "
      )))
    }
    
    if (continue_key == "q") {
      message("Quitting. Progress saved.")
      break
    }
    
    if (continue_key %in% c("y", "s", "b", "m")) {
      watersheds[[review_col]][i] <- toupper(continue_key)
      message(sprintf("  Saved from Map 1: %s\n", toupper(continue_key)))
      next
    }
    
    # -------------------------------------------------------------------------
    # Map 2: watershed-centered view
    # -------------------------------------------------------------------------
    m2 <- mapview::mapview(
      watershed,
      alpha.regions = 0.2,
      color = "orange",
      layer.name = "Full upstream watershed"
    )
    
    if (!is.null(local_basin) && nrow(local_basin) > 0) {
      m2 <- m2 + mapview::mapview(
        local_basin,
        alpha.regions = 0.15,
        color = "purple",
        layer.name = "rowid-matched HydroBASINS"
      )
    }
    
    if (!is.null(nearby_rivers) && nrow(nearby_rivers) > 0) {
      m2 <- m2 + mapview::mapview(
        nearby_rivers,
        color = "steelblue",
        lwd = 3,
        layer.name = "rowid-matched HydroRIVERS"
      )
    }
    
    if (nrow(site) > 0) {
      m2 <- m2 + mapview::mapview(
        site,
        col.regions = "red",
        cex = 8,
        layer.name = "Sample location"
      )
    }
    
    m2@map <- m2@map %>%
      leaflet::fitBounds(
        lng1 = bb2[["xmin"]], lat1 = bb2[["ymin"]],
        lng2 = bb2[["xmax"]], lat2 = bb2[["ymax"]]
      )
    
    print(m2)
    
    message(sprintf("[Map 2/2] [%d / %d] %s: %s",
                    match(i, remaining), length(remaining),
                    id_col, site_id))
    
    response <- ""
    while (!response %in% c("y", "s", "b", "m", "q")) {
      response <- tolower(trimws(readline("Accept? [y/s/b/m/q]: ")))
    }
    
    if (response == "q") {
      message("Quitting. Progress saved.")
      break
    }
    
    watersheds[[review_col]][i] <- toupper(response)
    message(sprintf("  Saved: %s\n", toupper(response)))
  }
  
  reviewed  <- sum(!is.na(watersheds[[review_col]]))
  remaining <- sum(is.na(watersheds[[review_col]]))
  message(sprintf("Done. %d reviewed, %d remaining.", reviewed, remaining))
  
  watersheds
}

sites <- intl_unique %>%
  select(rowid)

watersheds <- list.files("data/intl_watersheds_refined/", full.names = TRUE) %>%
  purrr::map_dfr(~readRDS(.x) %>%
                   dplyr::mutate(rowid = as.integer(tools::file_path_sans_ext(basename(.x)))) %>%
                   dplyr::select(rowid) %>%
                   nngeo::st_remove_holes())

basins_review <- basins %>%
  .[intl_unique,] %>%
  st_join(., sites, left = FALSE) %>%
  select(HYBAS_ID, rowid)

rivers_review <- hydro_rivers_review %>%
  .[sites %>% 
      sf::st_transform(3857) %>%
      sf::st_buffer(200000) %>%
      sf::st_transform(4326),] %>%
  st_join(., sites %>% 
            sf::st_transform(3857) %>%
            sf::st_buffer(200000) %>%
            sf::st_transform(4326), left = FALSE) %>%
  select(HYRIV_ID, rowid)

reviewed_watersheds <- review_watersheds_global(watersheds = watersheds,
                                                sites = sites,
                                                basins = basins_review,
                                                rivers = rivers_review,
                                                site_buffer_m = 2500,
                                                id_col = "rowid",
                                                review_col = "review")

saveRDS(reviewed_watersheds, "data/reviewed_watersheds.RDS")

# troubleshooting "bad" delineations:

bad <- reviewed_watersheds %>%
  filter(review != "Y")

review_watersheds_global(watersheds = watersheds %>% filter(rowid %in% bad$rowid),
                         sites = sites %>% filter(rowid %in% bad$rowid),
                         basins = basins_review,
                         rivers = rivers_review,
                         site_buffer_m = 2500,
                         id_col = "rowid",
                         review_col = "review")

# ---------------------------------------------------------------------------
# wrong flowline selected: 41
# ---------------------------------------------------------------------------
r41_flowline <- select_flowline_global(
  watersheds    =  watersheds %>% filter(rowid == 41),
  sites         = intl_unique %>% filter(rowid == 41),
  rivers        = hydro_rivers_review,
  river_buffer_m = 10000,
  id_col        = "rowid"
)

watersheds <- delineate_ws(
  sites               = intl_unique %>% filter(rowid == 41),
  flowline_selections = r41_flowline,
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

mapview(readRDS("data/intl_watersheds_refined/41.RDS")) + intl_unique %>% filter(rowid == 41)

# ---------------------------------------------------------------------------
#  too small: rowid 13, 19, 21, 33
# ---------------------------------------------------------------------------
delineate_neighborhood <- function(site,
                                   flow_acc_raster,
                                   flow_dir_raster,
                                   search_buffer_m = 500000,
                                   id_col          = "rowid") {
  
  stopifnot(inherits(site,            "sf"))
  stopifnot(inherits(flow_acc_raster, "SpatRaster"))
  stopifnot(inherits(flow_dir_raster, "SpatRaster"))
  stopifnot(id_col %in% names(site))
  
  site      <- sf::st_transform(site, 4326)
  out_rowid <- site[[id_col]][1]
  acc_crs   <- sf::st_crs(flow_acc_raster)
  
  message(sprintf("[%s] Delineating 9-cell neighborhood watersheds", out_rowid))
  
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
  
  message(sprintf("  Local raster: %d x %d cells",
                  terra::nrow(fdr_local), terra::ncol(fdr_local)))
  
  # ---------------------------------------------------------------------------
  # 2. Find the cell the site falls in, then get all 9 neighborhood cells
  #    (the site cell + its 8 immediate neighbours)
  # ---------------------------------------------------------------------------
  site_proj  <- sf::st_transform(site, acc_crs)
  site_vect  <- terra::vect(site_proj)
  center_cell <- terra::cellFromXY(acc_local, terra::crds(site_vect))
  
  if (is.na(center_cell))
    stop(sprintf("[%s] Site falls outside raster extent.", out_rowid))
  
  center_rc <- terra::rowColFromCell(acc_local, center_cell)
  cr        <- center_rc[1]
  cc        <- center_rc[2]
  nrows     <- terra::nrow(acc_local)
  ncols     <- terra::ncol(acc_local)
  
  # All 9 cells: center + 8 neighbours in row/col offsets
  offsets <- expand.grid(dr = -1L:1L, dc = -1L:1L)
  
  neighborhood <- purrr::pmap_dfr(offsets, function(dr, dc) {
    nr <- cr + dr
    nc <- cc + dc
    if (nr < 1 || nr > nrows || nc < 1 || nc > ncols) return(NULL)
    cell  <- terra::cellFromRowCol(acc_local, nr, nc)
    xy    <- terra::xyFromCell(acc_local, cell)
    acc   <- terra::extract(acc_local, cell)[[acc_col]][1]
    tibble::tibble(
      cell_id  = paste0("r", dr + 2L, "c", dc + 2L),  # label: r1c1 to r3c3
      row      = nr,
      col      = nc,
      cell_idx = cell,
      x        = xy[1],
      y        = xy[2],
      acc_val  = acc,
      dr       = dr,
      dc       = dc
    )
  })
  
  message(sprintf("  Neighborhood cells: %d", nrow(neighborhood)))
  message(sprintf("  Accumulation values range: %.0f to %.0f",
                  min(neighborhood$acc_val, na.rm = TRUE),
                  max(neighborhood$acc_val, na.rm = TRUE)))
  
  # ---------------------------------------------------------------------------
  # 3. Pre-load flow direction matrix once for BFS reuse
  # ---------------------------------------------------------------------------
  fdr_matrix <- terra::as.matrix(fdr_local, wide = TRUE)
  nrows_fdr  <- nrow(fdr_matrix)
  ncols_fdr  <- ncol(fdr_matrix)
  
  drains_into <- c(
    "1"   = 16L, "2"   = 32L, "4"   = 64L,  "8"   = 128L,
    "16"  = 1L,  "32"  = 2L,  "64"  = 4L,   "128" = 8L
  )
  
  neighbour_offsets <- list(
    "1"   = c( 0L,  1L), "2"   = c( 1L,  1L),
    "4"   = c( 1L,  0L), "8"   = c( 1L, -1L),
    "16"  = c( 0L, -1L), "32"  = c(-1L, -1L),
    "64"  = c(-1L,  0L), "128" = c(-1L,  1L)
  )
  
  # ---------------------------------------------------------------------------
  # 4. BFS helper — delineates upstream from a single pour point cell
  # ---------------------------------------------------------------------------
  bfs_upstream <- function(pour_row, pour_col) {
    
    visited      <- matrix(FALSE, nrow = nrows_fdr, ncol = ncols_fdr)
    visited[pour_row, pour_col] <- TRUE
    
    max_cells <- nrows_fdr * ncols_fdr
    up_r      <- integer(max_cells)
    up_c      <- integer(max_cells)
    up_r[1]   <- pour_row
    up_c[1]   <- pour_col
    cell_ptr  <- 1L
    frontier  <- matrix(c(pour_row, pour_col), nrow = 1)
    
    repeat {
      new_r <- integer(0)
      new_c <- integer(0)
      
      for (fi in seq_len(nrow(frontier))) {
        r <- frontier[fi, 1]
        c <- frontier[fi, 2]
        
        for (dir_name in names(neighbour_offsets)) {
          off <- neighbour_offsets[[dir_name]]
          nr  <- r + off[1]
          nc  <- c + off[2]
          if (nr < 1 || nr > nrows_fdr || nc < 1 || nc > ncols_fdr) next
          if (visited[nr, nc]) next
          nbr_fdr <- fdr_matrix[nr, nc]
          if (is.na(nbr_fdr)) next
          if (nbr_fdr == drains_into[dir_name]) {
            visited[nr, nc] <- TRUE
            new_r <- c(new_r, nr)
            new_c <- c(new_c, nc)
          }
        }
      }
      
      if (length(new_r) == 0) break
      
      n_new    <- length(new_r)
      up_r[(cell_ptr + 1L):(cell_ptr + n_new)] <- new_r
      up_c[(cell_ptr + 1L):(cell_ptr + n_new)] <- new_c
      cell_ptr <- cell_ptr + n_new
      frontier <- matrix(c(new_r, new_c), ncol = 2)
    }
    
    list(
      up_r     = up_r[seq_len(cell_ptr)],
      up_c     = up_c[seq_len(cell_ptr)],
      n_cells  = cell_ptr
    )
  }
  
  # ---------------------------------------------------------------------------
  # 5. Vectorize BFS result to polygon
  # ---------------------------------------------------------------------------
  cells_to_polygon <- function(up_r, up_c) {
    cells <- terra::cellFromRowCol(fdr_local, up_r, up_c)
    ws_rast       <- terra::rast(fdr_local)
    ws_rast[]     <- NA
    ws_rast[cells] <- 1L
    
    tryCatch({
      terra::as.polygons(ws_rast) %>%
        sf::st_as_sf() %>%
        sf::st_set_crs(acc_crs) %>%
        sf::st_transform(4326) %>%
        sf::st_make_valid() %>%
        sf::st_union() %>%
        sf::st_as_sf() %>%
        dplyr::rename(geometry = 1) %>%
        sf::st_set_geometry("geometry")
    }, error = function(e) {
      message(sprintf("    Vectorization failed: %s", conditionMessage(e)))
      NULL
    })
  }
  
  # ---------------------------------------------------------------------------
  # 6. Delineate watershed for each of the 9 neighborhood cells
  # ---------------------------------------------------------------------------
  
  ws_list <- vector("list", nrow(neighborhood))
  
  for (i in seq_len(nrow(neighborhood))) {
    
    cell_info <- neighborhood[i, ]
    
    message(sprintf("  [%d/9] Cell %s | row=%d col=%d | acc=%.0f",
                    i, cell_info$cell_id,
                    cell_info$row, cell_info$col,
                    cell_info$acc_val))
    
    bfs_result <- bfs_upstream(cell_info$row, cell_info$col)
    
    message(sprintf("    Upstream cells: %d", bfs_result$n_cells))
    
    ws_poly <- cells_to_polygon(bfs_result$up_r, bfs_result$up_c)
    
    if (is.null(ws_poly)) next
    
    ws_poly <- ws_poly %>%
      dplyr::mutate(
        "{id_col}"    := out_rowid,
        cell_id       = cell_info$cell_id,
        cell_row      = cell_info$row,
        cell_col      = cell_info$col,
        pour_x        = cell_info$x,
        pour_y        = cell_info$y,
        acc_val       = cell_info$acc_val,
        dr            = cell_info$dr,
        dc            = cell_info$dc,
        is_center     = (cell_info$dr == 0 & cell_info$dc == 0),
        area_km2      = as.numeric(sf::st_area(.)) / 1e6
      )
    
    ws_list[[i]] <- ws_poly
    
    message(sprintf("    Area: %.1f km²", ws_poly$area_km2[1]))
  }
  
  # ---------------------------------------------------------------------------
  # 7. Bind all 9 watersheds and save
  # ---------------------------------------------------------------------------
  out <- dplyr::bind_rows(Filter(Negate(is.null), ws_list))
  
  return(out)
  
 }

hyd_glo_acc_15s <- terra::rast("data/hyd_glo_acc_15s/hyd_glo_acc_15s.tif")

hyd_glo_dir_15s <- terra::rast("data/hyd_glo_dir_15s/hyd_glo_dir_15s.tif")

r13 <- delineate_neighborhood(site = intl_unique %>% dplyr::filter(rowid == 13),
  flow_acc_raster = hyd_glo_acc_15s,
  flow_dir_raster = hyd_glo_dir_15s,
  search_buffer_m = 500000,
  id_col          = "rowid") %>%
  rowid_to_column("index")

m <- mapview::mapview(intl_unique %>% dplyr::filter(rowid == 13),
                      col.regions = "red",
                      cex         = 8,
                      layer.name  = "Site")

for (i in seq_len(nrow(r13))) {
  row  <- r13[i, ]
  name <- sprintf("Cell %s | acc=%.0f | area=%.0f km²",
                  row$cell_id, row$acc_val, row$area_km2)
  m <- m + mapview::mapview(row, layer.name = name)
}

m

r13 <- r13 %>%
  .[c(3, 5, 6, 8),] %>%
  summarize() %>%
  mutate(rowid = 13)

mapview(r13) + intl_unique %>% dplyr::filter(rowid == 13)

saveRDS(r13, "data/intl_watersheds_refined/13.RDS")


r19 <- delineate_neighborhood(site = intl_unique %>% dplyr::filter(rowid == 19),
                              flow_acc_raster = hyd_glo_acc_15s,
                              flow_dir_raster = hyd_glo_dir_15s,
                              search_buffer_m = 500000,
                              id_col          = "rowid") %>%
  rowid_to_column("index")

m <- mapview::mapview(intl_unique %>% dplyr::filter(rowid == 19),
                      col.regions = "red",
                      cex         = 8,
                      layer.name  = "Site")

for (i in seq_len(nrow(r19))) {
  row  <- r19[i, ]
  name <- sprintf("Cell %s | acc=%.0f | area=%.0f km²",
                  row$cell_id, row$acc_val, row$area_km2)
  m <- m + mapview::mapview(row, layer.name = name)
}

m

r19_ <- r19 %>%
  .[c(7,8),] %>%
  summarize() %>%
  mutate(rowid = 19)

mapview(r19_) + intl_unique %>% dplyr::filter(rowid == 19)

saveRDS(r19_, "data/intl_watersheds_refined/19.RDS")


r21 <- delineate_neighborhood(site = intl_unique %>% dplyr::filter(rowid == 21),
                              flow_acc_raster = hyd_glo_acc_15s,
                              flow_dir_raster = hyd_glo_dir_15s,
                              search_buffer_m = 500000,
                              id_col          = "rowid") %>%
  rowid_to_column("index")

m <- mapview::mapview(intl_unique %>% dplyr::filter(rowid == 21),
                      col.regions = "red",
                      cex         = 8,
                      layer.name  = "Site")

for (i in seq_len(nrow(r21))) {
  row  <- r21[i, ]
  name <- sprintf("Cell %s | acc=%.0f | area=%.0f km²",
                  row$cell_id, row$acc_val, row$area_km2)
  m <- m + mapview::mapview(row, layer.name = name)
}

m

r21_ <- r21 %>%
  .[c(8),] %>%
  summarize() %>%
  mutate(rowid = 21)

mapview(r21_) + intl_unique %>% dplyr::filter(rowid == 21)

saveRDS(r21_, "data/intl_watersheds_refined/21.RDS")


r33 <- delineate_neighborhood(site = intl_unique %>% dplyr::filter(rowid == 33),
                              flow_acc_raster = hyd_glo_acc_15s,
                              flow_dir_raster = hyd_glo_dir_15s,
                              search_buffer_m = 500000,
                              id_col          = "rowid") %>%
  rowid_to_column("index")

m <- mapview::mapview(intl_unique %>% dplyr::filter(rowid == 33),
                      col.regions = "red",
                      cex         = 8,
                      layer.name  = "Site")

for (i in seq_len(nrow(r33))) {
  row  <- r33[i, ]
  name <- sprintf("Cell %s | acc=%.0f | area=%.0f km²",
                  row$cell_id, row$acc_val, row$area_km2)
  m <- m + mapview::mapview(row, layer.name = name)
}

m

r33_ <- r33 %>%
  .[c(5),] %>%
  summarize() %>%
  mutate(rowid = 33)

mapview(r33_) + intl_unique %>% dplyr::filter(rowid == 33)

saveRDS(r33_, "data/intl_watersheds_refined/33.RDS")
# ---------------------------------------------------------------------------
#  needs additional flowlines delineated/review associated flowline: 30, 22, 27, 40, 45, 49, 7
# ---------------------------------------------------------------------------
wonky_sites_update_a <- select_flowline_global(watersheds = watersheds %>% filter(rowid %in% c(30, 22, 27, 40, 45, 49, 7)),
                                               sites = intl_unique,
                                               rivers = hydro_rivers_review,
                                               river_buffer_m = 10000,
                                               id_col = "rowid")

still_weird <- still_weird %>%
  filter(review != "Y") %>%
  pull(rowid)

still_weird


r30 <- readRDS("data/intl_watersheds_refined/30.RDS") %>%
  select(rowid)
r30_basins <- basins %>%
  .[r30,] %>%
  .[c(222),]
r30 <- r30 %>%
  bind_rows(r30_basins) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview(r30) +  mapview(r30_basins, col.region = "red") + intl_unique %>% filter(rowid == 30) + hydro_rivers_review

saveRDS(r30, "data/intl_watersheds_refined/30.RDS")

r22 <- readRDS("data/intl_watersheds_refined/22.RDS") %>%
  select(rowid)
r22_basins <- basins %>%
  .[r22,] %>%
  .[c(63, 64, 65, 70),]
r22 <- r22 %>%
  bind_rows(r22_basins) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview(r22) +  mapview(r22_basins, col.region = "red") + intl_unique %>% filter(rowid == 22) #hydro_rivers_review

saveRDS(r22, "data/intl_watersheds_refined/22.RDS")

r27 <- readRDS("data/intl_watersheds_refined/27.RDS") %>%
  select(rowid)

r27_basins <- basins %>%
  .[r27,] %>%
  .[c(12, 11),]

r27 <- r27 %>%
  bind_rows(r27_basins) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview(r27) +  mapview(r27_basins, col.region = "red") + intl_unique %>% filter(rowid == 27) + hydro_rivers_review
saveRDS(r27, "data/intl_watersheds_refined/27.RDS")

r40 <- readRDS("data/intl_watersheds_refined/40.RDS") %>%
  select(rowid)

r40_basins <- basins %>%
  .[r40,] %>%
  .[c(8),]

r40 <- r40 %>%
  bind_rows(r40_basins) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview(r40) +  mapview(r40_basins, col.region = "red") + intl_unique %>% filter(rowid == 40) + hydro_rivers_review

saveRDS(r40, "data/intl_watersheds_refined/40.RDS")

r45 <- readRDS("data/intl_watersheds_refined/45.RDS") %>%
  select(rowid)

r45_basins <- basins %>%
  .[r45,] %>%
  .[c(17),]

r45 <- r45 %>%
  bind_rows(r45_basins) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview(r45) +  mapview(r45_basins, col.region = "red") + intl_unique %>% filter(rowid == 45) + hydro_rivers_review

saveRDS(r45, "data/intl_watersheds_refined/45.RDS")

r49 <- readRDS("data/intl_watersheds_refined/49.RDS") %>%
  select(rowid)

r49_basins <- basins %>%
  .[r49,] %>%
  .[c(250, 254),]

r49 <- r49 %>%
  bind_rows(r49_basins) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview(r49) +  mapview(r49_basins, col.region = "red") + intl_unique %>% filter(rowid == 49) + hydro_rivers_review

saveRDS(r49, "data/intl_watersheds_refined/49.RDS")


r7 <- readRDS("data/intl_watersheds_refined/7.RDS") %>%
  select(rowid)

r7_basins <- basins %>%
  .[r7,] %>%
  .[c(14),]

r7 <- r7 %>%
  bind_rows(r7_basins) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview(r7) +  mapview(r7_basins, col.region = "red") + intl_unique %>% filter(rowid == 7) + hydro_rivers_review

saveRDS(r7, "data/intl_watersheds_refined/7.RDS")

# ---------------------------------------------------------------------------
#  some downstream bits still showing up: 23
# ---------------------------------------------------------------------------
r23_flowline <- select_flowline_global(watersheds = watersheds %>% filter(rowid ==23),
                                       sites = intl_unique,
                                       rivers = hydro_rivers_review,
                                       river_buffer_m = 10000,
                                       id_col = "rowid")

delineate_ws(sites = intl_unique %>% filter(rowid == 23),
             flowline_selections = r23_flowline,
             flowlines_network = hydro_rivers,
             basins = basins,
             flow_acc_raster = hyd_glo_acc_30s,
             flow_dir_raster = hyd_glo_dir_30s,
             flowline_id_col = "HYRIV_ID",
             downstream_col = "NEXT_DOWN",
             snap_method = "point",
             local_basin_pct = 0.80,
             id_col = "rowid",
             hybas_id_col = "HYBAS_ID",
             file_path = "data/intl_watersheds_refined/",
             use_s2 = FALSE)

mapview(readRDS("data/intl_watersheds_refined/23.RDS")) + intl_unique %>% filter(rowid == 23)


# ---------------------------------------------------------------------------
# huge rivers -- estuarine hydro approach?: 43, 44, 
# ---------------------------------------------------------------------------

big_basin_1 <- basins %>%
  .[intl_unique %>% 
      filter(rowid == 43) %>% 
      sf::st_transform(3857) %>%
      sf::st_buffer(200000) %>%
      sf::st_transform(4326),]
mapview(big_basin_1) + intl_unique %>% filter(rowid == 43)

basins_crs <- sf::st_crs(big_basin_1)

# Work in meters for buffering / sliver handling
site_m <- intl_unique %>% 
  filter(rowid == 43) %>%
  sf::st_transform(3857)

basins_m <- big_basin_1 %>%
  sf::st_transform(3857) %>%
  sf::st_make_valid()

# Local search area around the point
search_area_m <- site_m %>%
  sf::st_buffer(80000) %>%
  sf::st_make_valid()

# Only keep basins near the point
local_basins_m <- basins_m[search_area_m, ]

# Make one dissolved polygon of all local basins
local_basin_union_m <- local_basins_m %>%
  sf::st_union() %>%
  sf::st_make_valid()

# The "empty spaces" are everything inside the search window
# that is not covered by HydroBASINS polygons.
gaps_m <- sf::st_difference(sf::st_geometry(search_area_m),
                            local_basin_union_m) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()


site_xy <- sf::st_coordinates(site_m)[1, ]

x0 <- site_xy[["X"]]
y0 <- site_xy[["Y"]]

# Make this longer than your search buffer if needed
clip_dist_m <- 200000

# Wider wedge to keep adjacent basins near the point
wedge_half_angle_deg <- 70

# SW direction
center_angle_deg <- 225

angle1 <- (center_angle_deg - wedge_half_angle_deg) * pi / 180
angle2 <- (center_angle_deg + wedge_half_angle_deg) * pi / 180

sw_wedge_coords <- matrix(
  c(
    x0, y0,
    x0 + clip_dist_m * cos(angle1), y0 + clip_dist_m * sin(angle1),
    x0 + clip_dist_m * cos(angle2), y0 + clip_dist_m * sin(angle2),
    x0, y0
  ),
  ncol = 2,
  byrow = TRUE
)

sw_wedge_m <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_polygon(list(sw_wedge_coords)),
    crs = sf::st_crs(site_m)
  )
)

site_keep_m <- site_m %>%
  sf::st_buffer(8000)

sw_clip_m <- rbind(
  sw_wedge_m,
  site_keep_m %>% dplyr::select(geometry)
) %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

gaps_sw_m <- sf::st_intersection(
  gaps_m,
  sw_clip_m
) %>%
  sf::st_make_valid() %>%
  st_cast("POLYGON") %>%
  .[-c(13:16),] 

mapview(gaps_sw_m, col.region = "red") + site_m + big_basin_1

names(gaps_sw_m)[names(gaps_sw_m) == "x"] <- "geometry"
st_geometry(gaps_sw_m) <- "geometry"

touching_basins_m <- big_basin_1[gaps_sw_m %>% st_buffer(100) %>% st_transform(4326),]

mapview(touching_basins_m, col.region = "red") + mapview(gaps_sw_m, col.region = "blue") + site_m

touching_basins = touching_basins_m %>%
  sf::st_transform(basins_crs)

touching_flowlines <- hydro_rivers %>%
  .[touching_basins,]

# 2. Trace upstream HydroRIVERS flowlines
get_upstream_flowline_ids <- function(target_flowline_ids,
                                      rivers,
                                      id_col = "HYRIV_ID",
                                      down_col = "NEXT_DOWN") {
  
  river_links <- rivers %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      river_id = .data[[id_col]],
      downstream_id = .data[[down_col]]
    ) %>%
    dplyr::filter(!is.na(.data$river_id))
  
  upstream_ids <- unique(target_flowline_ids)
  
  repeat {
    
    next_ids <- river_links %>%
      dplyr::filter(.data$downstream_id %in% upstream_ids) %>%
      dplyr::pull(.data$river_id) %>%
      unique()
    
    next_ids <- setdiff(next_ids, upstream_ids)
    
    if (length(next_ids) == 0) {
      break
    }
    
    upstream_ids <- c(upstream_ids, next_ids)
  }
  
  unique(upstream_ids)
}

target_flowline_ids <- touching_flowlines %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(HYRIV_ID) %>%
  unique() %>%
  stats::na.omit()

upstream_flowline_ids_43 <- get_upstream_flowline_ids(
  target_flowline_ids = target_flowline_ids,
  rivers = hydro_rivers,
  id_col = "HYRIV_ID",
  down_col = "NEXT_DOWN"
)


upstream_flowlines_43 <- hydro_rivers %>%
  dplyr::filter(.data$HYRIV_ID %in% upstream_flowline_ids_43) %>%
  sf::st_make_valid()

mapview(upstream_flowlines_43)


# 3. Grab all HydroBASINS touched by upstream flowlines
upstream_flowlines_buffer_43 <- upstream_flowlines_43 %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

upstream_basins_43 <- basins %>%
  sf::st_make_valid() %>%
  .[upstream_flowlines_buffer_43,] %>%
  bind_rows(gaps_sw_m %>% st_transform(4326)) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview::mapview(upstream_basins_43, color = "orange", alpha.regions = 0.25) +
  mapview::mapview(upstream_flowlines_43, color = "blue", lwd = 2) +
  mapview::mapview(intl_unique %>% dplyr::filter(rowid == 43),
                   col.regions = "black", cex = 8)

saveRDS(upstream_basins_43 %>% mutate(rowid = 43), "data/intl_watersheds_refined/43.RDS")


# NEXT, 44, 

mapview(big_basin_1) + intl_unique %>% filter(rowid %in% c(44, 43))

basins_crs <- sf::st_crs(big_basin_1)


# Work in meters for buffering / sliver handling
site_m <- intl_unique %>% 
  filter(rowid == 44) %>%
  sf::st_transform(3857)

basins_m <- big_basin_1 %>%
  sf::st_transform(3857) %>%
  sf::st_make_valid()

# Local search area around the point
search_area_m <- site_m %>%
  sf::st_buffer(80000) %>%
  sf::st_make_valid()

# Only keep basins near the point
local_basins_m <- basins_m[search_area_m, ]

# Make one dissolved polygon of all local basins
local_basin_union_m <- local_basins_m %>%
  sf::st_union() %>%
  sf::st_make_valid()

# The "empty spaces" are everything inside the search window
# that is not covered by HydroBASINS polygons.
gaps_m <- sf::st_difference(sf::st_geometry(search_area_m),
                            local_basin_union_m) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()


site_xy <- sf::st_coordinates(site_m)[1, ]

x0 <- site_xy[["X"]]
y0 <- site_xy[["Y"]]

# Make this longer than your search buffer if needed
clip_dist_m <- 200000

# Wider wedge to keep adjacent basins near the point
wedge_half_angle_deg <- 70

# SW direction
center_angle_deg <- 292

angle1 <- (center_angle_deg - wedge_half_angle_deg) * pi / 180
angle2 <- (center_angle_deg + wedge_half_angle_deg) * pi / 180

sw_wedge_coords <- matrix(
  c(
    x0, y0,
    x0 + clip_dist_m * cos(angle1), y0 + clip_dist_m * sin(angle1),
    x0 + clip_dist_m * cos(angle2), y0 + clip_dist_m * sin(angle2),
    x0, y0
  ),
  ncol = 2,
  byrow = TRUE
)

sw_wedge_m <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_polygon(list(sw_wedge_coords)),
    crs = sf::st_crs(site_m)
  )
)

site_keep_m <- site_m %>%
  sf::st_buffer(9000)

sw_clip_m <- rbind(
  sw_wedge_m,
  site_keep_m %>% dplyr::select(geometry)
) %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

gaps_sw_m <- sf::st_intersection(
  gaps_m,
  sw_clip_m
) %>%
  sf::st_make_valid() %>%
  st_cast("POLYGON") %>%
  .[-c(1, 8:13),] 

mapview(gaps_sw_m, col.region = "red") + site_m + big_basin_1

names(gaps_sw_m)[names(gaps_sw_m) == "x"] <- "geometry"
st_geometry(gaps_sw_m) <- "geometry"

touching_basins_m <- big_basin_1[gaps_sw_m %>% st_buffer(100) %>% st_transform(4326),]

mapview(touching_basins_m, col.region = "red") + mapview(gaps_sw_m, col.region = "blue") + site_m

touching_basins = touching_basins_m %>%
  sf::st_transform(basins_crs)

touching_flowlines <- hydro_rivers %>%
  .[touching_basins,]

# 2. Trace upstream HydroRIVERS flowlines
get_upstream_flowline_ids <- function(target_flowline_ids,
                                      rivers,
                                      id_col = "HYRIV_ID",
                                      down_col = "NEXT_DOWN") {
  
  river_links <- rivers %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      river_id = .data[[id_col]],
      downstream_id = .data[[down_col]]
    ) %>%
    dplyr::filter(!is.na(.data$river_id))
  
  upstream_ids <- unique(target_flowline_ids)
  
  repeat {
    
    next_ids <- river_links %>%
      dplyr::filter(.data$downstream_id %in% upstream_ids) %>%
      dplyr::pull(.data$river_id) %>%
      unique()
    
    next_ids <- setdiff(next_ids, upstream_ids)
    
    if (length(next_ids) == 0) {
      break
    }
    
    upstream_ids <- c(upstream_ids, next_ids)
  }
  
  unique(upstream_ids)
}

target_flowline_ids <- touching_flowlines %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(HYRIV_ID) %>%
  unique() %>%
  stats::na.omit()

upstream_flowline_ids_44 <- get_upstream_flowline_ids(
  target_flowline_ids = target_flowline_ids,
  rivers = hydro_rivers,
  id_col = "HYRIV_ID",
  down_col = "NEXT_DOWN"
)


upstream_flowlines_44 <- hydro_rivers %>%
  dplyr::filter(.data$HYRIV_ID %in% upstream_flowline_ids_43) %>%
  sf::st_make_valid()

mapview(upstream_flowlines_44)


# 3. Grab all HydroBASINS touched by upstream flowlines
upstream_flowlines_buffer_44 <- upstream_flowlines_44 %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

upstream_basins_44 <- basins %>%
  sf::st_make_valid() %>%
  .[upstream_flowlines_buffer_44,] %>%
  bind_rows(gaps_sw_m %>% st_transform(4326)) %>%
  summarize() %>%
  nngeo::st_remove_holes()

mapview::mapview(upstream_basins_44, color = "orange", alpha.regions = 0.25) +
  mapview::mapview(upstream_flowlines_44, color = "blue", lwd = 2) +
  mapview::mapview(intl_unique %>% dplyr::filter(rowid == 43),
                   col.regions = "black", cex = 8)

saveRDS(upstream_basins_44 %>% mutate(rowid = 44), "data/intl_watersheds_refined/44.RDS")


# ---------------------------------------------------------------------------
# "holes" need filled: 47
# ---------------------------------------------------------------------------
r47 <- readRDS("data/intl_watersheds_refined/47.RDS") %>%
  sf::st_transform(3857) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(1000) %>%
  nngeo::st_remove_holes() %>%
  sf::st_buffer(-1000) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

saveRDS(r47, "data/intl_watersheds_refined/47.RDS")

mapview(r47) + intl_unique %>% filter(rowid == 47)

# ---------------------------------------------------------------------------
# needs to be run with flow dir and flow acc: 15, 34
# --------------------------------------------------------------------------- 
delineate_ws(sites = intl_unique %>% filter(rowid %in% c(15, 34)),
             flowline_selections = flowline_selections,
             flowlines_network = hydro_rivers,
             basins = basins,
             flow_acc_raster = hyd_glo_acc_30s,
             flow_dir_raster = hyd_glo_dir_30s,
             flowline_id_col = "HYRIV_ID",
             downstream_col = "NEXT_DOWN",
             snap_method = "point",
             local_basin_pct = 0.80,
             id_col = "rowid",
             hybas_id_col = "HYBAS_ID",
             file_path = "data/intl_watersheds_refined/",
             use_s2 = FALSE)

mapview(readRDS("data/intl_watersheds_refined/15.RDS")) + intl_unique %>% filter(rowid == 15)
mapview(readRDS("data/intl_watersheds_refined/34.RDS")) + intl_unique %>% filter(rowid == 34)

# ---------------------------------------------------------------------------
# yojoa: 35, 36, 37, 38, 39
# --------------------------------------------------------------------------- 
yojoa_basins <- basins %>%
  .[intl_unique %>% 
      filter(rowid %in% 35:39) %>% 
      sf::st_transform(3857) %>%
      sf::st_buffer(2000) %>%
      sf::st_transform(4326),] %>%
  summarize()

yojoa_basins_by_rowid <- sf::st_sf(
  rowid = 35:39,
  geometry = sf::st_geometry(yojoa_basins)[rep(1, length(35:39))]
)

output_dir <- "data/intl_watersheds_refined"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

yojoa_basins_by_rowid %>%
  split(.$rowid) %>%
  purrr::iwalk(~{
    saveRDS(
      .x,
      file = file.path(output_dir, paste0(.y, ".rds"))
    )
  })
