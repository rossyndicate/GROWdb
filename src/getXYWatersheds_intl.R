# Watershed delineation for international (non-CONUS) sampling sites.
# Uses HydroBASINS (HydroSHEDS) pre-delineated level-12 basins instead of
# nhdplusTools/NLDI (CONUS-only) or DEM-based tools.
#
# Required packages: sf, dplyr, tibble
#
# Arguments:
#   sf        - single-row sf point (must have columns rowid, Latitude, Longitude)
#   coordinates - c(longitude, latitude) if not providing sf
#   crs       - CRS for coordinates if not providing sf
#   basins    - HydroBASINS level-12 sf object, pre-loaded for relevant continents;
#               must have columns HYBAS_ID, NEXT_DOWN, SUB_AREA, UP_AREA, PFAF_ID
#   adj       - named list pre-built from basins via build_hydrobasins_adj();
#               pass this so it is computed once, not per site
#   file_path - output directory for per-site RDS files
#
# HydroBASINS gives full upstream polygons. Level 12 basins average ~250 km²,
# so the most-local sub-basin is included whole (no split-catchment at the exact
# pour-point location, unlike the CONUS NHD raindrop approach).
#
# Download HydroBASINS level 12 from https://www.hydrosheds.org/products/hydrobasins
# Load all required continental files and rbind them before calling this function.


# Pre-compute the upstream-child adjacency list from the full basins dataset.
# Call once after loading basins; pass the result to every getXYWatersheds_intl() call.
build_hydrobasins_adj <- function(basins) {
  ids = basins$HYBAS_ID
  next_down = basins$NEXT_DOWN
  adj = split(ids, next_down)    # adj[[as.character(X)]] = all basins whose NEXT_DOWN == X
  adj[["0"]] = NULL              # 0 means "drains to ocean" — not a real parent
  adj
}


getXYWatersheds_intl <- function(sf = NULL,
                                  coordinates = NULL,
                                  crs = NULL,
                                  basins,
                                  adj,
                                  file_path = "data/intl_watersheds/") {

  if (is.null(sf) && is.null(coordinates)) stop("Provide `sf` or `coordinates`.")

  if (is.null(sf)) {
    aoi_raw <- tibble::tibble(long = coordinates[1], lat = coordinates[2]) %>%
      sf::st_as_sf(coords = c("long", "lat"), crs = crs)
  } else {
    aoi_raw <- sf
  }

  if (sf::st_crs(aoi_raw)$epsg != 4326) aoi <- sf::st_transform(aoi_raw, 4326) else aoi <- aoi_raw

  out_rowid <- aoi$rowid[1]

  # Find the level-12 basin containing the pour point.
  # Fall back to nearest basin if the point falls in a gap (coast, island edge, etc.)
  site_basin <- sf::st_join(aoi, basins, join = sf::st_within, left = FALSE)

  if (nrow(site_basin) == 0) {
    nearest_idx <- sf::st_nearest_feature(aoi, basins)
    site_basin  <- basins[nearest_idx, ]
  }

  start_id <- site_basin$HYBAS_ID[1]

  # BFS upstream traversal using the pre-built adjacency list.
  # Each step looks up which basins drain into the current frontier set.
  all_ids  <- start_id
  frontier <- as.character(start_id)

  repeat {
    children <- unlist(adj[frontier], use.names = FALSE)
    if (is.null(children) || length(children) == 0) break
    new_ids <- setdiff(children, all_ids)
    if (length(new_ids) == 0) break
    all_ids  <- c(all_ids, new_ids)
    frontier <- as.character(new_ids)
  }

  # Union all upstream level-12 basin polygons
  ws_poly <- basins %>%
    dplyr::filter(HYBAS_ID %in% all_ids) %>%
    sf::st_make_valid() %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_make_valid()

  names(ws_poly)[1] <- "geometry"
  sf::st_geometry(ws_poly) <- "geometry"

  ws_out <- ws_poly %>%
    dplyr::mutate(
      HYBAS_ID  = start_id,
      rowid     = out_rowid,
      Latitude  = aoi$Latitude,
      Longitude = aoi$Longitude,
      watershed = "HydroBASINS L12"
    ) %>%
    dplyr::select(HYBAS_ID, rowid, Latitude, Longitude, watershed)

  if (!identical(sf::st_crs(ws_out), sf::st_crs(aoi_raw))) {
    ws_out <- sf::st_transform(ws_out, sf::st_crs(aoi_raw))
  }

  saveRDS(ws_out, file.path(file_path, paste0(out_rowid, ".RDS")))
}
