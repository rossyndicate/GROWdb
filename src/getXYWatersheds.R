getXYWatersheds <- function(sf = NULL, coordinates = NULL, crs = NULL, snap = FALSE) {
  
  if (is.null(sf) && is.null(coordinates)) {
    stop("Provide either `sf` or `coordinates`.")
  }
  
  if (is.null(sf)) {
    df <- tibble::tibble(
      long = coordinates[1],
      lat  = coordinates[2]
    )
    
    aoi_raw <- sf::st_as_sf(df, coords = c("long", "lat"), crs = crs)
  } else {
    aoi_raw <- sf
  }
  
  if (sf::st_crs(aoi_raw)$epsg != 4326) {
    aoi <- sf::st_transform(aoi_raw, 4326)
  } else {
    aoi <- aoi_raw
  }
  
  if (snap == FALSE) {
    flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326)
    # Use EPSG:5070 (CONUS Albers, metre-based) as the single working CRS.
    # get_nhdplus and get_split_catchment both expect/return 4326, so we
    # reproject their outputs immediately after each call.
    working_crs <- 5070
    aoi <- st_transform(aoi, working_crs)
    
      # get_nhdplus returns 4326 — reproject to working CRS right away
      flowline_danger_zone <- get_nhdplus(AOI = st_transform(aoi, 4326),
                                          realization = "flowline") %>%
        st_transform(working_crs) %>%
        st_buffer(30)   # 30 m buffer, valid because we're in a metre CRS
      
      # Always use the boundary-point ensemble approach — build a 35-m buffer,
      # sample its boundary while avoiding the flowline corridor, then select
      # the largest split catchment from all candidate points.
      site_buffer <- aoi %>% st_buffer(35)
      
      boundary_points <- site_buffer %>%
        st_boundary() %>%
        st_cast("POINT") %>%
        mutate(point_id = row_number()) %>%
        st_difference(., flowline_danger_zone) %>%   # exclude flowline corridor
        filter(point_id %% 50 == 0) %>%              # thin to every 50th point
        bind_rows(aoi)                                # always include the original click
      
      # get_split_catchment expects 4326; reproject each point before calling,
      # then immediately bring the result back to working_crs
      splits <- vector("list", nrow(boundary_points))
      for (i in seq_len(nrow(boundary_points))) {
        pt_4326 <- st_transform(boundary_points[i, ], 4326) %>% st_as_sfc()
        splits[[i]] <- tryCatch(
          get_split_catchment(point = pt_4326) %>%
            st_transform(working_crs) %>%
            filter(is.na(catchmentID)) %>%
            st_make_valid() %>%
            mutate(area = as.numeric(st_area(.))),
          error = function(e) NULL
        )
      }
      
      splits <- bind_rows(Filter(Negate(is.null), splits))
      
      if (nrow(splits) == 0) stop("No valid catchment splits found near this location.")
      
      watershed_sf <- splits %>%
        filter(area == max(area)) %>%
        slice(1) %>%
        dplyr::mutate(area = as.numeric(sf::st_area(.)),
                      comid = flowline$comid,
                      sample_name = aoi$sample_name) %>%
        dplyr::select(comid, sample_name, area)  %>%
        nngeo::st_remove_holes() %>%
        st_make_valid() %>%
        st_buffer(0)
      
      # Project back
      nhd_catch <- st_transform(watershed_sf, st_crs(aoi_raw))
      
    }

  
  if (snap) {
    flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326)
    
    nearest_points <- sf::st_nearest_points(aoi, flowline)
    snapped_points_sf <- sf::st_cast(nearest_points, "POINT")[2, ]
    
    trace <- get_raindrop_trace(snapped_points_sf, direction = "down")
    
    raindrop <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]),
      crs = 4326)
    
    nhd_catch <- get_split_catchment(raindrop, upstream = TRUE)[2, ] %>%
      sf::st_make_valid() %>%
      dplyr::mutate(area = as.numeric(sf::st_area(.)),
                    comid = flowline$comid,
                    sample_name = sf$sample_name) %>%
      dplyr::select(comid, sample_name, area)  %>%
      nngeo::st_remove_holes()
    
    if (sf::st_crs(nhd_catch) != sf::st_crs(aoi_raw)) {
      nhd_catch <- sf::st_transform(nhd_catch, sf::st_crs(aoi_raw))
    }
    
  }
  
  saveRDS(nhd_catch, paste0("data/grow_watersheds/", aoi$sample_name, ".RDS"))
}

