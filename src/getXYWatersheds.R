getXYWatersheds <- function(sf = NULL, coordinates = NULL, crs = NULL, snap = FALSE, file_path = "data/grow_watersheds/") {
  
  if (is.null(sf) && is.null(coordinates)) {
    stop("Provide either `sf` or `coordinates`.")
  }
  
  if (is.null(sf)) {
    df <- tibble::tibble(
      long = coordinates[1],
      lat = coordinates[2]
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
  
  small_watersheds <- tibble(Latitude = c(44.49060, 38.92329, 44.21339, 38.98269, 32.42735, 33.33772, 40.78319, 40.77970, 33.33747, 43.95472, 38.89520, 41.86957,
                                          43.95472, 38.87497, 37.05229, 40.78464, 40.78010, 42.52339, 38.89440, 37.05229, 41.86957, 38.92329, 44.21339),
                             Longitude = c(-72.16220, -106.94239, -122.24398, -107.00515, -110.75784, -81.71816, -111.80126, -111.80640, -81.71821, -71.72278, -78.14760, -73.00222,
                                           -71.72278,  -76.54649, -119.19536, -111.79547, -111.80540,  -71.18545, -78.14740, -119.19536, -73.00222, -106.94239, -122.24398))
  
  if (paste(round(aoi$Latitude, 4), round(aoi$Longitude, 4)) %in% paste(round(small_watersheds$Latitude, 4), round(small_watersheds$Longitude, 4))){
    
    snap <- FALSE
    
  }
  
  wrong_comids <- tibble(Latitude = c(41.86958, 30.75852, 41.01528, 46.73222, 36.95497, 43.68620, 31.85310, 37.21647),
                         Longitude = c(-72.79677, -91.39595, -96.15778, -117.18038, -119.02375, -121.68733, -88.15748, -89.46758),
                         comid = c(6109731, 19085457, 17416032, 23459939, 22050327, 23702001, 18548456, 5092616))
  
  if (paste(round(aoi$Latitude, 4), round(aoi$Longitude, 4)) %in% paste(round(wrong_comids$Latitude, 4), round(wrong_comids$Longitude, 4))){
    
    aoi_mod <- wrong_comids %>%
      filter(paste(round(Latitude, 4), round(Longitude, 4)) %in% paste(round(aoi$Latitude, 4), round(aoi$Longitude, 4)))
    
    
    nhd_catch <- get_nldi_basin(nldi_feature = list(featureSource = "comid", featureID = aoi_mod$comid)) %>% 
      st_make_valid() %>%
      mutate(comid = aoi_mod$comid,
             rowid = aoi$rowid,
             Latitude = aoi$Latitude,
             Longitude = aoi$Longitude,
             watershed = "nldi") %>%
      dplyr::select(comid, rowid, Latitude, Longitude, watershed) %>%
      nngeo::st_remove_holes()
    
  } else if(round(aoi$Latitude, 4) == round(32.3397, 4) & round(aoi$Longitude, 4) == round(-90.9125, 4)){
    
    flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326) 
    
    nhd_catch <- get_nldi_basin(list(featureSource = "comid", featureID = flowline$comid)) %>%
      .[[1]] %>%
      st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%  
      st_remove_holes() %>%
      sf::st_make_valid() %>%          
      dplyr::mutate(comid = flowline$comid,
                    rowid = aoi$rowid,
                    Latitude = aoi$Latitude,
                    Longitude = aoi$Longitude,
                    watershed = "nldi") %>%
      dplyr::select(comid, rowid, Latitude, Longitude, watershed)
    
    
  } else if (snap == FALSE) {
    
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
      st_buffer(30)  # 30 m buffer, valid because we're in a metre CRS
    
    if((round(aoi$Latitude, 4) == round(40.78319, 4) & round(aoi$Longitude, 4) == round(-111.80126, 4)) |
       (round(aoi$Latitude, 4) == round(37.05229, 4) & round(aoi$Longitude, 4) == round(-119.19536, 4)) |
       (round(aoi$Latitude, 4) == round(41.86957, 4) & round(aoi$Longitude, 4) == round(-73.00222, 4)) |
       (round(aoi$Latitude, 4) == round(38.92329, 4) & round(aoi$Longitude, 4) == round(-106.94239, 4)) |
       (round(aoi$Latitude, 4) == round(44.21339, 4) & round(aoi$Longitude, 4) == round(-122.24398, 4))){
      
      site_buffer <- aoi %>% st_buffer(80)
      
    } else {
      
      # Always use the boundary-point ensemble approach — build a 35-m buffer,
      # sample its boundary while avoiding the flowline corridor, then select
      # the largest split catchment from all candidate points.
      site_buffer <- aoi %>% st_buffer(35)
      
    }
    
    
    if((round(aoi$Latitude, 4) == round(38.89520, 4) & round(aoi$Longitude, 4) == round(-78.14760, 4)) |
       (round(aoi$Latitude, 4) == round(38.89440, 4) & round(aoi$Longitude, 4) == round(-78.14740, 4)) |
       (round(aoi$Latitude, 4) == round(40.78319, 4) & round(aoi$Longitude, 4) == round(-111.80126, 4))|
       (round(aoi$Latitude, 4) == round(37.05229, 4) & round(aoi$Longitude, 4) == round(-119.19536, 4)) |
       (round(aoi$Latitude, 4) == round(41.86957, 4) & round(aoi$Longitude, 4) == round(-73.00222, 4)) |
       (round(aoi$Latitude, 4) == round(38.92329, 4) & round(aoi$Longitude, 4) == round(-106.94239, 4)) |
       (round(aoi$Latitude, 4) == round(44.21339, 4) & round(aoi$Longitude, 4) == round(-122.24398, 4))){
      split_count <- 10
    } else {
      split_count <- 50
    }
    
    boundary_points <- site_buffer %>%
      st_boundary() %>%
      st_cast("POINT") %>%
      mutate(point_id = row_number()) %>%
      st_difference(., flowline_danger_zone) %>%  # exclude flowline corridor
      filter(point_id %% split_count == 0) %>%       # thin to every 50th point
      bind_rows(aoi)                # always include the original click
    
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
    
    nhd_catch <- splits %>%
      filter(area == max(area)) %>%
      slice(1) %>%
      dplyr::mutate(comid = NA,
                    rowid = aoi$rowid,
                    Latitude = aoi$Latitude,
                    Longitude = aoi$Longitude,
                    watershed = "micro-nhd") %>%
      dplyr::select(comid, rowid, Latitude, Longitude, watershed) %>%
      nngeo::st_remove_holes() %>%
      st_make_valid() %>%
      st_buffer(0)
    
  }else if (snap == TRUE) {
    
    flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326)
    
    if((round(aoi$Latitude, 4) == round(38.87486, 4) & round(aoi$Longitude, 4) == round(-76.54654, 4))){
      
      # explode all multilinestring to linestring
      lines_df <- st_cast(flowline, "LINESTRING")
      
      # convert line tips to coordinates
      endpoints_list <- map_dfr(1:nrow(lines_df), function(i) {
        coords <- st_coordinates(lines_df[i, ])
        
        # start point (first coordinate)
        start_pt <- st_point(c(coords[1, "X"], coords[1, "Y"]))
        
        # end point (last coordinate) 
        end_pt <- st_point(c(coords[nrow(coords), "X"], coords[nrow(coords), "Y"]))
        
        data.frame(point_type = c("start", "end"),
                   geometry = st_sfc(start_pt, end_pt, crs = st_crs(flowline)))
      })
      
      # make the coordinates into points
      snapped_points_sf <- st_as_sf(endpoints_list) %>%
        filter(point_type == "end") %>%
        st_as_sfc()
      
      
    } else {
      
      
      nearest_points <- sf::st_nearest_points(aoi, flowline)
      snapped_points_sf <- sf::st_cast(nearest_points, "POINT")[2, ]
      
    }
    
    
    trace <- get_raindrop_trace(snapped_points_sf, direction = "down")
    
    raindrop <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]),
                           crs = 4326)
    
    
    
    error_watersheds <- tibble(Latitude = c(30.69074, 30.44567, 29.69282, 38.70981, 32.44417, 32.71866, 29.85715, 38.89440),
                               Longitude = c(-91.73623,  -91.19156, -91.21194,  -91.43850,  -90.91417, -114.71884,  -89.97785,  -78.14740))
    
    
    if (paste(round(aoi$Latitude, 4), round(aoi$Longitude, 4)) %in% paste(round(error_watersheds$Latitude, 4), round(error_watersheds$Longitude, 4))){
      
      
      remove <- get_nhdplus(AOI = aoi, realization = "catchment")
      
      
      
      swap <- get_split_catchment(raindrop, upstream = FALSE)[2, ] %>%
        sf::st_make_valid() %>%
        nngeo::st_remove_holes()
      
      nhd_catch <- get_nldi_basin(nldi_feature = list(featureSource = "comid", featureID = flowline$comid)) %>% 
        st_make_valid() %>%
        nngeo::st_remove_holes() %>%
        st_difference(remove) %>%
        st_make_valid() %>%
        st_cast("POLYGON") %>%
        mutate(area = st_area(.)) %>%
        slice_max(area, n = 1) %>%
        st_make_valid() %>%
        bind_rows(swap) %>%
        summarize() %>%
        nngeo::st_remove_holes() %>%
        st_make_valid() %>%
        mutate(comid = flowline$comid,
               rowid = aoi$rowid,
               Latitude = aoi$Latitude,
               Longitude = aoi$Longitude,
               watershed = "nldi + swapped lower catchment") %>%
        dplyr::select(comid, rowid, Latitude, Longitude, watershed) 
      
      
    } else {
      
      
      nhd_catch <- get_split_catchment(raindrop, upstream = TRUE)[2, ] %>%
        sf::st_make_valid() %>%
        dplyr::mutate(comid = flowline$comid,
                      rowid = aoi$rowid,
                      Latitude = aoi$Latitude,
                      Longitude = aoi$Longitude,
                      watershed = "split catchment") %>%
        dplyr::select(comid, rowid, Latitude, Longitude, watershed) %>%
        nngeo::st_remove_holes()
    }
    
  }
  
  if (sf::st_crs(nhd_catch) != sf::st_crs(aoi_raw)) {
    nhd_catch <- sf::st_transform(nhd_catch, sf::st_crs(aoi_raw))
  }
  
  saveRDS(nhd_catch, paste0(file_path, "/", aoi$rowid, ".RDS"))
  
}

