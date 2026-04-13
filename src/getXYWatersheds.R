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
  
  if (!snap) {
    
    flowline_danger_zone <- get_nhdplus(AOI = aoi, realization = "flowline") %>%
      sf::st_buffer(30)
    
    near_flowline_100 <- any(lengths(sf::st_intersects(sf::st_buffer(aoi, 100), flowline_danger_zone)) > 0)
    near_flowline_35  <- any(lengths(sf::st_intersects(sf::st_buffer(aoi, 35),  flowline_danger_zone)) > 0)
    
    if (!near_flowline_100) {
      
      if (near_flowline_35) {
        
        site_buffer <- aoi %>%
          sf::st_buffer(35)
        
        boundary_points <- site_buffer %>%
          sf::st_boundary() %>%
          sf::st_cast("POINT") %>%
          dplyr::mutate(point_id = dplyr::row_number()) %>%
          sf::st_difference(flowline_danger_zone) %>%
          dplyr::filter(point_id %% 50 == 0) %>%
          dplyr::bind_rows(aoi)
        
        splits <- purrr::map(
          seq_len(nrow(boundary_points)),
          \(i) {
            get_split_catchment(point = sf::st_as_sfc(boundary_points[i, ])) %>%
              dplyr::filter(is.na(catchmentID)) %>%
              sf::st_make_valid() %>%
              dplyr::mutate(area = as.numeric(sf::st_area(.)))
          }
        ) %>%
          dplyr::bind_rows() %>%
          dplyr::filter(area == max(area, na.rm = TRUE)) %>%
          dplyr::select(-dplyr::any_of(c("catchmentID", "id")))
        
        if (sf::st_crs(splits) != sf::st_crs(aoi_raw)) {
          splits <- sf::st_transform(splits, sf::st_crs(aoi_raw))
        }
        
        return(splits)
        
      } else {
        
        mini_ws <- get_split_catchment(point = sf::st_as_sfc(aoi)) %>%
          dplyr::filter(is.na(catchmentID)) %>%
          sf::st_make_valid() %>%
          dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
          dplyr::select(-dplyr::any_of(c("catchmentID", "id")))
        
        if (sf::st_crs(mini_ws) != sf::st_crs(aoi_raw)) {
          mini_ws <- sf::st_transform(mini_ws, sf::st_crs(aoi_raw))
        }
        
        return(mini_ws)
      }
      
    } else {
      
      flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326)
      
      nearest_points <- sf::st_nearest_points(aoi, flowline)
      snapped_points_sf <- sf::st_cast(nearest_points, "POINT")[2, ]
      
      trace <- get_raindrop_trace(snapped_points_sf, direction = "down")
      
      raindrop <- sf::st_sfc(
        sf::st_point(trace$intersection_point[[1]][1:2]),
        crs = 4326
      )
      
      nhd_catch <- get_split_catchment(raindrop, upstream = TRUE)[2, ] %>%
        sf::st_make_valid() %>%
        dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
        dplyr::select(area) %>%
        nngeo::st_remove_holes()
      
      if (sf::st_crs(nhd_catch) != sf::st_crs(aoi_raw)) {
        nhd_catch <- sf::st_transform(nhd_catch, sf::st_crs(aoi_raw))
      }
      
      saveRDS(nhd_catch, paste0("data/grow_watersheds/", sf$sample_name, ".RDS"))
    }
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
    
    saveRDS(nhd_catch, paste0("data/grow_watersheds/", sf$sample_name, ".RDS"))
  }
}