review_gage_pairs <- function(points,
                              gages,
                              lat_col = "Latitude",
                              lon_col = "Longitude",
                              point_site_col = "site_no",
                              gage_site_col = "gage",
                              buffer_m = 2500) {
  
  stopifnot(all(c(lat_col, lon_col, point_site_col) %in% names(points)))
  stopifnot(gage_site_col %in% names(gages))
  
  if (!"review_id" %in% names(points)) {
    points <- points %>% mutate(review_id = row_number())
  }
  
  if (!"review" %in% names(points)) {
    points$review <- NA_character_
  }
  
  if (!"updated_site_no" %in% names(points)) {
    points$updated_site_no <- NA_character_
  }
  
  points_sf <- points %>%
    st_as_sf(
      coords = c(lon_col, lat_col),
      crs = 4326,
      remove = FALSE
    )
  
  gages_sf <- if (inherits(gages, "sf")) gages else st_as_sf(gages)
  
  if (is.na(st_crs(gages_sf))) {
    st_crs(gages_sf) <- 4326
  }
  
  points_sf <- st_transform(points_sf, 4326)
  gages_sf  <- st_transform(gages_sf, 4326)
  
  review_rows <- which(is.na(points_sf$review))
  
  if (length(review_rows) == 0) {
    message("All gage pairs have been reviewed!")
    return(points_sf %>% st_drop_geometry())
  }
  
  message(sprintf("Starting review. %d rows remaining.", length(review_rows)))
  message("Commands: a = approve | b = bizarre | u = update gage | q = quit & save\n")
  
  for (i in review_rows) {
    
    pt <- points_sf[i, ]
    current_site <- pt[[point_site_col]][1]
    
    linked_gages <- gages_sf %>%
      filter(.data[[gage_site_col]] == current_site)
    
    if (nrow(linked_gages) == 0) {
      message(sprintf(
        "WARNING: no linked gage found for site_no %s. Skipping map for review_id %s.",
        current_site, pt$review_id
      ))
    }
    
    map_features <- bind_rows(
      pt %>% mutate(map_type = "input point"),
      linked_gages %>% mutate(map_type = "linked gage")
    )
    
    map_features_buf <- map_features %>%
      st_transform(3857) %>%
      st_buffer(buffer_m) %>%
      st_transform(4326)
    
    bb <- st_bbox(map_features_buf)
    
    m <- mapview(pt, col.regions = "blue", cex = 8, layer.name = "Input point") +
      mapview(linked_gages, col.regions = "red", cex = 8, layer.name = "Linked gage")
    
    m@map <- m@map %>%
      fitBounds(
        lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
        lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
      )
    
    print(m)
    
    message(sprintf(
      "[%d / %d] review_id: %s | full_list site_no: %s | lat/lon: %s, %s",
      match(i, review_rows),
      length(review_rows),
      pt$review_id,
      current_site,
      pt[[lat_col]][1],
      pt[[lon_col]][1]
    ))
    
    message(sprintf("Linked gages found: %d", nrow(linked_gages)))
    
    if (nrow(linked_gages) > 0) {
      message("\nLinked gages:")
      print(
        linked_gages %>%
          st_drop_geometry() %>%
          select(any_of(c(gage_site_col, "COMID", "GNIS_NAME", "REACHCODE", "TotDASqKM"))) %>%
          distinct()
      )
    }
    
    response <- ""
    
    while (!response %in% c("a", "b", "u", "q")) {
      response <- tolower(trimws(readline(
        "Review? [a = approve / b = bizarre / u = update / q = quit]: "
      )))
    }
    
    if (response == "q") {
      message("Quitting. Progress saved.")
      break
    }
    
    if (response == "a") {
      points_sf$review[i] <- "approve"
      points_sf$updated_site_no[i] <- current_site
    }
    
    if (response == "b") {
      points_sf$review[i] <- "bizarre"
      points_sf$updated_site_no[i] <- current_site
    }
    
    if (response == "u") {
      new_site <- trimws(readline("Enter updated / best gage site_no: "))
      
      if (!new_site %in% gages_sf[[gage_site_col]]) {
        message("WARNING: that site_no was not found in gages.")
      }
      
      points_sf$review[i] <- "update"
      points_sf$updated_site_no[i] <- new_site
    }
    
    message(sprintf("Saved: %s\n", points_sf$review[i]))
  }
  
  out <- points_sf %>% st_drop_geometry()
  
  reviewed <- sum(!is.na(out$review))
  remaining <- sum(is.na(out$review))
  
  message(sprintf("Done. %d reviewed, %d remaining.", reviewed, remaining))
  
  return(out)
}