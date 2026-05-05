review_point_pairs <- function(new_points, old_points,
                               sample_col_new = "sample_name",
                               sample_col_old = "SampleName",
                               buffer_m = 2500) {
  
  stopifnot(inherits(new_points, "sf"))
  stopifnot(inherits(old_points, "sf"))
  
  # Add review column if needed
  if (!"review" %in% names(new_points)) {
    new_points$review <- NA_character_
  }
  
  # Only review samples that exist in both datasets
  shared_samples <- intersect(new_points[[sample_col_new]], old_points[[sample_col_old]])
  
  review_rows <- which(
    new_points[[sample_col_new]] %in% shared_samples &
      is.na(new_points$review)
  )
  
  if (length(review_rows) == 0) {
    message("All point pairs have been reviewed!")
    return(new_points)
  }
  
  message(sprintf(
    "Starting review. %d point pairs remaining.\n",
    length(review_rows)
  ))
  message("Commands: y = acceptable | n = not acceptable | q = quit & save\n")
  
  for (i in review_rows) {
    
    sample_id <- new_points[[sample_col_new]][i]
    
    new_pt <- new_points[i, ]
    
    old_pt <- old_points %>%
      dplyr::filter(.data[[sample_col_old]] == sample_id)
    
    if (nrow(old_pt) == 0) {
      message(sprintf("Skipping %s: no comparison point found.", sample_id))
      next
    }
    
    # Make sure CRS matches
    old_pt <- sf::st_transform(old_pt, sf::st_crs(new_pt))
    
    # Combine pair for bbox
    pair <- dplyr::bind_rows(
      new_pt %>% dplyr::mutate(point_source = "new"),
      old_pt %>% dplyr::mutate(point_source = "previous")
    )
    
    # Buffer around both points for zoom
    pair_buf <- pair %>%
      sf::st_transform(3857) %>%
      sf::st_buffer(buffer_m) %>%
      sf::st_transform(sf::st_crs(pair))
    
    bb <- sf::st_bbox(pair_buf)
    
    # Map: new point default, old/comparison point red
    m <- mapview::mapview(new_pt, col.regions = "blue", cex = 8) +
      mapview::mapview(old_pt, col.regions = "red", cex = 8)
    
    m@map <- m@map %>%
      leaflet::fitBounds(
        lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
        lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
      )
    
    print(m)
    
    message(sprintf(
      "[%d / %d] sample: %s",
      match(i, review_rows),
      length(review_rows),
      sample_id
    ))
    
    response <- ""
    while (!response %in% c("y", "n", "q")) {
      response <- tolower(trimws(readline("Difference acceptable? [y/n/q]: ")))
    }
    
    if (response == "q") {
      message("Quitting. Progress saved.")
      break
    }
    
    new_points$review[i] <- toupper(response)
    message(sprintf("  Saved: %s\n", toupper(response)))
  }
  
  reviewed <- sum(!is.na(new_points$review))
  remaining <- sum(is.na(new_points$review))
  
  message(sprintf("Done. %d reviewed, %d remaining.", reviewed, remaining))
  
  return(new_points)
}