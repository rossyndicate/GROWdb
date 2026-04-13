review_watersheds <- function(watersheds, sites, flowlines) {
  
  # Add review column if it doesn't exist
  if (!"review" %in% names(watersheds)) {
    watersheds$review <- NA_character_
  }
  
  # Find starting index (skip already reviewed)
  remaining <- which(is.na(watersheds$review))
  
  if (length(remaining) == 0) {
    message("All watersheds have been reviewed!")
    return(watersheds)
  }
  
  message(sprintf("Starting review. %d of %d watersheds remaining.\n", 
                  length(remaining), nrow(watersheds)))
  message("Commands: y = Yes | n = No | s = Skip | q = Quit & save\n")
  
  for (i in remaining) {
    watershed <- watersheds[i, ]
    site      <- sites %>% filter(sample_name == watershed$sample_name)
    flowline  <- flowlines %>% filter(comid == watershed$comid)
    bb <- st_bbox(flowline)
    # Build map zoomed to flowline bbox
    m <- mapview(flowline, color = "steelblue", lwd = 3) +
      mapview(watershed, alpha.regions = 0.2, color = "orange") +
      mapview(site, col.regions = "red", cex = 8)
    
    m@map <- m@map %>%
      leaflet::fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
                         lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
    
    
    print(m)
    
    # Prompt
    message(sprintf("[%d / %d] sample: %s | comid: %s",
                    i, nrow(watersheds),
                    watershed$sample_name, watershed$comid))
    
    response <- ""
    while (!response %in% c("y", "n", "s", "q")) {
      response <- tolower(trimws(readline("  Accept? [y/n/s/q]: ")))
    }
    
    if (response == "q") {
      message("Quitting. Progress saved.")
      break
    } else if (response == "s") {
      message("  Skipped.\n")
      next
    } else {
      watersheds$review[i] <- toupper(response)
      message(sprintf("  Saved: %s\n", toupper(response)))
    }
  }
  
  reviewed  <- sum(!is.na(watersheds$review))
  remaining <- sum(is.na(watersheds$review))
  message(sprintf("Done. %d reviewed, %d remaining.", reviewed, remaining))
  
  return(watersheds)
}