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
  message("Workflow:")
  message("  1. Point-scale map (Enter or y/s/m/b to continue)")
  message("  2. Watershed-scale map (final y/s/b/q)")
  message("Commands: y = Yes | s = Small | b = Big | q = Quit & save\n")
  
  for (i in remaining) {
    watershed <- watersheds[i, ]
    site      <- sites %>% dplyr::filter(rowid == watershed$rowid)
    flowline  <- flowlines %>% dplyr::filter(comid == watershed$comid)
    
    # -----------------------------
    # Map 1: site-centered view
    # -----------------------------
    m1 <- mapview::mapview(watershed, alpha.regions = 0.2, color = "orange")
    
    if (nrow(flowline) > 0) {
      m1 <- m1 + mapview::mapview(flowline, color = "steelblue", lwd = 3)
    }
    
    if (nrow(site) > 0) {
      m1 <- m1 + mapview::mapview(site, col.regions = "red", cex = 8)
      
      # ~5 km x 5 km box around point
      site_buf <- site %>%
        sf::st_transform(3857) %>%
        sf::st_buffer(2500) %>%
        sf::st_transform(sf::st_crs(site))
      
      bb1 <- sf::st_bbox(site_buf)
    } else {
      # fallback if no site
      bb1 <- sf::st_bbox(watershed)
    }
    
    m1@map <- m1@map %>%
      leaflet::fitBounds(
        lng1 = bb1[["xmin"]], lat1 = bb1[["ymin"]],
        lng2 = bb1[["xmax"]], lat2 = bb1[["ymax"]]
      )
    
    print(m1)
    
    message(sprintf("[Map 1/2] sample: %s | comid: %s",
                    watershed$rowid, watershed$comid))
    
    continue_key <- "INVALID"
    while (!continue_key %in% c("", "y", "s", "m", "b")) {
      continue_key <- tolower(trimws(readline("Press Enter or y/s/m/b to continue: ")))
    }
    
    # -----------------------------
    # Map 2: watershed-centered view
    # -----------------------------
    m2 <- mapview::mapview(watershed, alpha.regions = 0.2, color = "orange")
    
    if (nrow(flowline) > 0) {
      m2 <- m2 + mapview::mapview(flowline, color = "steelblue", lwd = 3)
    }
    
    if (nrow(site) > 0) {
      m2 <- m2 + mapview::mapview(site, col.regions = "red", cex = 8)
    }
    
    bb2 <- sf::st_bbox(watershed)
    m2@map <- m2@map %>%
      leaflet::fitBounds(
        lng1 = bb2[["xmin"]], lat1 = bb2[["ymin"]],
        lng2 = bb2[["xmax"]], lat2 = bb2[["ymax"]]
      )
    
    print(m2)
    
    message(sprintf("[Map 2/2] [%d / %d] sample: %s | comid: %s",
                    i, nrow(watersheds),
                    watershed$rowid, watershed$comid))
    
    response <- ""
    while (!response %in% c("y", "s", "b", "q")) {
      response <- tolower(trimws(readline("Accept? [y/s/b/q]: ")))
    }
    
    if (response == "q") {
      message("Quitting. Progress saved.")
      break
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