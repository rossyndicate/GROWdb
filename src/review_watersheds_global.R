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