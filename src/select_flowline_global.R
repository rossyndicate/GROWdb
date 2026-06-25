select_flowline_global <- function(watersheds,
                                 sites,
                                 rivers,
                                 river_buffer_m = 10000,
                                 id_col         = "rowid") {
  
  stopifnot(inherits(watersheds, "sf"))
  stopifnot(inherits(sites,     "sf"))
  stopifnot(inherits(rivers,    "sf"))
  stopifnot(id_col %in% names(watersheds))
  stopifnot(id_col %in% names(sites))
  
  # ---------------------------------------------------------------------------
  # Pre-project everything to WGS84 once
  # ---------------------------------------------------------------------------
  watersheds_wgs <- sf::st_transform(watersheds, 4326)
  sites_wgs      <- sf::st_transform(sites,      4326)
  rivers_wgs     <- sf::st_transform(rivers,     4326)
  rivers_idx     <- sf::st_sfc(sf::st_geometry(rivers_wgs))
  
  sites_buffered <- sites_wgs %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(river_buffer_m) %>%
    sf::st_transform(4326)
  
  site_ids  <- watersheds_wgs[[id_col]]
  results   <- vector("list", length(site_ids))
  
  message(sprintf("Starting flowline selection for %d sites.", length(site_ids)))
  message("Click the HydroRIVERS segment the point should snap to.")
  message("Type q at the prompt to quit and return progress.\n")
  
  for (idx in seq_along(site_ids)) {
    
    site_id  <- site_ids[idx]
    watershed <- watersheds_wgs[idx, ]
    
    site <- sites_wgs %>%
      dplyr::filter(.data[[id_col]] == site_id)
    
    if (nrow(site) == 0) next
    
    site_buf <- sites_buffered %>%
      dplyr::filter(.data[[id_col]] == site_id)
    
    hits          <- sf::st_intersects(rivers_idx, site_buf, sparse = FALSE)
    nearby_rivers <- rivers_wgs[rowSums(hits) > 0, ]
    
    if (nrow(nearby_rivers) == 0) {
      message(sprintf("[%d / %d] %s: %s — no nearby rivers found, skipping.",
                      idx, length(site_ids), id_col, site_id))
      next
    }
    
    # Give each nearby river a local click-label so the user knows what they selected
    nearby_rivers$.river_label <- paste0("river_", seq_len(nrow(nearby_rivers)))
    
    # -------------------------------------------------------------------------
    # Build the map
    # -------------------------------------------------------------------------
    m <- mapview::mapview(
      watershed,
      alpha.regions = 0.2,
      color         = "orange",
      layer.name    = "Full upstream watershed"
    ) +
      mapview::mapview(
        nearby_rivers,
        zcol       = ".river_label",
        lwd        = 3,
        layer.name = "Nearby HydroRIVERS (click one)"
      ) +
      mapview::mapview(
        site,
        col.regions = "red",
        cex         = 8,
        layer.name  = "Sample location"
      )
    
    bb <- sf::st_bbox(site_buf)
    m@map <- m@map %>%
      leaflet::fitBounds(
        lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
        lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
      )
    
    print(m)
    
    message(sprintf("[%d / %d] %s: %s | %d nearby river segments shown.",
                    idx, length(site_ids), id_col, site_id, nrow(nearby_rivers)))
    
    # -------------------------------------------------------------------------
    # User selects a river via its row number
    # -------------------------------------------------------------------------
    selection <- ""
    valid_choices <- as.character(seq_len(nrow(nearby_rivers)))
    
    message(sprintf("Enter the row number of the river to select (1 - %d), or q to quit:",
                    nrow(nearby_rivers)))
    
    for (r in seq_len(nrow(nearby_rivers))) {
      message(sprintf("  [%d] %s", r, nearby_rivers$.river_label[r]))
    }
    
    while (!selection %in% c(valid_choices, "q")) {
      selection <- tolower(trimws(readline("Selection: ")))
    }
    
    if (selection == "q") {
      message("Quitting. Returning progress so far.")
      break
    }
    
    chosen_row    <- as.integer(selection)
    chosen_river  <- nearby_rivers[chosen_row, ] %>%
      dplyr::select(-.river_label) %>%
      dplyr::mutate("{id_col}" := site_id, .before = 1)
    
    results[[idx]] <- chosen_river
    
    message(sprintf("  Saved river row %d for %s: %s\n",
                    chosen_row, id_col, site_id))
  }
  
  # ---------------------------------------------------------------------------
  # Bind all selections into one sf table
  # ---------------------------------------------------------------------------
  out <- dplyr::bind_rows(Filter(Negate(is.null), results))
  
  message(sprintf("Done. %d / %d sites assigned a flowline.", nrow(out), length(site_ids)))
  
  out
}