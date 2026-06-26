select_flowline_global <- function(flowlines,
                                   sites,
                                   river_buffer_m = 10000,
                                   id_col         = "rowid") {
  
  stopifnot(inherits(flowlines, "sf"))
  stopifnot(inherits(sites,     "sf"))
  stopifnot(id_col %in% names(sites))
  
  # ---------------------------------------------------------------------------
  # Pre-project everything to WGS84 once
  # ---------------------------------------------------------------------------
  sites_wgs     <- sf::st_transform(sites,     4326)
  flowlines_wgs <- sf::st_transform(flowlines, 4326)
  flowlines_idx <- sf::st_sfc(sf::st_geometry(flowlines_wgs))
  
  sites_buffered <- sites_wgs %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(river_buffer_m) %>%
    sf::st_transform(4326)
  
  site_ids <- sites_wgs[[id_col]]
  results  <- vector("list", length(site_ids))
  
  message(sprintf("Starting flowline selection for %d sites.", length(site_ids)))
  message("Click/inspect the HydroRIVERS segment the point should snap to.")
  message("Enter a row number to select a flowline.")
  message("Enter s/small if the site is too small or no flowline is shown.")
  message("Enter b/big if the site is too big for flowlines, e.g., estuary.")
  message("Enter q to quit and return progress.\n")
  
  for (idx in seq_along(site_ids)) {
    
    site_id <- site_ids[idx]
    
    site <- sites_wgs %>%
      dplyr::filter(.data[[id_col]] == site_id)
    
    if (nrow(site) == 0) next
    
    site_buf <- sites_buffered %>%
      dplyr::filter(.data[[id_col]] == site_id)
    
    # -------------------------------------------------------------------------
    # Candidate flowlines near the site
    # -------------------------------------------------------------------------
    hits <- sf::st_intersects(flowlines_idx, site_buf, sparse = FALSE)
    
    nearby_flowlines <- flowlines_wgs[rowSums(hits) > 0, ]
    
    # -------------------------------------------------------------------------
    # If no flowlines are nearby, still show the point and allow small/big/q
    # -------------------------------------------------------------------------
    if (nrow(nearby_flowlines) == 0) {
      
      m <- mapview::mapview(
        site_buf,
        alpha.regions = 0.1,
        color         = "gray",
        layer.name    = "Search buffer"
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
      
      message(sprintf("[%d / %d] %s: %s — no nearby flowlines found.",
                      idx, length(site_ids), id_col, site_id))
      message("Enter s/small, b/big, or q to quit:")
      
      selection <- ""
      
      while (!selection %in% c("s", "small", "b", "big", "q")) {
        selection <- tolower(trimws(readline("Selection: ")))
      }
      
      if (selection == "q") {
        message("Quitting. Returning progress so far.")
        break
      }
      
      flowline_status <- dplyr::case_when(
        selection %in% c("s", "small") ~ "small_no_flowline",
        selection %in% c("b", "big")   ~ "big_estuary_or_too_large"
      )
      
      results[[idx]] <- site %>%
        dplyr::mutate(
          flowline_status = flowline_status,
          selected_row    = NA_integer_,
          .before         = 1
        )
      
      message(sprintf("  Saved %s for %s: %s\n",
                      flowline_status, id_col, site_id))
      
      next
    }
    
    # Give each nearby flowline a local click-label
    nearby_flowlines$.flowline_label <- paste0("flowline_", seq_len(nrow(nearby_flowlines)))
    
    # -------------------------------------------------------------------------
    # Build the map
    # -------------------------------------------------------------------------
    m <- mapview::mapview(
      site_buf,
      alpha.regions = 0.1,
      color         = "gray",
      layer.name    = "Search buffer"
    ) +
      mapview::mapview(
        nearby_flowlines,
        zcol       = ".flowline_label",
        lwd        = 3,
        layer.name = "Nearby HydroRIVERS flowlines"
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
    
    message(sprintf("[%d / %d] %s: %s | %d nearby flowlines shown.",
                    idx, length(site_ids), id_col, site_id, nrow(nearby_flowlines)))
    
    # -------------------------------------------------------------------------
    # User selects a flowline, or marks as small/big
    # -------------------------------------------------------------------------
    valid_rows    <- as.character(seq_len(nrow(nearby_flowlines)))
    valid_choices <- c(valid_rows, "s", "small", "b", "big", "q")
    
    message(sprintf("Enter the row number of the flowline to select (1 - %d), s/small, b/big, or q:",
                    nrow(nearby_flowlines)))
    
    for (r in seq_len(nrow(nearby_flowlines))) {
      message(sprintf("  [%d] %s", r, nearby_flowlines$.flowline_label[r]))
    }
    
    selection <- ""
    
    while (!selection %in% valid_choices) {
      selection <- tolower(trimws(readline("Selection: ")))
    }
    
    if (selection == "q") {
      message("Quitting. Returning progress so far.")
      break
    }
    
    if (selection %in% c("s", "small", "b", "big")) {
      
      flowline_status <- dplyr::case_when(
        selection %in% c("s", "small") ~ "small_no_flowline",
        selection %in% c("b", "big")   ~ "big_estuary_or_too_large"
      )
      
      results[[idx]] <- site %>%
        dplyr::mutate(
          flowline_status = flowline_status,
          selected_row    = NA_integer_,
          .before         = 1
        )
      
      message(sprintf("  Saved %s for %s: %s\n",
                      flowline_status, id_col, site_id))
      
      next
    }
    
    chosen_row <- as.integer(selection)
    
    chosen_flowline <- nearby_flowlines[chosen_row, ] %>%
      dplyr::select(-.flowline_label) %>%
      dplyr::mutate(
        "{id_col}"       := site_id,
        flowline_status  = "selected_flowline",
        selected_row     = chosen_row,
        .before          = 1
      )
    
    results[[idx]] <- chosen_flowline
    
    message(sprintf("  Saved flowline row %d for %s: %s\n",
                    chosen_row, id_col, site_id))
  }
  
  # ---------------------------------------------------------------------------
  # Bind all selections into one sf table
  # ---------------------------------------------------------------------------
  out <- dplyr::bind_rows(Filter(Negate(is.null), results))
  
  message(sprintf("Done. %d / %d sites assigned a status.",
                  nrow(out), length(site_ids)))
  
  out
}