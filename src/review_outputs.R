review_outputs <- function(site_master_table,
                           flowlines,
                           river_buffer_m = 10000,
                           sample_name_col = "sample_name",
                           lat_col = "Latitude",
                           lon_col = "Longitude",
                           hyriv_col = "HYRIV_ID",
                           stream_order_col = "ORD_STRA",
                           discharge_col = "DIS_AV_CMS",
                           main_riv_col = "MAIN_RIV",
                           max_candidates_to_print = Inf) {
  
  # ---------------------------------------------------------------------------
  # Purpose
  # ---------------------------------------------------------------------------
  # Fast interactive review of HydroRIVERS assignments in `site_master_table`.
  #
  # Important behavior:
  #   1. Reviews one row per distinct Latitude/Longitude site, not one row per
  #      sample.
  #   2. If the current HydroRIVERS choice is accepted with `y`, nothing is
  #      stored.
  #   3. If the current HydroRIVERS choice is rejected with `n`, the user selects
  #      a better nearby flowline and the function stores ONLY:
  #        - sample_names
  #        - Latitude
  #        - Longitude
  #        - HYRIV_ID
  #   4. The returned object is only the compact table of manual corrections.
  #      It does not return or mutate the full `site_master_table`.
  # ---------------------------------------------------------------------------
  
  # ---------------------------------------------------------------------------
  # Checks
  # ---------------------------------------------------------------------------
  stopifnot(is.data.frame(site_master_table))
  stopifnot(inherits(flowlines, "sf"))
  stopifnot(sample_name_col %in% names(site_master_table))
  stopifnot(lat_col %in% names(site_master_table))
  stopifnot(lon_col %in% names(site_master_table))
  stopifnot(hyriv_col %in% names(site_master_table))
  stopifnot(hyriv_col %in% names(flowlines))
  
  if (!stream_order_col %in% names(flowlines)) {
    alt_stream_order_col <- intersect(c("ORD_STRA", "StreamOrder", "stream_order"), names(flowlines))[1]
    if (is.na(alt_stream_order_col)) {
      stop("Could not find a Strahler stream-order column in `flowlines`.")
    }
    stream_order_col <- alt_stream_order_col
  }
  
  # Stable coordinate key. Avoid joining on exact floating point later by using
  # this key everywhere after distinct-site construction.
  site_master_table <- site_master_table %>%
    dplyr::mutate(
      .review_lat = round(.data[[lat_col]], 7),
      .review_lon = round(.data[[lon_col]], 7),
      .review_site_id = paste0(.review_lat, "__", .review_lon)
    )
  
  # ---------------------------------------------------------------------------
  # One row per distinct lat/long site
  # ---------------------------------------------------------------------------
  site_review <- site_master_table %>%
    dplyr::arrange(.review_site_id, .data[[sample_name_col]]) %>%
    dplyr::group_by(.review_site_id, .review_lat, .review_lon) %>%
    dplyr::summarise(
      sample_names = paste(sort(unique(as.character(.data[[sample_name_col]]))), collapse = ", "),
      n_samples = dplyr::n(),
      current_hyriv = {
        x <- .data[[hyriv_col]][!is.na(.data[[hyriv_col]])]
        if (length(x) == 0) NA else x[1]
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(.site_index = dplyr::row_number())
  
  message(sprintf(
    "Starting output review for %d distinct lat/long sites (%d sample rows total).",
    nrow(site_review),
    nrow(site_master_table)
  ))
  message("For each distinct site: y = current HydroRIVERS flowline is correct; n = choose a better nearby flowline.")
  message("Only `n` selections are stored in the returned correction table.\n")
  
  # ---------------------------------------------------------------------------
  # Build sf layers once
  # ---------------------------------------------------------------------------
  sites_sf <- site_review %>%
    sf::st_as_sf(coords = c(".review_lon", ".review_lat"), crs = 4326, remove = FALSE)
  
  site_buffers <- sites_sf %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(river_buffer_m) %>%
    sf::st_transform(4326)
  
  # Keep only columns needed in interactive review. This makes each map lighter.
  flow_keep <- unique(stats::na.omit(c(
    hyriv_col,
    stream_order_col,
    discharge_col,
    main_riv_col
  )))
  
  flowlines_wgs <- flowlines %>%
    sf::st_transform(4326) %>%
    dplyr::select(dplyr::all_of(flow_keep)) %>%
    dplyr::mutate(
      .flow_index = dplyr::row_number(),
      .stream_order_review = as.integer(.data[[stream_order_col]]),
      .hyriv_review = .data[[hyriv_col]]
    )
  
  # ---------------------------------------------------------------------------
  # Fast precomputation: one spatial-index query for all sites.
  # ---------------------------------------------------------------------------
  message("Precomputing nearby flowlines for all distinct sites...")
  candidate_list <- sf::st_intersects(site_buffers, flowlines_wgs)
  
  # Lookup for the already-selected HYRIV_ID. This ensures the selected line is
  # displayed even when it falls slightly outside the review buffer.
  selected_lookup <- flowlines_wgs %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.flow_index, .hyriv_review) %>%
    dplyr::filter(!is.na(.hyriv_review)) %>%
    dplyr::distinct(.hyriv_review, .keep_all = TRUE)
  
  corrections <- list()
  correction_i <- 0L
  
  # ---------------------------------------------------------------------------
  # Review loop over distinct sites only
  # ---------------------------------------------------------------------------
  for (site_i in seq_len(nrow(site_review))) {
    site_key <- site_review$.review_site_id[site_i]
    site <- sites_sf[site_i, ]
    site_buf <- site_buffers[site_i, ]
    current_hyriv_id <- site_review$current_hyriv[site_i]
    
    candidate_idx <- candidate_list[[site_i]]
    
    if (!is.na(current_hyriv_id)) {
      selected_idx <- selected_lookup$.flow_index[selected_lookup$.hyriv_review == current_hyriv_id]
      candidate_idx <- unique(c(candidate_idx, selected_idx))
    }
    
    nearby_flowlines <- flowlines_wgs[candidate_idx, ]
    
    if (nrow(nearby_flowlines) > 0) {
      nearby_flowlines <- nearby_flowlines %>%
        dplyr::distinct(.data[[hyriv_col]], .keep_all = TRUE) %>%
        dplyr::mutate(
          .candidate_row = dplyr::row_number(),
          .is_selected = !is.na(current_hyriv_id) & .data[[hyriv_col]] == current_hyriv_id,
          .flowline_label = paste0(
            "row_", .candidate_row,
            " | HYRIV_ID=", .data[[hyriv_col]],
            " | order=", .stream_order_review
          )
        )
    }
    
    # -------------------------------------------------------------------------
    # Build map. Nearby flowlines are colored by Strahler order. Current selected
    # flowline is drawn as a thick black casing plus order-colored top line.
    # -------------------------------------------------------------------------
    m <- mapview::mapview(
      site_buf,
      alpha.regions = 0.1,
      color = "gray",
      layer.name = "Search buffer"
    )
    
    if (nrow(nearby_flowlines) > 0) {
      m <- m + mapview::mapview(
        nearby_flowlines,
        zcol = ".stream_order_review",
        lwd = 3,
        layer.name = "Nearby HydroRIVERS by Strahler order"
      )
      
      selected_flowline <- nearby_flowlines %>% dplyr::filter(.is_selected)
      
      if (nrow(selected_flowline) > 0) {
        m <- m +
          mapview::mapview(
            selected_flowline,
            color = "black",
            lwd = 10,
            layer.name = "Current selected HydroRIVERS highlight"
          ) +
          mapview::mapview(
            selected_flowline,
            zcol = ".stream_order_review",
            lwd = 6,
            layer.name = "Current selected HydroRIVERS by Strahler order"
          )
      }
    }
    
    m <- m + mapview::mapview(
      site,
      col.regions = "red",
      cex = 8,
      layer.name = "Sample location"
    )
    
    bb <- sf::st_bbox(site_buf)
    m@map <- m@map %>%
      leaflet::fitBounds(
        lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
        lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
      )
    
    print(m)
    
    message(sprintf(
      "[%d / %d] distinct site %s | lat/lon: %s, %s | samples at site: %d",
      site_i,
      nrow(site_review),
      site_key,
      round(site_review$.review_lat[site_i], 6),
      round(site_review$.review_lon[site_i], 6),
      site_review$n_samples[site_i]
    ))
    message(sprintf("Sample names: %s", site_review$sample_names[site_i]))
    message(sprintf("Current selected %s: %s", hyriv_col, current_hyriv_id))
    message(sprintf("Nearby HydroRIVERS shown: %d", nrow(nearby_flowlines)))
    
    if (nrow(nearby_flowlines) > 0) {
      message("Candidate flowlines:")
      rows_to_print <- seq_len(min(nrow(nearby_flowlines), max_candidates_to_print))
      
      for (r in rows_to_print) {
        river_name <- if (main_riv_col %in% names(nearby_flowlines)) nearby_flowlines[[main_riv_col]][r] else NA
        river_q <- if (discharge_col %in% names(nearby_flowlines)) nearby_flowlines[[discharge_col]][r] else NA
        selected_marker <- if (isTRUE(nearby_flowlines$.is_selected[r])) "  <-- current selected" else ""
        
        message(sprintf(
          "  [%d] HYRIV_ID: %s | Strahler: %s | discharge cms: %s | main river: %s%s",
          r,
          nearby_flowlines[[hyriv_col]][r],
          nearby_flowlines$.stream_order_review[r],
          round(as.numeric(river_q), 3),
          river_name,
          selected_marker
        ))
      }
      
      if (is.finite(max_candidates_to_print) && nrow(nearby_flowlines) > max_candidates_to_print) {
        message(sprintf("  ... %d additional candidates not printed", nrow(nearby_flowlines) - max_candidates_to_print))
      }
    }
    
    response <- ""
    while (!response %in% c("y", "n", "q")) {
      response <- tolower(trimws(readline("Accept current selected flowline? [y/n/q]: ")))
    }
    
    if (response == "q") {
      message("Quitting. Returning corrections selected so far.")
      break
    }
    
    if (response == "y") {
      message("  Accepted. Nothing stored for this site.\n")
      next
    }
    
    # -------------------------------------------------------------------------
    # If rejected, select a better flowline by row number. Store only the compact
    # correction record requested by the user.
    # -------------------------------------------------------------------------
    if (nrow(nearby_flowlines) == 0) {
      message("No candidate flowlines are available to choose from. Nothing stored for this site.\n")
      next
    }
    
    valid_rows <- as.character(seq_len(nrow(nearby_flowlines)))
    selection <- ""
    while (!selection %in% valid_rows) {
      selection <- trimws(readline(sprintf("Select better flowline row [1-%d]: ", nrow(nearby_flowlines))))
    }
    
    chosen_row <- as.integer(selection)
    chosen_flowline <- nearby_flowlines[chosen_row, ]
    
    correction_i <- correction_i + 1L
    corrections[[correction_i]] <- tibble::tibble(
      sample_names = site_review$sample_names[site_i],
      Latitude = site_review$.review_lat[site_i],
      Longitude = site_review$.review_lon[site_i],
      HYRIV_ID = chosen_flowline[[hyriv_col]][1]
    )
    
    message(sprintf(
      "  Stored correction only: HYRIV_ID %s for %d sample row(s) at this lat/long.\n",
      chosen_flowline[[hyriv_col]][1],
      site_review$n_samples[site_i]
    ))
  }
  
  out <- dplyr::bind_rows(corrections)
  
  if (nrow(out) == 0) {
    out <- tibble::tibble(
      sample_names = character(),
      Latitude = numeric(),
      Longitude = numeric(),
      HYRIV_ID = numeric()
    )
  }
  
  message(sprintf("Done. Stored %d manual HydroRIVERS correction(s).", nrow(out)))
  
  out
}
