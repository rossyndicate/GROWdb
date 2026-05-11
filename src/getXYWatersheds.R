# getXYWatersheds — watershed delineation for individual CONUS GROWdb sample locations.
#
# All site-specific behavior (snap setting, buffer radius, COMID corrections, etc.)
# is read from a site_config row rather than hardcoded here. To change how a
# particular site is delineated, edit data/site_config.csv and re-run — do not
# add coordinate checks to this function.
#
# For new sites with no config row yet, all parameters fall back to sensible defaults
# (snap=TRUE, buffer_m=35, split_count=50, no COMID override). Run add_new_locations()
# and review the config before delineating those sites.
#
# Parameters:
#   sf          — single-row sf point with at minimum Latitude, Longitude, and
#                 location_id columns. Typically one row from new_dupes_removed
#                 after it has been joined with site_config.
#   coordinates — c(longitude, latitude) alternative to sf; supply crs too.
#   crs         — CRS integer/string for the coordinates argument.
#   site_config — single-row data frame from site_config.csv for this location.
#                 If NULL or zero rows, all defaults apply.
#   file_path   — directory where the per-site RDS is written.
#                 File is named {location_id}.RDS.
#
# Output RDS columns: location_id, comid, Latitude, Longitude, watershed, geometry

getXYWatersheds <- function(sf          = NULL,
                             coordinates = NULL,
                             crs         = NULL,
                             site_config = NULL,
                             file_path   = "data/grow_watersheds_all/") {

  if (is.null(sf) && is.null(coordinates)) stop("Provide `sf` or `coordinates`.")

  if (is.null(sf)) {
    aoi_raw <- tibble::tibble(long = coordinates[1], lat = coordinates[2]) %>%
      sf::st_as_sf(coords = c("long", "lat"), crs = crs)
  } else {
    aoi_raw <- sf
  }

  if (sf::st_crs(aoi_raw)$epsg != 4326) aoi <- sf::st_transform(aoi_raw, 4326) else aoi <- aoi_raw

  # ---------------------------------------------------------------------------
  # Read per-site config parameters; fall back to defaults if no config provided.
  # All of these were previously hardcoded as coordinate lookup tables.
  # ---------------------------------------------------------------------------

  has_cfg <- !is.null(site_config) && nrow(site_config) > 0

  snap              <- if (has_cfg) isTRUE(site_config$snap[1])              else TRUE
  buffer_m          <- if (has_cfg) site_config$buffer_m[1]                  else 35L
  split_count       <- if (has_cfg) site_config$split_count[1]               else 50L
  comid_override    <- if (has_cfg) site_config$comid_override[1]            else NA_integer_
  extra_comid       <- if (has_cfg) site_config$extra_comid[1]               else NA_integer_
  error_watershed   <- if (has_cfg) isTRUE(site_config$error_watershed[1])   else FALSE
  snap_to_endpoint  <- if (has_cfg) isTRUE(site_config$snap_to_endpoint[1])  else FALSE
  label_off_network <- if (has_cfg) isTRUE(site_config$label_off_network[1]) else FALSE
  nldi_from_flowline<- if (has_cfg) isTRUE(site_config$nldi_from_flowline[1])else FALSE

  # Stable file identifier; falls back to a temporary name for unregistered sites.
  out_id <- if (!is.null(aoi$location_id) && !is.na(aoi$location_id[1])) {
    aoi$location_id[1]
  } else {
    warning("Site has no location_id. Run add_new_locations() and review site_config.csv first.")
    paste0("UNREGISTERED_", aoi$Latitude[1], "_", aoi$Longitude[1])
  }

  # ---------------------------------------------------------------------------
  # Delineation — four paths in priority order:
  #   1. extra_comid    — site straddles two catchments; union approach
  #   2. comid_override — wrong auto-detected COMID; use NLDI with override
  #   3. nldi_from_flowline — multi-flowline AOI; NLDI from full COMID vector
  #   4. snap = FALSE   — off-network; boundary-point ensemble + split catchment
  #   5. snap = TRUE    — on-network; raindrop trace (with optional fixes)
  # ---------------------------------------------------------------------------

  if (!is.na(extra_comid)) {
    # ---- Path 1: dual-catchment site ----------------------------------------
    # The sample location sits where two NHD catchments meet. The watershed is
    # built by merging the NLDI basin for the adjacent catchment (extra_comid)
    # with the local split-catchment derived from a raindrop trace on the nearest
    # flowline. Without the merge, only half the true contributing area is captured.

    flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326)

    nearest_points    <- sf::st_nearest_points(aoi, flowline)
    snapped_points_sf <- sf::st_cast(nearest_points, "POINT")[2, ]

    trace    <- get_raindrop_trace(snapped_points_sf, direction = "down")
    raindrop <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]), crs = 4326)

    nhd_bb_catch <- get_split_catchment(raindrop, upstream = TRUE)[2, ] %>%
      sf::st_make_valid() %>%
      nngeo::st_remove_holes()

    nhd_catch <- get_nldi_basin(nldi_feature = list(featureSource = "comid",
                                                     featureID     = extra_comid)) %>%
      st_make_valid() %>%
      bind_rows(nhd_bb_catch) %>%
      summarize() %>%
      st_make_valid() %>%
      dplyr::mutate(comid       = extra_comid,
                    location_id = out_id,
                    Latitude    = aoi$Latitude,
                    Longitude   = aoi$Longitude,
                    watershed   = "NLDI") %>%
      dplyr::select(comid, location_id, Latitude, Longitude, watershed) %>%
      nngeo::st_remove_holes()

  } else if (!is.na(comid_override)) {
    # ---- Path 2: wrong auto-detected COMID ----------------------------------
    # Auto-detection places the site in the wrong NHD catchment (verified via
    # mapview). Use the manually corrected COMID for the NLDI basin call.

    nhd_catch <- get_nldi_basin(nldi_feature = list(featureSource = "comid",
                                                     featureID     = comid_override)) %>%
      st_make_valid() %>%
      dplyr::mutate(comid       = comid_override,
                    location_id = out_id,
                    Latitude    = aoi$Latitude,
                    Longitude   = aoi$Longitude,
                    watershed   = "NLDI") %>%
      dplyr::select(comid, location_id, Latitude, Longitude, watershed) %>%
      nngeo::st_remove_holes()

  } else if (nldi_from_flowline) {
    # ---- Path 3: multi-flowline AOI -----------------------------------------
    # get_nhdplus returns multiple flowlines for this AOI. Passing the full
    # COMID vector to get_nldi_basin and taking [[1]] matches the original
    # behavior that was observed to produce the correct result here.

    flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326)

    nhd_catch <- get_nldi_basin(list(featureSource = "comid",
                                     featureID     = flowline$comid)) %>%
      .[[1]] %>%
      st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      st_remove_holes() %>%
      sf::st_make_valid() %>%
      dplyr::mutate(comid       = flowline$comid,
                    location_id = out_id,
                    Latitude    = aoi$Latitude,
                    Longitude   = aoi$Longitude,
                    watershed   = "NLDI") %>%
      dplyr::select(comid, location_id, Latitude, Longitude, watershed)

  } else if (!snap) {
    # ---- Path 4: off-network (boundary-point ensemble) ----------------------
    # The sample location is not reached by the NHD stream network (e.g.,
    # headwater streams, small tributaries). Instead of snapping to the nearest
    # flowline and tracing, we:
    #   a. buffer the site by buffer_m metres (in CONUS Albers for valid distances)
    #   b. sample points around the buffer perimeter, excluding the flowline corridor
    #   c. call get_split_catchment from each candidate point
    #   d. return the largest result as the delineated watershed

    working_crs <- 5070  # CONUS Albers Equal Area (metre-based)
    aoi <- st_transform(aoi, working_crs)

    # Flowline "danger zone": a 30 m buffer around nearby NHD flowlines. Boundary
    # points that land inside here are excluded because split-catchment calls from
    # within the channel produce unreliable results.
    flowline <- get_nhdplus(AOI = st_transform(aoi, 4326), realization = "flowline", t_srs = 4326)
    flowline_danger_zone <- get_nhdplus(AOI = st_transform(aoi, 4326),
                                        realization = "flowline") %>%
      st_transform(working_crs) %>%
      st_buffer(30)

    site_buffer <- aoi %>% st_buffer(buffer_m)

    boundary_points <- site_buffer %>%
      st_boundary() %>%
      st_cast("POINT") %>%
      mutate(point_id = row_number()) %>%
      st_difference(., flowline_danger_zone) %>%
      filter(point_id %% split_count == 0) %>%  # thin to every Nth point
      bind_rows(aoi)                             # always include the original click

    # Call get_split_catchment from each candidate point; collect all results.
    # get_split_catchment expects 4326, so reproject each point before calling.
    splits <- vector("list", nrow(boundary_points))
    for (i in seq_len(nrow(boundary_points))) {
      pt_4326  <- st_transform(boundary_points[i, ], 4326) %>% st_as_sfc()
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
      dplyr::mutate(comid       = flowline$comid,
                    location_id = out_id,
                    Latitude    = aoi$Latitude,
                    Longitude   = aoi$Longitude,
                    watershed   = "Off-Network") %>%
      dplyr::select(comid, location_id, Latitude, Longitude, watershed) %>%
      nngeo::st_remove_holes() %>%
      st_make_valid() %>%
      st_buffer(0)

  } else {
    # ---- Path 5: on-network (raindrop trace) ---------------------------------
    # Standard path for most sites. Snap the pour point to the nearest NHD
    # flowline, run a raindrop trace downstream to find the reach intersection,
    # then call get_split_catchment from that point to get the upstream watershed.

    flowline <- get_nhdplus(AOI = aoi, realization = "flowline", t_srs = 4326)

    if (snap_to_endpoint) {
      # A few sites snap to the wrong tributary when using the nearest perpendicular
      # point. Snapping to the downstream endpoint of the flowline instead correctly
      # attributes the sample to its receiving water.
      lines_df <- st_cast(flowline, "LINESTRING")
      endpoints_list <- map_dfr(seq_len(nrow(lines_df)), function(i) {
        coords    <- st_coordinates(lines_df[i, ])
        start_pt  <- st_point(c(coords[1, "X"], coords[1, "Y"]))
        end_pt    <- st_point(c(coords[nrow(coords), "X"], coords[nrow(coords), "Y"]))
        data.frame(point_type = c("start", "end"),
                   geometry   = st_sfc(start_pt, end_pt, crs = st_crs(flowline)))
      })
      snapped_points_sf <- st_as_sf(endpoints_list) %>%
        filter(point_type == "end") %>%
        st_as_sfc()
    } else {
      nearest_points    <- sf::st_nearest_points(aoi, flowline)
      snapped_points_sf <- sf::st_cast(nearest_points, "POINT")[2, ]
    }

    trace    <- get_raindrop_trace(snapped_points_sf, direction = "down")
    raindrop <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]), crs = 4326)

    if (error_watershed) {
      # The NLDI basin boundary overshoots into the wrong drainage area for this
      # site (typically caused by a poorly-conditioned catchment near a confluence).
      # Fix: fetch the full NLDI basin, subtract the mis-attributed local NHD
      # catchment, then add back only the downstream portion of the split catchment.
      remove <- get_nhdplus(AOI = aoi, realization = "catchment")
      swap   <- get_split_catchment(raindrop, upstream = FALSE)[2, ] %>%
        sf::st_make_valid() %>%
        nngeo::st_remove_holes()

      nhd_catch <- get_nldi_basin(nldi_feature = list(featureSource = "comid",
                                                       featureID     = flowline$comid)) %>%
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
        dplyr::mutate(comid       = flowline$comid,
                      location_id = out_id,
                      Latitude    = aoi$Latitude,
                      Longitude   = aoi$Longitude,
                      watershed   = "NLDI") %>%
        dplyr::select(comid, location_id, Latitude, Longitude, watershed)

    } else {
      # Standard raindrop trace — the common case for on-network sites.
      nhd_catch <- get_split_catchment(raindrop, upstream = TRUE)[2, ] %>%
        sf::st_make_valid() %>%
        dplyr::mutate(comid       = flowline$comid,
                      location_id = out_id,
                      Latitude    = aoi$Latitude,
                      Longitude   = aoi$Longitude,
                      watershed   = "NHD Raindrop Trace") %>%
        dplyr::select(comid, location_id, Latitude, Longitude, watershed) %>%
        nngeo::st_remove_holes()
    }
  }

  # Reproject output back to the CRS of the original input.
  if (!identical(sf::st_crs(nhd_catch), sf::st_crs(aoi_raw))) {
    nhd_catch <- sf::st_transform(nhd_catch, sf::st_crs(aoi_raw))
  }

  # Override the watershed label if this site is too small to trust on-network
  # attribution, even if it was delineated via an on-network method.
  if (label_off_network) {
    nhd_catch <- nhd_catch %>% mutate(watershed = "Off-Network")
  }

  saveRDS(nhd_catch, file.path(file_path, paste0(out_id, ".RDS")))
}
