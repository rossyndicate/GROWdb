# bootstrap_site_config.R
#
# ONE-TIME SETUP SCRIPT — run this once to create data/site_config.csv from the
# current site list. After that, site_config.csv is the authoritative source for
# all per-location delineation settings. Do NOT source this into the main workflow;
# run it interactively or via Rscript.
#
# To add new locations in the future, use add_new_locations() defined below —
# that function appends rows with defaults rather than rebuilding the whole file.
#
# site_config.csv columns:
#   location_id        — stable unique ID per (Latitude, Longitude); never changes
#   Latitude, Longitude
#   snap               — TRUE = on-network (raindrop trace); FALSE = off-network
#                        (boundary-point ensemble). Use FALSE for headwater/small sites
#                        where the NHD stream network doesn't reach the sample location.
#   buffer_m           — search radius (m) for the off-network boundary ensemble.
#                        Default 35. Increase for sites with diffuse flow paths or
#                        where the 35 m buffer sits entirely within the flowline corridor.
#   split_count        — keep every Nth boundary point in the ensemble. Default 50.
#                        Reduce to 10 for sites with complex terrain where dense
#                        sampling causes split-catchment calls to cross flow divides.
#   comid_override     — manually verified COMID when auto-detection places the site
#                        in the wrong NHD catchment. Leave NA for auto-detection.
#   extra_comid        — secondary COMID used when a site straddles two adjacent
#                        catchments; the function unions the NLDI basin for this COMID
#                        with the local split-catchment. Leave NA if not needed.
#   error_watershed    — TRUE for sites where the NLDI basin boundary overshoots into
#                        the wrong drainage area. The function removes the mis-attributed
#                        local NHD catchment and swaps in only the downstream split-
#                        catchment portion. Only valid when snap = TRUE.
#   snap_to_endpoint   — TRUE for sites where snapping to the nearest point on the
#                        flowline captures the wrong tributary; snapping to the
#                        downstream line endpoint gives the correct catchment instead.
#   label_off_network  — TRUE to relabel the delineated watershed as "Off-Network"
#                        in the output, even if another method was used. For sites
#                        that are technically on the network but whose catchment is
#                        too small to trust the result.
#   nldi_from_flowline — TRUE for a single site (32.3397, -90.9125) where the AOI
#                        contains multiple flowlines and the NLDI basin must be
#                        retrieved for the full COMID list, taking the first result.
#   is_international   — TRUE for sites that pass the tigris CONUS boundary filter
#                        but are actually on international waters (e.g., Columbia R.
#                        near the Canada border). These are excluded from CONUS
#                        delineation and handled by international_workflow.Rmd instead.
#   notes              — free-text explanation of why this site has non-default settings.

library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# ---------------------------------------------------------------------------
# 1. All known special cases (previously hardcoded in getXYWatersheds.R)
#    Add a row here whenever you discover a new site that needs non-default
#    treatment, then re-run this script to regenerate site_config.csv.
# ---------------------------------------------------------------------------

# Sites too small or too far from the NHD stream network for on-network
# delineation. Even if the calling code passes snap=TRUE, these sites are
# routed through the off-network (boundary-point ensemble) path.
snap_false_sites <- tibble(
  Latitude  = c(44.49060, 38.92329, 44.21339, 38.98269, 32.42735, 33.33772,
                40.78319, 40.77970, 33.33747, 43.95472, 38.89520, 41.86957,
                43.95472, 38.87497, 37.05229, 40.78464, 40.78010, 42.52339,
                38.89440, 37.05229, 41.86957, 38.92329, 44.21339, 32.42735,
                38.87486, 44.20732, 44.20852, 44.21339),
  Longitude = c(-72.16220, -106.94239, -122.24398, -107.00515, -110.75784, -81.71816,
                -111.80126, -111.80640, -81.71821, -71.72278, -78.14760, -73.00222,
                -71.72278, -76.54649, -119.19536, -111.79547, -111.80540, -71.18545,
                -78.14740, -119.19536, -73.00222, -106.94239, -122.24398, -110.75784,
                -76.54654, -122.25810, -122.25680, -122.24400)
)

# Sites requiring a larger buffer because the 35 m default sits within the
# flowline danger zone or because flow paths are unusually diffuse.
buffer_80_sites <- tibble(
  Latitude  = c(40.78319, 37.05229, 41.86957, 38.92329),
  Longitude = c(-111.80126, -119.19536, -73.00222, -106.94239),
  buffer_m  = 80L
)

# One site with very low relief where the stream corridor is wide enough that
# even 80 m still lands in the flowline danger zone.
buffer_250_sites <- tibble(
  Latitude  = 44.21339,
  Longitude = -122.24398,
  buffer_m  = 250L
)

# Sites where the default 50th-point thinning samples across flow divides in
# complex terrain, causing get_split_catchment to return erratic results.
# Reducing to every 10th point keeps the ensemble within the true catchment.
split_10_sites <- tibble(
  Latitude  = c(38.89520, 38.89440, 40.78319, 37.05229, 41.86957,
                38.92329, 44.21339, 44.20970),
  Longitude = c(-78.14760, -78.14740, -111.80126, -119.19536, -73.00222,
                -106.94239, -122.24398, -122.25700)
)

# Sites where auto-detected COMIDs place the sample in the wrong NHD catchment,
# verified via mapview inspection of the site location vs. catchment boundary.
comid_override_sites <- tibble(
  Latitude       = c(30.75852, 41.01528, 46.73222, 36.95497, 43.68620, 31.85310, 37.21647),
  Longitude      = c(-91.39595, -96.15778, -117.18038, -119.02375, -121.68733, -88.15748, -89.46758),
  comid_override = c(19085457L, 17416032L, 23459939L, 22050327L, 23702001L, 18548456L, 5092616L)
)

# One site that straddles two adjacent NHD catchments. The watershed is built by
# unioning the NLDI basin for extra_comid (the adjacent catchment) with the local
# split-catchment from a raindrop trace. site_comid 13633173 is recorded in notes.
extra_comid_site <- tibble(
  Latitude    = 43.10950,
  Longitude   = -89.64070,
  extra_comid = 13633277L
)

# Sites where the full NLDI basin boundary overshoots into the wrong drainage
# area (typically caused by a nearby confluence). The fix removes the local NHD
# catchment and replaces it with only the downstream portion of the split catchment.
error_watershed_sites <- tibble(
  Latitude  = c(30.69074, 30.44567, 29.69282, 38.70981, 32.44417,
                32.71866, 29.85715, 38.89440, 41.86958),
  Longitude = c(-91.73623, -91.19156, -91.21194, -91.43850, -90.91417,
                -114.71884, -89.97785, -78.14740, -72.79677)
)

# One site where snapping to the nearest point on the flowline captures the
# wrong upstream tributary. Snapping to the downstream line endpoint instead
# correctly attributes the sample to its actual receiving water.
snap_endpoint_site <- tibble(Latitude = 38.87486, Longitude = -76.54654)

# Sites that are technically on the NHD network but whose delineated polygon
# should be labeled "Off-Network" because the catchment is too small to be
# reliably attributed.
label_off_network_sites <- tibble(
  Latitude  = c(42.52356, 42.52339),
  Longitude = c(-71.18482, -71.18545)
)

# Single site where get_nhdplus returns multiple flowlines for the AOI and
# get_nldi_basin must be called with the full COMID vector, taking [[1]].
nldi_from_flowline_site <- tibble(Latitude = 32.33970, Longitude = -90.91250)

# Sample names that appear within the tigris CONUS boundary filter but are
# actually on international waters. These are excluded from CONUS delineation.
international_sample_names <- c(
  "columbia_2019_sw_WHONDRS-S19S_0036",
  "columbia_2019_sw_WHONDRS-S19S_0074",
  "columbiariver1_2019_sw_WHONDRS-S19S_0075",
  "columbiariver2_2019_sw_WHONDRS-S19S_0076",
  "NASQAN2016_178", "NASQAN2016_179",
  "NASQAN2014_141", "NASQAN2014_144",
  "NASQAN2015_031", "NASQAN2016_164"
)

# ---------------------------------------------------------------------------
# 2. Helper: match (Latitude, Longitude) pairs by rounded coordinates
# ---------------------------------------------------------------------------

coord_key <- function(lat, lon, digits = 4) paste(round(lat, digits), round(lon, digits))

in_ref <- function(lat, lon, ref_df, digits = 4) {
  coord_key(lat, lon, digits) %in% coord_key(ref_df$Latitude, ref_df$Longitude, digits)
}

lookup_val <- function(lat, lon, ref_df, val_col, digits = 4) {
  key     <- coord_key(lat, lon, digits)
  ref_key <- coord_key(ref_df$Latitude, ref_df$Longitude, digits)
  idx     <- match(key, ref_key)
  ifelse(is.na(idx), NA, ref_df[[val_col]][idx])
}

# ---------------------------------------------------------------------------
# 3. Load all CONUS sample locations and assign stable location_ids
# ---------------------------------------------------------------------------

conus_bbox <- tigris::states() %>%
  filter(!STUSPS %in% c("HI", "PR", "AK")) %>%
  st_buffer(10000)

# Apply known coordinate correction before spatial filter
all_sites <- read_csv("data/date_lat_long_grow_KKA_05.05.26.csv", show_col_types = FALSE) %>%
  mutate(
    Longitude = if_else(sample_name == "marshallgulch_2019_sw_WHONDRS-S19S_0035", -110.75784, Longitude),
    Latitude  = if_else(sample_name == "marshallgulch_2019_sw_WHONDRS-S19S_0035",   32.42735, Latitude)
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269, remove = FALSE) %>%
  .[conus_bbox, ] %>%
  st_drop_geometry()

# Resolve coordinates for international sample names so they can be flagged below
intl_coords <- all_sites %>%
  filter(sample_name %in% international_sample_names) %>%
  distinct(Latitude, Longitude)

# Unique (Latitude, Longitude) pairs — sorted north-to-south, west-to-east for a
# deterministic initial ordering. IDs are then permanently fixed in the CSV.
unique_locations <- all_sites %>%
  distinct(Latitude, Longitude) %>%
  arrange(desc(Latitude), Longitude) %>%
  mutate(location_id = sprintf("CONUS_%04d", row_number()))

# ---------------------------------------------------------------------------
# 4. Assemble the config by applying special-case flags
# ---------------------------------------------------------------------------

site_config <- unique_locations %>%
  mutate(
    # Delineation method
    snap = !in_ref(Latitude, Longitude, snap_false_sites),

    # Off-network buffer size (used only when snap = FALSE)
    buffer_m = case_when(
      in_ref(Latitude, Longitude, buffer_250_sites) ~ 250L,
      in_ref(Latitude, Longitude, buffer_80_sites)  ~ 80L,
      TRUE                                          ~ 35L
    ),

    # Boundary-point thinning interval (used only when snap = FALSE)
    split_count = if_else(in_ref(Latitude, Longitude, split_10_sites), 10L, 50L),

    # COMID corrections and additions
    comid_override = as.integer(
      lookup_val(Latitude, Longitude, comid_override_sites, "comid_override")
    ),
    extra_comid = as.integer(
      lookup_val(Latitude, Longitude, extra_comid_site, "extra_comid")
    ),

    # Special on-network flags (snap = TRUE sites only)
    error_watershed   = in_ref(Latitude, Longitude, error_watershed_sites),
    snap_to_endpoint  = in_ref(Latitude, Longitude, snap_endpoint_site),
    nldi_from_flowline = in_ref(Latitude, Longitude, nldi_from_flowline_site),

    # Post-processing label override
    label_off_network = in_ref(Latitude, Longitude, label_off_network_sites),

    # Exclude from CONUS delineation (routed to international_workflow.Rmd instead)
    is_international  = in_ref(Latitude, Longitude, intl_coords),

    # Human-readable explanation for any non-default setting
    notes = case_when(
      in_ref(Latitude, Longitude, extra_comid_site) ~
        "Site straddles two adjacent NHD catchments; local split-catchment unioned with NLDI basin for extra_comid. site_comid = 13633173.",
      in_ref(Latitude, Longitude, comid_override_sites) ~
        "Auto-detected COMID places site in wrong NHD catchment; COMID manually verified via mapview.",
      in_ref(Latitude, Longitude, nldi_from_flowline_site) ~
        "AOI contains multiple flowlines; NLDI basin retrieved for full COMID vector, first result used.",
      in_ref(Latitude, Longitude, snap_endpoint_site) ~
        "Nearest-point snap captures wrong tributary; downstream endpoint snap gives correct catchment.",
      in_ref(Latitude, Longitude, error_watershed_sites) ~
        "NLDI basin overshoots into wrong drainage area; local NHD catchment removed and downstream split-catchment swapped in.",
      in_ref(Latitude, Longitude, label_off_network_sites) ~
        "Catchment too small to reliably attribute to NHD network; relabeled Off-Network.",
      in_ref(Latitude, Longitude, snap_false_sites) ~
        "Off-network site; sample location not reached by NHD stream network.",
      in_ref(Latitude, Longitude, intl_coords) ~
        "Passes CONUS boundary filter but is on international waters; handle in international_workflow.Rmd.",
      TRUE ~ NA_character_
    )
  )

# ---------------------------------------------------------------------------
# 5. Write to CSV
# ---------------------------------------------------------------------------

write_csv(site_config, "data/site_config.csv")
cat(sprintf("Created data/site_config.csv with %d unique CONUS locations.\n", nrow(site_config)))
cat(sprintf("  snap=FALSE (off-network): %d\n",    sum(!site_config$snap, na.rm = TRUE)))
cat(sprintf("  comid_override set:       %d\n",    sum(!is.na(site_config$comid_override))))
cat(sprintf("  extra_comid set:          %d\n",    sum(!is.na(site_config$extra_comid)))  )
cat(sprintf("  error_watershed:          %d\n",    sum(site_config$error_watershed)))
cat(sprintf("  is_international:         %d\n",    sum(site_config$is_international)))

# ---------------------------------------------------------------------------
# 6. add_new_locations() — append new (Lat, Lon) pairs to an existing config
#    Source this file or copy this function to use it in future update sessions.
# ---------------------------------------------------------------------------

#' Add new site locations to site_config.csv with default delineation settings.
#'
#' Call this whenever new sample locations appear in the site data that don't yet
#' have a row in site_config.csv. New rows are appended with all defaults (snap=TRUE,
#' buffer_m=35, etc.). Review and edit the CSV before running delineation for any
#' site whose coordinates suggest it may need non-default settings.
#'
#' @param new_sites  sf or data frame with at least Latitude and Longitude columns.
#' @param config_path  path to site_config.csv; defaults to "data/site_config.csv".
#' @return the updated config data frame (invisibly); the CSV is rewritten in place.
add_new_locations <- function(new_sites, config_path = "data/site_config.csv") {
  existing <- read_csv(config_path, show_col_types = FALSE)

  if (inherits(new_sites, "sf")) new_sites <- st_drop_geometry(new_sites)

  new_coords <- new_sites %>%
    distinct(Latitude, Longitude) %>%
    anti_join(existing, by = c("Latitude", "Longitude"))

  if (nrow(new_coords) == 0) {
    message("No new locations to add — all sites are already in the config.")
    return(invisible(existing))
  }

  last_num <- max(as.integer(gsub("CONUS_", "", existing$location_id)), na.rm = TRUE)

  new_rows <- new_coords %>%
    mutate(
      location_id        = sprintf("CONUS_%04d", seq(last_num + 1, last_num + n())),
      snap               = TRUE,
      buffer_m           = 35L,
      split_count        = 50L,
      comid_override     = NA_integer_,
      extra_comid        = NA_integer_,
      error_watershed    = FALSE,
      snap_to_endpoint   = FALSE,
      label_off_network  = FALSE,
      nldi_from_flowline = FALSE,
      is_international   = FALSE,
      notes              = NA_character_
    )

  updated <- bind_rows(existing, new_rows)
  write_csv(updated, config_path)

  message(sprintf("Added %d new location(s) to %s", nrow(new_rows), config_path))
  message(paste(" ", new_rows$location_id, collapse = "\n"))
  message("Review their settings in site_config.csv before running delineation.")

  invisible(updated)
}
