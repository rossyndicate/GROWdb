

# ---- read data ----
grow_old <- read_csv(
  "data/archive/GROWdb_with_vars_20230519.csv",
  show_col_types = FALSE
)

grow_full <- readRDS("data/conus_watersheds.RDS") %>%
  mutate(AreaSqKmWs = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  #rename(comid_original = comid) %>%
  left_join(
    read_csv("data/geospatial_final.csv", show_col_types = FALSE) %>%
      select(-c(comid, watershed)),
    by = c("Latitude", "Longitude")
  ) %>%
  left_join(
    read_csv("data/flow_final.csv", show_col_types = FALSE) %>%
      select(-c(Date, Collection_Date, Latitude, Longitude)),
    by = "sample_name"
  ) %>%
  distinct()
  # calculate area-based land cover metrics from watershed area


# ---- original names ----
old_cols <- names(grow_old)
new_cols <- names(grow_full)

# ---- normalized names ----
old_norm <- normalize_names(old_cols)
new_norm <- normalize_names(new_cols)

# ---- crosswalk ----
old_lookup <- tibble(
  old_name = old_cols,
  norm = old_norm
)

new_lookup <- tibble(
  new_name = new_cols,
  norm = new_norm
)

column_crosswalk <- full_join(
  old_lookup,
  new_lookup,
  by = "norm"
) %>%
  arrange(norm)

column_crosswalk

# ---- identify missing/new columns AFTER normalization ----
missing_in_new <- old_lookup %>%
  filter(!norm %in% new_norm)

new_in_new <- new_lookup %>%
  filter(!norm %in% old_norm)

missing_in_new
new_in_new

# ---- rename grow_full columns to old names where matches exist ----
rename_vector <- column_crosswalk %>%
  filter(!is.na(old_name),
         !is.na(new_name),
         old_name != new_name) %>%
  select(old_name, new_name)

rename_list <- setNames(rename_vector$new_name,
                        rename_vector$old_name)

# rename syntax needs: new_name = old_name
# dplyr::rename() needs: old_name = new_name
# meaning: rename(grow_full, OldColumnName = current_new_column_name)

rename_list <- column_crosswalk %>%
  filter(
    !is.na(old_name),
    !is.na(new_name),
    old_name != new_name
  ) %>%
  distinct(old_name, new_name) %>%
  deframe()
# names(rename_list) = old names to create
# values(rename_list) = current grow_full names to rename

grow_full_renamed <- grow_full %>%
  rename(!!!rename_list)

# ---- add missing columns from grow_old ----
still_missing <- setdiff(names(grow_old),
                         names(grow_full_renamed))

for (cc in still_missing) {
  grow_full_renamed[[cc]] <- NA
}

# ---- reorder columns to match grow_old ----
grow_full_ordered <- grow_full_renamed %>%
  select(
    all_of(names(grow_old)),
    everything()
  ) %>%
  mutate(Date = Collection_Date,
         conus = TRUE) %>%
  select(-c(objectid, Collection_Date, CatPctFull, WsPctFull)) %>%
  select(conus, status = update, delineation_type = watershed, everything()) %>%
  relocate(gage_seasonal, .after = flow_record) %>%
  mutate(
    AreaSqKmImp2016Ws    = AreaSqKmWs * (PctImp2016Ws / 100),
    AreaSqKmOw2016Ws     = AreaSqKmWs * (PctOw2016Ws / 100),
    AreaSqKmIce2016Ws    = AreaSqKmWs * (PctIce2016Ws / 100),
    
    AreaSqKmUrbOp2016Ws  = AreaSqKmWs * (PctUrbOp2016Ws / 100),
    AreaSqKmUrbLo2016Ws  = AreaSqKmWs * (PctUrbLo2016Ws / 100),
    AreaSqKmUrbMd2016Ws  = AreaSqKmWs * (PctUrbMd2016Ws / 100),
    AreaSqKmUrbHi2016Ws  = AreaSqKmWs * (PctUrbHi2016Ws / 100),
    
    AreaSqKmBl2016Ws     = AreaSqKmWs * (PctBl2016Ws / 100),
    AreaSqKmDecid2016Ws  = AreaSqKmWs * (PctDecid2016Ws / 100),
    AreaSqKmConif2016Ws  = AreaSqKmWs * (PctConif2016Ws / 100),
    AreaSqKmMxFst2016Ws  = AreaSqKmWs * (PctMxFst2016Ws / 100),
    AreaSqKmShrb2016Ws   = AreaSqKmWs * (PctShrb2016Ws / 100),
    AreaSqKmGrs2016Ws    = AreaSqKmWs * (PctGrs2016Ws / 100),
    AreaSqKmHay2016Ws    = AreaSqKmWs * (PctHay2016Ws / 100),
    AreaSqKmCrop2016Ws   = AreaSqKmWs * (PctCrop2016Ws / 100),
    
    AreaSqKmWdWet2016Ws  = AreaSqKmWs * (PctWdWet2016Ws / 100),
    AreaSqKmHbWet2016Ws  = AreaSqKmWs * (PctHbWet2016Ws / 100)
  )


write_csv(grow_full_ordered,
          "data/grow_conus.csv")

missing_international <- old_grow %>%
  filter(!SampleName %in% grow_full_ordered$SampleName)

all_grow <- read_csv("data/date_lat_long_grow_KKA_05.05.26.csv") %>%
  filter(!sample_name %in% grow_full_ordered$SampleName) %>%
  mutate(status = ifelse(sample_name %in% grow_old$SampleName, "previous",
         ifelse(!sample_name %in% grow_old$SampleName, "new", NA))) %>%
  rename(SampleName = sample_name,
         Date = Collection_Date) %>%
  mutate(conus = FALSE)
  
final_grow <- grow_old %>%
  filter(!SampleName %in% grow_full_ordered$SampleName) %>%
  anti_join(., all_grow, by = "SampleName") %>%
  select(SampleName, Date, Latitude, Longitude) %>%
  mutate(conus = FALSE,
         status = "missing") %>%
  bind_rows(all_grow) %>%
  bind_rows(grow_full_ordered) %>%
  distinct()

write_csv(final_grow, "data/GROWdb_20260507.csv")


#495




library(data.table)

# Read data ---------------------------------------------------------------
new <- fread("GROWdb_20260507.csv")
old <- fread("GROWdb_with_vars_20230519(1).csv")

# Make sure SampleName exists --------------------------------------------
stopifnot("SampleName" %in% names(new))
stopifnot("SampleName" %in% names(old))

# Flag duplicate SampleNames in old/new ----------------------------------
new_dupes <- new[duplicated(SampleName) | duplicated(SampleName, fromLast = TRUE)]
old_dupes <- old[duplicated(SampleName) | duplicated(SampleName, fromLast = TRUE)]

# Keep one row per SampleName for comparison -----------------------------
new <- unique(new, by = "SampleName")
old <- unique(old, by = "SampleName")

# Identify comparable columns --------------------------------------------
common_cols <- intersect(names(new), names(old))

comid_cols <- common_cols[
  grepl("comid", common_cols, ignore.case = TRUE)
]

pct_ws_cols <- common_cols[
  grepl("^Pct.*Ws$", common_cols) |
    grepl("^percent.*ws$", common_cols, ignore.case = TRUE) |
    grepl("Pct.*Ws", common_cols)
]

streamflow_cols <- common_cols[
  grepl(
    "CFS|p20|p80|flow_record|flowlink|gage_seasonal|seasonal_20|seasonal_80|streamflow|discharge",
    common_cols,
    ignore.case = TRUE
  )
]

compare_cols <- unique(c(comid_cols, pct_ws_cols, streamflow_cols))

# Join new/old by SampleName ---------------------------------------------
cmp <- merge(
  new[, c("SampleName", compare_cols), with = FALSE],
  old[, c("SampleName", compare_cols), with = FALSE],
  by = "SampleName",
  all = TRUE,
  suffixes = c("_new", "_old")
)

cmp[, row_status := fifelse(
  SampleName %in% new$SampleName & SampleName %in% old$SampleName, "in_both",
  fifelse(SampleName %in% new$SampleName, "new_only", "old_only")
)]

# New / missing sites ----------------------------------------------------
new_sites <- new[!SampleName %in% old$SampleName, .(SampleName)]
missing_sites <- old[!SampleName %in% new$SampleName, .(SampleName)]

# COMID differences ------------------------------------------------------
comid_diff <- rbindlist(lapply(comid_cols, function(col) {
  new_col <- paste0(col, "_new")
  old_col <- paste0(col, "_old")
  
  cmp[
    row_status == "in_both" &
      fifelse(is.na(get(new_col)), "NA", as.character(get(new_col))) !=
      fifelse(is.na(get(old_col)), "NA", as.character(get(old_col))),
    .(
      SampleName,
      column = col,
      old_value = as.character(get(old_col)),
      new_value = as.character(get(new_col))
    )
  ]
}), fill = TRUE)

comid_summary <- comid_diff[
  , .N,
  by = column
][order(-N)]

# Numeric differences: order-of-magnitude only ---------------------------
numeric_cols <- unique(c(pct_ws_cols, streamflow_cols))

# Numeric differences: order-of-magnitude OR NA mismatches ---------------
numeric_diff <- rbindlist(lapply(numeric_cols, function(col) {
  new_col <- paste0(col, "_new")
  old_col <- paste0(col, "_old")
  
  tmp <- cmp[
    row_status == "in_both",
    .(
      SampleName,
      column = col,
      old_value = suppressWarnings(as.numeric(get(old_col))),
      new_value = suppressWarnings(as.numeric(get(new_col)))
    )
  ]
  
  tmp[, diff := new_value - old_value]
  tmp[, abs_diff := abs(diff)]
  
  # Flag NA mismatches
  tmp[, na_mismatch := xor(is.na(old_value), is.na(new_value))]
  
  # Ratio comparison
  tmp[, ratio := fifelse(
    !is.na(old_value) & !is.na(new_value) &
      old_value != 0 & new_value != 0,
    pmax(abs(new_value / old_value),
         abs(old_value / new_value)),
    NA_real_
  )]
  
  # Classification
  tmp[, difference_type := fifelse(
    na_mismatch,
    "NA_mismatch",
    fifelse(
      !is.na(ratio) & ratio >= 10,
      "order_of_magnitude",
      NA_character_
    )
  )]
  
  tmp[
    !is.na(difference_type),
    .(
      SampleName,
      column,
      old_value,
      new_value,
      diff,
      abs_diff,
      ratio,
      difference_type
    )
  ]
}), fill = TRUE)


numeric_summary <- numeric_diff[
  , .(
    n_order_magnitude_different = .N,
    mean_ratio = mean(ratio, na.rm = TRUE),
    median_ratio = median(ratio, na.rm = TRUE),
    max_ratio = max(ratio, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    median_abs_diff = median(abs_diff, na.rm = TRUE),
    max_abs_diff = max(abs_diff, na.rm = TRUE)
  ),
  by = column
][order(-n_order_magnitude_different, -max_ratio)]

# Optional: numeric differences involving zero ---------------------------
# These are separated because ratio/order-of-magnitude comparisons with zero
# can be misleading, but they may still be worth reviewing.
numeric_zero_diff <- rbindlist(lapply(numeric_cols, function(col) {
  new_col <- paste0(col, "_new")
  old_col <- paste0(col, "_old")
  
  tmp <- cmp[
    row_status == "in_both",
    .(
      SampleName,
      column = col,
      old_value = suppressWarnings(as.numeric(get(old_col))),
      new_value = suppressWarnings(as.numeric(get(new_col)))
    )
  ]
  
  tmp[, diff := new_value - old_value]
  tmp[, abs_diff := abs(diff)]
  
  tmp[
    !is.na(old_value) & !is.na(new_value) &
      old_value != new_value &
      (old_value == 0 | new_value == 0),
    .(
      SampleName,
      column,
      old_value,
      new_value,
      diff,
      abs_diff
    )
  ]
}), fill = TRUE)

# Overall summary --------------------------------------------------------
overall_summary <- data.table(
  new_rows = nrow(new),
  old_rows = nrow(old),
  shared_SampleName = sum(new$SampleName %in% old$SampleName),
  new_only_SampleName = nrow(new_sites),
  old_only_SampleName = nrow(missing_sites),
  new_duplicate_SampleName_rows = nrow(new_dupes),
  old_duplicate_SampleName_rows = nrow(old_dupes),
  comid_differences = nrow(comid_diff),
  order_magnitude_numeric_differences = nrow(numeric_diff),
  numeric_zero_differences = nrow(numeric_zero_diff)
)

# Print outputs ----------------------------------------------------------
overall_summary

comid_summary
comid_diff

numeric_summary
numeric_diff

numeric_zero_diff

new_sites
missing_sites

new_dupes
old_dupes

# Optional: write outputs ------------------------------------------------
fwrite(overall_summary, "comparison_overall_summary.csv")
fwrite(comid_summary, "comparison_comid_summary.csv")
fwrite(comid_diff, "comparison_comid_diff.csv")
fwrite(numeric_summary, "comparison_numeric_order_magnitude_summary.csv")
fwrite(numeric_diff, "comparison_numeric_order_magnitude_diff.csv")
fwrite(numeric_zero_diff, "comparison_numeric_zero_diff.csv")
fwrite(new_sites, "comparison_new_sites.csv")
fwrite(missing_sites, "comparison_missing_sites.csv")
