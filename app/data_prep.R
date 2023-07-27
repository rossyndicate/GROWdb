library(tidyverse)
library(sf)
library(nhdplusTools)
library(mapview)

# Pull in most recent sequencing metadata:
meta <- read_csv('data/in/sequencing_meta.csv') %>%
  mutate(MetaG_JGI_accession = as.character(MetaG_JGI_accession))


# Pull in what's on GitHub:
grow_non_na <- read_csv('data/out/GROWdb_with_vars_20230519.csv') %>%
  anti_join(meta, by = "SampleName")

grow_git <- read_csv('data/out/GROWdb_with_vars_20230519.csv') %>%
  filter(!SampleName %in% grow_non_na$SampleName) %>%
  select(1,4, 21:322) %>%
  full_join(meta) %>%
  bind_rows(grow_non_na) %>%
  mutate(MetaT = ifelse(has_MetaT == "Y", "MetaT", ""),
         MetaG = ifelse(grepl("3", MetaG_JGI_accession), "MetaG", ""),
         FTICR = ifelse(has_FTICR == "Y","FTICR","")) %>%
  mutate(AvailableMeta = trimws(paste0(MetaG, " ", MetaT, " ", FTICR))) %>%
  mutate(JGIlink = ifelse(MetaG == "MetaG", 
                          paste0("https://img.jgi.doe.gov/cgi-bin/mer/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=", MetaG_JGI_accession),
                          paste0(""))) %>%
  mutate(Country = ifelse(Country == "USA (Puerto Rico)", "USA", Country))

# Pull in most recent metabolism data:
metabolism <- read_csv('data/in/norm.counts.rpk_edger.bins_median.adj.csv') %>%
  # Per MB's reccomendations, these are the only ones we want on GROWdb
  select(SampleName = rn, Aerobe, Fermenter, Denitrifier, Phototroph) %>%
  mutate(SampleName = str_replace(SampleName, "[.]", "-"))

# Bind geospatial data and metabolism data together, prep for putting into shiny app...
grow_update <- grow_git %>%
  left_join(., metabolism, by = "SampleName") %>%
  #sf::st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  mutate(HUC2 = str_replace(huc2, "HUC-", ""),
         FlowConditions=ifelse(is.na(CFS), "No Flow Data",
                               ifelse(CFS <= percentile_20, "Low",
                                      ifelse(CFS >= percentile_80, "High",
                                             ifelse(CFS > percentile_20 & CFS < percentile_80, "Normal", "Weird")))),
         PctForest2016Ws = PctDecid2016Ws + PctConif2016Ws + PctMxFst2016Ws) %>%
  select(SampleName,
         Sample, 
         ProjectPI = Project_PI,
         Date,
         Time,
         Country,
         Latitude,
         Longitude,
         AvailableMeta,
         Notes,
         Aerobe,
         Fermenter, 
         Denitrifier, 
         Phototroph,
         JGIData = JGIlink,
         HUC2,
         COMID = comid,
         GNISName = gnis_name,
         StreamOrder = streamorde,
         WatershedAreaSqKm = totdasqkm,
         SlopeSite = slope,
         DamDensWs,
         DamNrmStorWs,
         MineDensWs,
         WWTPAllDensWs,
         WWTPMajorDensWs,
         PctForest2016Ws,
         PctImp2016Ws,
         PctCrop2016Ws,
         TmeanSite,
         TmeanWs,
         PrecipSite,
         PrecipWs,
         AridityIndexSite = AriditySite,
         AridityIndexWs = AridityWs,
         OmernikISite,
         OmernikIISite,
         OmernikIIISite,
         GageID = gage_id,
         FlowData = flowlink,
         FlowRecord = flow_record,
         DischargeCFS = CFS,
         FlowConditions)

saveRDS(grow_update, 'app/data/GROWDb.RDS')

contus <- grow_update %>%
  filter(!is.na(COMID)) %>%
  saveRDS('app/data/GROWdb_CONTUS.RDS')

