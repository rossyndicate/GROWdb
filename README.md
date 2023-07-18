# Genome Resolved Open Watersheads Database (GROWdb) Geospatial Data Puller

This repository houses the workflow for pulling geospatial data associated with GROWdb sample locations. It also houses the back-end code for the [GROWdb visualizing app](https://geocentroid.shinyapps.io/GROWdatabase/).

## [Methods]{.underline}

The watershed statistics for each sample were primarily obtained from the Environmental Protection Agency's StreamCat database (Hill et al., 2015) and the National Hydrography Plus Version 2 (NHDPlus V2) Dataset using the nhdplusTools package (Blodgett & Johnson, 2023) in R (R Core Team, 2023). StreamCat provides over 600 consistently computed watershed metrics for all waterbodies identified in the US Geological Survey (USGS)'s NHDPlusV2 geospatial framework, making it a suitable data source for the broad spectrum of sample locations in this study.

For watershed metrics that were not included in StreamCat (i.e., dominant Omernik Ecoregion, mean net primary production, and mean aridity index), we first delineated each sample's watershed using nhdplusTools, then utilized the terra package (Hijmans et al., 2022) to aggregate the additional datasets across each site's watershed accordingly. This approach is consistent with SteamCat's geospatial methodology.

Lastly, we collected streamflow data for sites that had a nearby stream gage. For locations without an identified co-located stream gage (WHONDRS typically co-located their sample sites with a stream gage), we identified USGS stream gages within 10 kilometers up- or downstream of our sampling locations using the dataRetrieval and nhdplusTools packages. All stream gages were then manually verified for their applicability to each sampling site (e.g., verifying there were no dams between the site and the stream gage, a major confluence, etc.). See Table 1 for a complete list of data sets included in this analysis.

| Dataset                                           | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|-------------------------|-----------------------------------------------|
| StreamCat Geospatial Dataset (Hill et al., 2015)  | Dams (Ostroff et al., 2013); fire perimeters and forest loss (USGS Sciencebase, 2015; Walters et al., 2015); soil characteristics (Carlisle et al., 2009); geochemical and geophysical characteristics (Hill et al., 2013; Olson & Hawkins, 2014); landcover (Dewitz , 2021); mines (USGS National Coal Resources Data System, 2015); climate (PRISM Climate Group, 2015; National Atmospheric Deposition Program, 2016); and anthropogenic/socioeconomic traits (US Census Bureau, 2014; Penino et al., 2020).     |
| NHDPlus V2 (McKay et al., 2012)                   | Stream characteristics and digital elevation model rasters.                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Additional Geospatial Datasets                    | Omernik ecoregions (Omernik & Griffith, 2014); net primary production (Running & Shao, 2019); and aridity index (Zomer et al., 2007; Zomer et al., 2008).                                                                                                                                                                                                                                                                                                                                                           |
| Streamflow Datasets                               | National Water Information System (USGS, 2021); Colorado Department of Water Resources (2020); Oregon Water Resources Department (2020); Oak Ridge National Laboratory (Brooks et al., 2019; Riscassi et al., 2019; Brooks et al., 2021); Long Term Ecological Research Network (USFS Northern Research Station, 2020); and the National Ecological Observatory Network (2022).                                                                                                                                     |

: Table 1: A complete list of data sets included in our analysis.

## [Works Cited]{.underline}

Blodgett D. and J. M. Johnson (2023). nhdplusTools: Tools for accessing and working with the NHDPlus. doi: 10.5066/P97AS8JD.

Brooks S. C. and K. A. Lowe (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2016. doi: 10.12769/1489830.

Brooks S. C. and K. A. Lowe (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2017. doi: 10.12769/1489831.

Brooks S. C. and K. A. Lowe (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2018. doi: 10.12769/1489832.

Brooks S. C. and K. A. Lowe (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2019. doi: 10.12769/1569761.

Brooks S. C. and K. A. Lowe (2021). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2020. doi: 10.12769/1779632.

Carlisle D. M., Falcone J., and M. R. Meador (2009). Predicting the biological condition of streams: Use of geopatial indicators of natural and anthropogenic characteristics of watersheds. *Environmental Monitoring and Assessment,* 151: 143-160. doi: 10.1007/s10661-008-0256-z.

DeCicco L. A., Hirsch R. M., Watkins W., and M. Johnson (2023). dataRetrieval: R packages for discovering and retrieving water data available from U.S. federal hydrologic web services. doi: 10.5066/P9X4L3GE.

Hijmans R. J., Bivand R., Pebesma E., and M. D. Sumner (2023). terra: Spatial data analysis. url: https://CRAN.R-project.org/package=terra.

Hill R. A., Hawkins C. P., and D. M. Carlisle (2013). Predicting thermal reference conditions for USA streams and rivers. *Freshwater Science*, 32(1): 39-55. doi: 10.1899/12-009.1.

Hill R. A., Weber M. H., Leibowitz S. G., Olsen A. R., and D. J. Thornbrugh (2015). The Stream-Catchment (StreamCat) dataset: A database of watershed metrics for the conterminous United States. *Journal of the American Water Resources Association,* 52(1): 120-128, doi: 10.1111/1752-1688.12372.

McKay L., Bondelid T., Dewald T., Johnston J., Moore R., and A. Rhea (2012). NHDPlus Version 2: User Guide. url: https://www.epa.gov/system/files/documents/2023-04/NHDPlusV2_User_Guide.pdf.

National Ecological Observatory Network (2022). Continuous Discharge (DP4.00130.001). doi: 10.48443/xz4k-5j04.

Olson J. R. and C. P. Hawkins (2014). Geochemical Characteristics of the Conterminous United States: U.S. Geological Survey data release. doi: 10.5066/F7X0653P.

Omernik J. M. and G. E. Griffith (2014). Ecoregions of the conterminous United States: evolution of a hierarchical spatial framework. *Environmental Management,* 54(6): 1249-1266. doi: 10.1007/s00267-014-0364-1.

Oregon Water Resources Department (2021). Hydrographics Data Access and Summary Statistics. url: https://apps.wrd.state.or.us/apps/sw/hydro_report/.

Ostroff A., Wieferich D., Cooper A., and D. Infante (2013). 2012 National Anthropogenic Barrier Dataset (NABD). National Fish Habitat Partnership Data System. doi: 10.5066/F7VX0DFG.

Pennino M. J., Leibowitz S. G., Compton J. E., Hill R. A., and R. D. Sabo (2020). Patterns and predictions of drinking water nitrate violations across the conterminous United States. *Science of the Total Environment,* 722: 137661. doi: 10.1016/j.scitotenv.2020.137661.

PRISM Climate Group, Oregon State University (2021). url: https://prism.oregonstate.edu.

R Core Team (2023). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. url: https://www.R-project.org/.

Riscassi A. L. and S. C. Brooks (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2012. doi: 10.12769/1489524.

Riscassi A. L. and S. C. Brooks (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2013. doi: 10.12769/1490223.

Riscassi A. L. and S. C. Brooks (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2014. doi: 10.12769/1489825. 

Riscassi A. L., Lowe K. A., and S. C. Brooks (2019). East Fork Poplar Creek Discharge at Kilometer 5.4 Water Year 2015. doi: 10.12769/1489828.

Running S. and M. Zhao (2019). *MOD17A3HGF MODIS/Terra Net Primary Production Gap-Filled Yearly L4 Global 500 m SIN Grid V006*. NASA EOSDIS Land Processes DAAC. doi: 10.5067/MODIS/MOD17A3HGF.006.

USDA Forest Service, Northern Research Station (2020). Hubbard Brook Experimental Forest: Daily Streamflow by Watershed, 1956 - present. Environmental Data Initiative. doi: 10.6073/pasta/5286c6e92dd9585d732590726937f5c9.

US Geological Survey (USGS) (2020). National Water Information System: Water data for the Nation. url: http://waterdata.usgs.gov/nwis/.

Walters S. P., Schneider N. J., and J. D. Guthrie (2010). Geospatial Multi-Agency Coordination (GeoMAC) Wildland Fire Perimeters. url: https://www.sciencebase.gov/catalog/folder/4f4e4767e4b07f02db47e0af.

Zomer R. J., Trabucco A., Bossio D. A., van Straaten O., and L. V. Verchot (2008). Climate change mitigation: A spatial analysis of global land suitability for clean development mechanism afforestation and reforestation. *Agriculture, Ecosystems & Environment,* 126(1-2): 67-80. doi: 10.1016/j.agee.2008.01.014.

Zomer R. J., Bossio D. A., Trabucco A., Yuanjie L., Gupta D. C. and V. P. Singh (2007). Trees and water: Smallholder agroforestry on irrigated lands in Northern India. Colombo, Sri Lanka: International Water Management Institute. pp 45. (IWMI Research Report 122). doi: 10.3910/2009.122.
