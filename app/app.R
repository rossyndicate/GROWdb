library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(shinythemes)
library(markdown)
library(bslib)
library(DT)
library(shinyWidgets)
library(plotly)
library(shinyBS)

huc2 <- readRDS('data/huc2.RDS') %>% st_transform(crs=4326)

jgi_full_dataset <- readRDS('data/GROWdb.RDS') %>%
  select(1:13, 15)

jgi <- readRDS('data/GROWdb.RDS') %>% filter(!is.na(COMID)) 

jgi_spatial_1 <- jgi_full_dataset %>%
  st_as_sf(coords = c('Longitude', 'Latitude'),
           crs = 4326,
           remove = F)

jgi_spatial <- jgi %>%
  st_as_sf(coords = c('Longitude', 'Latitude'),
           crs = 4326,
           remove = F)

#meta <- readRDS('app/data/geospatial_variable_descriptions.RDS')

# Change the color of dropdowns
col.list <- c('#9E0243','#C02C4A','#DE5B5C','#F07555','#F98F52',
              '#FCB66C','#FED47F','#FFEC9F','#FFFFBF','#EFF8AA',
              '#DBEFA3','#B3E0A3','#8BCFA5','#62BBA8','#4596B7',
              '#4B73B3','#5E4FA1')

colors <- paste0("color:",col.list,";")

country_colors <- paste0("color:",c('#4B73B3', "#8BCFA5","#C02C4A", '#FED47F','#F07555',"#F98F52","#B3E0A3",'#DBEFA3'),";")

### USER INTERFACE ###

ui <- navbarPage("GROW Database Explorer",
                 theme = bslib::bs_theme(
                   bootswatch = "flatly",
                   #bg = "#FFFFFF",
                   #fg = "#000",
                   primary = "#0072B2",
                   secondary = "#EC7031",
                   success = "#EC7031",
                   base_font = font_google("Kanit")
                 ),
                 tags$head(tags$style(HTML(
                   ".selectize-dropdown-menu a[data-value=\"01\"]{color:#9E0243}
                    .selectize-dropdown-menu a[data-value=\"02\"]{color:#C02C4A}
                    .selectize-dropdown-menu a[data-value=\"03\"]{color:#DE5B5C}
                    .selectize-dropdown-menu a[data-value=\"04\"]{color:#F07555}
                    .selectize-dropdown-menu a[data-value=\"05\"]{color:#F98F52}
                    .selectize-dropdown-menu a[data-value=\"06\"]{color:#FCB66C}
                    .selectize-dropdown-menu a[data-value=\"07\"]{color:#FED47F}
                    .selectize-dropdown-menu a[data-value=\"08\"]{color:#FFEC9F}
                    .selectize-dropdown-menu a[data-value=\"All\"]{color:black}
                    .selectize-dropdown-menu a[data-value=\"10\"]{color:#FFFFBF}
                    .selectize-dropdown-menu a[data-value=\"11\"]{color:#EFF8AA}
                    .selectize-dropdown-menu a[data-value=\"12\"]{color:#DBEFA3}
                    .selectize-dropdown-menu a[data-value=\"13\"]{color:#B3E0A3}
                    .selectize-dropdown-menu a[data-value=\"14\"]{color:#8BCFA5}
                    .selectize-dropdown-menu a[data-value=\"15\"]{color:#62BBA8}
                    .selectize-dropdown-menu a[data-value=\"16\"]{color:#4596B7}
                    .selectize-dropdown-menu a[data-value=\"17\"]{color:#4B73B3}
                    .selectize-dropdown-menu a[data-value=\"18\"]{color:#5E4FA1}"))),
                 #tabPanel("GROW database",
                 #         fluidPage("Blah blah blah")),
                 tabPanel("Full Dataset",
                          fluidPage(
                            
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(class = "filters",
                                         
                                         # Omics Check Box.
                                         column(
                                           12,
                                           checkboxGroupInput(
                                             "Group_1",
                                             "Available Data",
                                             choices = c("MetaG","MetaT","FTICR"),
                                             selected = "MetaG"))),
                                
                                
                                # Country Selector.
                                fluidRow(class = "filters",
                                         column(
                                           12,
                                           pickerInput(
                                             inputId = "Country_1",
                                             label = "Countries", 
                                             choices = c(unique(jgi_full_dataset$Country)),#c("USA","UK","Canada","Italy","Germany","Israel","South Korea","Republic of Congo"), #%>% sort(),
                                             selected = c(unique(jgi_full_dataset$Country)),#c("USA","UK","Canada","Italy","Germany","Israel","South Korea","Republic of Congo"),
                                             multiple = TRUE,
                                             options = list('actions-box' = TRUE),
                                             choicesOpt = list(style = country_colors) # <- NEW
                                           ))), 
                                
                                downloadButton("downloadData_1", "Download Selected Data")),
                              
                              mainPanel(
                                
                                tabsetPanel(id="nav_1",
                                            tabPanel(title = "Map the Data", class="map", leafletOutput("map_1", width = "100%", height = 380))))),
                            
                            fluidRow(class = "table",
                                     # Table
                                     dataTableOutput("table_1")))),
                 
                 tabPanel("Explore Geospatial (CONUS only)",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(class = "filters",
                                         
                                         h6("Select samples based on their stream and watershed characteristics:", class = "title"),
                                         
                                         tags$br(),
                                         
                                         # Omics Check Box.
                                         column(
                                           6,
                                           checkboxGroupInput(
                                             "Group",
                                             "Available Data",
                                             choices = c("MetaG","MetaT","FTICR"),
                                             selected = "MetaG")),
                                         
                                         # "Flow" Check Box.
                                         column(
                                           6,
                                           checkboxGroupInput(
                                             "flow",
                                             "Flow Conditions",
                                             choices = c("High","Low","Normal","No Flow Data"),
                                             selected = c("High","Low","Normal","No Flow Data"))),
                                         
                                         # HUC-2 Selector.
                                         column(
                                           6,
                                           pickerInput(
                                             inputId = "huc2",
                                             label  = "HUC-02",
                                             choices = c('01','02','03','04','05','06','07','08',
                                                         '10','11','12','13','14','15','16','17',
                                                         '18'),
                                             selected = c('01','02','03','04','05','06','07','08',
                                                          '10','11','12','13','14','15','16','17',
                                                          '18'),
                                             options = list('actions-box' = TRUE),
                                             multiple = TRUE,
                                             choicesOpt = list(style = colors)
                                           )),
                                         
                                         # Watershed Size Selector.
                                         column(
                                           6,
                                           sliderTextInput(
                                             inputId = "area",
                                             label = "Watershed Size (sqkm)",
                                             grid = TRUE,
                                             force_edges = TRUE,
                                             choices = c("0","10","100","1000","10000","100000","1000000",'3500000'),
                                             selected = c("0","3500000")
                                           ))),
                                
                                fluidRow(class = "filters",
                                         
                                         # Stream Order Slider.
                                         column(
                                           6,
                                           sliderInput(
                                             "stream_order",
                                             "Stream Order",
                                             min = min(jgi$StreamOrder),
                                             max = max(jgi$StreamOrder),
                                             step = 1,
                                             sep = "",
                                             value = c(min(jgi$StreamOrder), max(jgi$StreamOrder)),
                                             round = T)),
                                         
                                         # Percent Cropland Slider.
                                         column(
                                           6,
                                           sliderInput(
                                             "crop",
                                             "Percent Cropland",
                                             min = 0,
                                             max = 100,
                                             step = 1,
                                             sep = "",
                                             value = c(0,100),
                                             round = T)),
                                         
                                         # Percent Forested Slider.
                                         column(
                                           6,
                                           sliderInput(
                                             "forest",
                                             "Percent Forested",
                                             min = 0,
                                             max = 100,
                                             step = 1,
                                             sep = "",
                                             value = c(0,100),
                                             round = T)),
                                         
                                         # % Imperviousness Slider.
                                         column(
                                           6,
                                           sliderInput(
                                             "impervious",
                                             "Percent Imperviousness",
                                             min = 0,
                                             max = 100,
                                             step = 1,
                                             sep = "",
                                             value = c(0, 100),
                                             round = T))),
                                
                                downloadButton("downloadData", "Download Selected Data")),
                              
                              mainPanel(tabsetPanel(id="nav",
                                                    tabPanel(title = "Map the Data", class="map", leafletOutput("map", width = "100%", height = 600)),
                                                    tabPanel(title = "Plot Geospatial Data",
                                                             fluidRow(class = "filters",
                                                                      
                                                                      column(4,selectInput("x","x-Axis",
                                                                                           choices = c(
                                                                                             
                                                                                             "DischargeCFS",
                                                                                             
                                                                                             "StreamOrder",
                                                                                             "WatershedAreaSqKm",
                                                                                             
                                                                                             "DamDensWs",
                                                                                             "DamNrmStorWs",
                                                                                             "MineDensWs",
                                                                                             "WWTPAllDensWs",
                                                                                             "WWTPMajorDensWs",
                                                                                             
                                                                                             "PctForest2016Ws",
                                                                                             "PctImp2016Ws",
                                                                                             "PctCrop2016Ws",
                                                                                             
                                                                                             "TmeanWs",
                                                                                             "PrecipWs",
                                                                                             "AridityIndexWs",
                                                                                             "AridityIndexSite"
                                                                                             
                                                                                           ) %>% sort(),
                                                                                           
                                                                                           selected = 'StreamOrder')),
                                                                      
                                                                      column(4,selectInput("y","y-Axis",
                                                                                           choices = c(
                                                                                             
                                                                                             "DischargeCFS",
                                                                                             
                                                                                             "StreamOrder",
                                                                                             "WatershedAreaSqKm",
                                                                                             
                                                                                             "DamDensWs",
                                                                                             "DamNrmStorWs",
                                                                                             "MineDensWs",
                                                                                             "WWTPAllDensWs",
                                                                                             "WWTPMajorDensWs",
                                                                                             
                                                                                             "PctForest2016Ws",
                                                                                             "PctImp2016Ws",
                                                                                             "PctCrop2016Ws",
                                                                                             
                                                                                             "TmeanWs",
                                                                                             "PrecipWs",
                                                                                             "AridityIndexWs",
                                                                                             "AridityIndexSite"
                                                                                             
                                                                                           ) %>% sort(),
                                                                                           
                                                                                           selected = "WatershedAreaSqKm")),
                                                                      
                                                                      column(4,selectInput("z","Color",
                                                                                           choices = c(
                                                                                             
                                                                                             "DischargeCFS",
                                                                                             
                                                                                             "StreamOrder",
                                                                                             "WatershedAreaSqKm",
                                                                                             
                                                                                             "DamDensWs",
                                                                                             "DamNrmStorWs",
                                                                                             "MineDensWs",
                                                                                             "WWTPAllDensWs",
                                                                                             "WWTPMajorDensWs",
                                                                                             
                                                                                             "PctForest2016Ws",
                                                                                             "PctImp2016Ws",
                                                                                             "PctCrop2016Ws",
                                                                                             
                                                                                             "TmeanWs",
                                                                                             "PrecipWs",
                                                                                             "AridityIndexWs",
                                                                                             "AridityIndexSite"
                                                                                             
                                                                                           ) %>% sort(),
                                                                                           
                                                                                           selected = "WatershedAreaSqKm"))),
                                                             
                                                             #fluidRow(girafeOutput("geo_plot", width = "100%", height = 600))),
                                                             fluidRow(plotlyOutput("geo_plot", width = "100%", height = 600))),
                                                    tabPanel(title = "Plot MetaT Data",
                                                             fluidRow(class = "filters",
                                                                      
                                                                      column(4,selectInput("ex","x-Axis",
                                                                                           choices = c(
                                                                                             
                                                                                             "DischargeCFS",
                                                                                             
                                                                                             "StreamOrder",
                                                                                             "WatershedAreaSqKm",
                                                                                             
                                                                                             "DamDensWs",
                                                                                             "DamNrmStorWs",
                                                                                             "MineDensWs",
                                                                                             "WWTPAllDensWs",
                                                                                             "WWTPMajorDensWs",
                                                                                             
                                                                                             "PctForest2016Ws",
                                                                                             "PctImp2016Ws",
                                                                                             "PctCrop2016Ws",
                                                                                             
                                                                                             "TmeanWs",
                                                                                             "PrecipWs",
                                                                                             "AridityIndexWs",
                                                                                             "AridityIndexSite"
                                                                                             
                                                                                           ) %>% sort(),
                                                                                           
                                                                                           selected = "AridityIndexSite"
                                                                                           
                                                                      )),
                                                                      
                                                                      column(4,selectInput("why","y-Axis",
                                                                                           choices = c(#"Autotroph",
                                                                                             #"Heterotroph",
                                                                                             #"SulfateReducer",
                                                                                             #"Homoacetogen",
                                                                                             #"Methanogen",
                                                                                             #"DenitrifierNonETC",
                                                                                             #"AerobicAmmoniaOxidizer",
                                                                                             #"Comammox",
                                                                                             #"Annamox",
                                                                                             #"NitrogenFixer",
                                                                                             "Phototroph",
                                                                                             #"Fermenter",
                                                                                             "Aerobe",
                                                                                             #"Microaerophillic",
                                                                                             #"OxidaseNonETC",
                                                                                             "Denitrifier"
                                                                                             
                                                                                           ) %>% sort(),
                                                                                           
                                                                                           selected = "Aerobe")),
                                                                      
                                                                      column(4,selectInput("zee","Color",
                                                                                           choices = c(
                                                                                             
                                                                                             "DischargeCFS",
                                                                                             
                                                                                             "StreamOrder",
                                                                                             "WatershedAreaSqKm",
                                                                                             
                                                                                             "DamDensWs",
                                                                                             "DamNrmStorWs",
                                                                                             "MineDensWs",
                                                                                             "WWTPAllDensWs",
                                                                                             "WWTPMajorDensWs",
                                                                                             
                                                                                             "PctForest2016Ws",
                                                                                             "PctImp2016Ws",
                                                                                             "PctCrop2016Ws",
                                                                                             
                                                                                             "TmeanWs",
                                                                                             "PrecipWs",
                                                                                             "AridityIndexWs",
                                                                                             "AridityIndexSite"
                                                                                             
                                                                                           ) %>% sort(),
                                                                                           
                                                                                           selected = "WatershedAreaSqKm"))),
                                                             
                                                             
                                                             
                                                             
                                                             fluidRow(plotlyOutput("meta_plot", width = "100%", height = 600)))))),
                            
                            fluidRow(class = "table",
                                     # Table
                                     dataTableOutput("table")))))


### SERVER ###

server <- function(input, output, session) {
  
  filtered_data_1 <- reactive({
    
    # Filter data based on Country
    if(!isTruthy(input$Country_1)){jgi_full_dataset<-dplyr::filter(jgi_full_dataset,SampleName=="Bogus")
    }else{
      jgi_full_dataset <- dplyr::filter(jgi_full_dataset, Country %in% c(input$Country_1))
      #jgi_full_dataset <- dplyr::filter(jgi_full_dataset, grepl(paste((input$Country_1),collapse="|"), Country))
    }
    
    # Filter data based on selected omics data.
    if(!isTruthy(input$Group_1)){jgi_full_dataset<-filter(jgi_full_dataset,SampleName=="Bogus")
    }else{
      jgi_full_dataset <- filter(jgi_full_dataset,grepl(paste0("(?=.*",paste((input$Group_1),collapse=")(?=.*"),")"), AvailableMeta, perl=T))
    }
    
    jgi_full_dataset
    
  })
  
  output$map_1 <- renderLeaflet({
    
    validate (need(isTruthy(filtered_data_1()), ""))
    
    # Basemap.
    map_1 <- leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = min(jgi_spatial_1$Longitude),
        lat1 = min(jgi_spatial_1$Latitude),
        lng2 = max(jgi_spatial_1$Longitude),
        lat2 = max(jgi_spatial_1$Latitude))})
  
  observe({
    
    input$nav_1
    
    validate (need(nrow(filtered_data_1()) > 0, message="No samples selected."))
    
    pal <- colorFactor(palette = "Spectral", jgi_full_dataset$Country)
    
    # Add selected data to map.
    leafletProxy("map_1") %>%
      addCircleMarkers(
        data = filtered_data_1(),
        lng = ~Longitude,
        lat = ~Latitude,
        popup = paste(filtered_data_1()$SampleName,
                      "<br>",
                      filtered_data_1()$Country),
        radius = 5,
        fill = TRUE,
        color = "black",
        weight = 1,
        opacity = 0.5,
        fillColor = ~ pal(Country),
        fillOpacity = 1)
    
  })
  
  
  output$table_1 <- DT::renderDataTable({
    validate (need(nrow(filtered_data_1()) > 0, message="No samples selected."))
    tableee <- filtered_data_1()
    tableee$JGIData <- paste0('<a  target=_blank href=', tableee$JGIData, '>', tableee$JGIData,'</a>' )
    DT::datatable(tableee , escape = FALSE, options = list(autoWidth=TRUE, scrollX=TRUE, scrollY = "400px", scrollCollapse = TRUE))},
    options = list(autoWidth=TRUE, scrollX=TRUE, scrollY = "400px", scrollCollapse = TRUE))
  
  # Create a .csv of selected data for download.
  output$downloadData_1 <- downloadHandler(
    filename = "GROWdb_selected_sites.csv",
    content = function(file) {
      write_csv(filtered_data_1(),file)})
  
  # CONTUS ONLY  
  filtered_data <- reactive({
    
    # Filter data based on percent cropland.
    jgi <- filter(jgi,
                  PctCrop2016Ws >= input$crop[1] &
                    PctCrop2016Ws <= input$crop[2])
    
    # Filter data based on percent forested.
    jgi <- filter(jgi,
                  PctForest2016Ws >= input$forest[1] &
                    PctForest2016Ws <= input$forest[2])
    
    # Filter data based on selected stream order(s).
    jgi <- filter(jgi,
                  StreamOrder >= input$stream_order[1] &
                    StreamOrder <= input$stream_order[2])
    
    # Filter data based on HUC-02
    if(!isTruthy(input$huc2)){jgi<-filter(jgi,SampleName=="Bogus")
    }else{
      jgi<- filter(jgi, HUC2 %in% c(input$huc2))}
    
    # Filter data based on percent imperviousness.
    jgi <- filter(jgi,
                  PctImp2016Ws >= input$impervious[1] &
                    PctImp2016Ws <= input$impervious[2])
    
    # Filter data based on watershed area.
    jgi <- filter(jgi,
                  WatershedAreaSqKm >= as.numeric(input$area[1]) &
                    WatershedAreaSqKm <= as.numeric(input$area[2]))
    
    # Filter data based on selected flow requirements.
    if(!isTruthy(input$flow)){jgi<-filter(jgi,SampleName=="Bogus")
    }else{
      jgi <- filter(jgi,grepl(paste(simplify(input$flow), collapse="|"), FlowConditions))
    }
    
    # Filter data based on selected omics data.
    if(!isTruthy(input$Group)){jgi<-filter(jgi,SampleName=="Bogus")
    }else{
      jgi <- filter(jgi,grepl(paste0("(?=.*",paste((input$Group),collapse=")(?=.*"),")"), AvailableMeta, perl=T))
    }
    
    jgi
    
  })
  
  #Filter polygon based on selected HUC-02
  filtered_huc2 <- reactive({
    if(!isTruthy(input$huc2)){jgi<-filter(huc2,HUC2="666")
    }else{
      huc2 <- filter(huc2, HUC2 %in% c(input$huc2))} 
    huc2
  })
  
  output$map <- renderLeaflet({
    
    validate (need(isTruthy(filtered_data()), ""))
    
    # Basemap.
    map <- leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = min(jgi_spatial$Longitude),
        lat1 = min(jgi_spatial$Latitude),
        lng2 = max(jgi_spatial$Longitude),
        lat2 = max(jgi_spatial$Latitude))})
  
  observe({
    
    input$nav
    
    validate (need(nrow(filtered_data()) > 0, message="No samples selected."))
    
    pal <- colorFactor(palette = "Spectral", jgi$HUC2) 
    
    #Add selected data to map.
    tab1 <- leafletProxy("map") %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>%
      addPolygons(data = filtered_huc2(),
                  color = ~ pal(HUC2),
                  smoothFactor = 0.5,
                  weight = 3.5,
                  fillOpacity = 0.09,
                  fillColor = ~ pal(HUC2)) %>%
      addCircleMarkers(
        data = filtered_data(),
        lng = ~Longitude,
        lat = ~Latitude,
        popup = paste(filtered_data()$SampleName,
                      "<br>",
                      "HUC2: ", filtered_data()$HUC2),
        radius = 5,
        fill = TRUE,
        color = "black",
        weight = 1,
        opacity = 0.5,
        fillColor = ~pal(HUC2),
        fillOpacity = 0.90)
    
    output$geo_plot <- renderPlotly({
      
      plotly::plot_ly(data=filtered_data(), x=~get(input$x), y=~get(input$y), type='scatter', mode='markers',
                      color= ~get(input$z), colors=col.list) %>%
        plotly::layout(yaxis = list(title = input$y),
                       xaxis = list(title = input$x)) %>%
        colorbar(title=input$z)
    })
    
    output$meta_plot <- renderPlotly({
      
      plotly::plot_ly(data=filtered_data(), x=~get(input$ex), y=~get(input$why), type='scatter', mode='markers',
                      color= ~get(input$zee), colors=col.list) %>%
        plotly::layout(yaxis = list(title = paste0(input$why, ' Taxa Abundance')),
                       xaxis = list(title = input$ex)) %>%
        colorbar(title=input$zee)
    })
    
    
  })
  
  # Create data table of selected features.
  output$table <- DT::renderDataTable({
    validate (need(nrow(filtered_data()) > 0, message="No samples selected."))
    tablee <- select(filtered_data(), -FlowConditions)
    tablee$FlowData <- paste0('<a  target=_blank href=', tablee$FlowData, '>', tablee$FlowData,'</a>' )
    tablee$JGIData <- paste0('<a  target=_blank href=', tablee$JGIData, '>', tablee$JGIData,'</a>' )
    DT::datatable(tablee , escape = FALSE, options = list(autoWidth=TRUE, scrollX=TRUE, scrollY = "200px", scrollCollapse = TRUE))},
    options = list(autoWidth=TRUE, scrollX=TRUE,scrollX=TRUE, scrollY = "200px", scrollCollapse = TRUE))
  
  # Create a .csv of selected data for download.
  output$downloadData <- downloadHandler(
    filename = "GROWdb_selected_sites.csv",
    content = function(file) {
      write_csv(filtered_data(),file)})
}

### Run the app. ###
shinyApp(ui = ui, server = server)
