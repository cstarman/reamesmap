library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)
library(shinyWidgets)

data <- readxl::read_excel("GreekData.xlsx")
dataTwo <- readxl::read_excel("ReamesDataTwo.xlsx")
dataThree <- readxl::read_excel("ReamesDataThree.xlsx")
names(data) <- c("Name", "NameBase", "Popularity", "Location", "Lat/Long", "Latitude", "Longitude", "Date Range", "Date", "DateNum", "Region", "URL")
names(dataTwo) <- c("NameT", "PopularityT", "LocationT", "Lat/LongT", "Latitude", "Longitude", "Date RangeT", "DateT", "DateNumT", "RegionT", "URLT")
val <- c("Attic", "Doric")
col <- colorNumeric(c("#d13c3c", "#5f50e5"), 1:1)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100%}, .irs-grid-text{color: black, stroke: 2}" ),
  tags$head(includeCSS("styles.css")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(h3("The Case of Hephaistion"),
                id = "controls", class = "panel panel-default", top = 10, right = 10,
                fixed = TRUE, draggable = FALSE, width = 330, height = "auto",
                
                # Histogram 
                # plotOutput("histCentile", height = 200),
                # plotOutput("lineTrend", height = 140),
                h4 ("Attic-Ionic"),
                # Region filters 
                selectInput("Region", "Region:", choices = NULL),
                selectInput("Name", "Name:", choices = NULL),
                
                h4("Doric-Aeolic"),
                
                selectInput("RegionT", "Region:", choices = NULL),
                selectInput("NameT", "Name:", choices = NULL),
                
                radioButtons("radio", label = h3("Cluster Options"),
                             choices = list("Regular Color" = 1, "Regular Color, No Cluster" = 2, "Two Tone, No Cluster" = 3), 
                             selected = 1)
                
                # 
                # tags$p(tags$small(includeHTML("attr.html")))
                
  ),
  absolutePanel(h3("Timeline"),
      id = "time", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
      width = 350, height = "auto", top = 10, left = 50, align = "center", 
      sliderTextInput(
        inputId = "timeline", 
        label = "Years BCE", 
        grid = TRUE, 
        force_edges = TRUE,
        animate = TRUE,
        post = " BC",
        choices = c(600, 500, 400, 300, 200, 100)
      )
  )
)

server <- function(input, output, session) {
  
  region_list <- data$Region
  names(region_list) <- region_list
  regionChoice <- c("All", "None", region_list)
  updateSelectInput(session, "Region", choices = regionChoice)
  
  name_list <- data$Name
  names(name_list) <- name_list
  nameChoice <- c("All", name_list)
  updateSelectInput(session, "Name", choices = nameChoice)
  
  region_listT <- dataTwo$RegionT
  names(region_listT) <- region_listT
  regionChoiceT <- c("All", "None", region_listT)
  updateSelectInput(session, "RegionT", choices = regionChoiceT)
  
  name_listT <- dataTwo$NameT
  names(name_listT) <- name_listT
  nameChoiceT <- c("All", name_listT)
  updateSelectInput(session, "NameT", choices = nameChoiceT)
  
  updateSliderTextInput(session, "DateNum")
  
  pallete <- brewer.pal(8, "Set1")
  
  palleteTwo <- brewer.pal(9, "Set1")
  
  colorpal <- reactive({
    colorFactor(pallete, data$Name)
  })
  
  colorpalTwo <- reactive({
    colorFactor(palleteTwo, dataTwo$NameT)
  })
  

  
  output$map <- renderLeaflet({
    leaflet(data) %>% 
      addProviderTiles(provider = "Stamen.Watercolor") %>% 
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  # Flitering for first two data controls
  filteredData <- reactive({
    if ("All" %in% input$Region && "All" %in% input$Name){
      data %>% filter(DateNum >= input$timeline)
    }
    else if ("All" %in% input$Region && !("All" %in% input$Name)) {
      data %>% filter(Name == input$Name,
                      DateNum >= input$timeline)
    }
    else if (!("All" %in% input$Region) && "All" %in% input$Name) {
      data %>% filter(Region == input$Region,
                      DateNum >= input$timeline)
    }
    else {
      data %>% filter(Region == input$Region,
                      Name == input$Name,
                      DateNum >= input$timeline)
    }
  })
  
  #Filtering for second two data sets
  filteredDataTwo <- reactive({
    if ("All" %in% input$RegionT && "All" %in% input$NameT){
      dataTwo %>% filter(DateNumT >= input$timeline)
    }
    else if ("All" %in% input$RegionT && !("All" %in% input$NameT)) {
      dataTwo %>% filter(NameT == input$NameT,
                         DateNumT >= input$timeline)
    }
    else if (!("All" %in% input$RegionT) && "All" %in% input$NameT) {
      dataTwo %>% filter(RegionT == input$RegionT,
                         DateNumT >= input$timeline)
    }
    else {
      dataTwo %>% filter(RegionT == input$RegionT,
                         NameT == input$NameT,
                         DateNumT >= input$timeline)
    }
  })
  
  #filteredData %>% filter(DateNum >= input$timeline)
  #filteredDataTwo %>% filter(DateNum >= input$timeline)
  
  observe({
    # pal <- colorpal()
    # 
    # leafletProxy("map", data = filteredData()) %>% 
    #   clearMarkers() %>% 
    #   clearControls() %>% 
    #   addCircleMarkers(radius = 6,
    #                    stroke = FALSE,
    #                    fillColor = ~pal(accidentseverity),
    #                    fillOpacity = 0.7,
    #                    popup = ~paste("Severity: ", accidentseverity, 
    #                                   "<br/>",
    #                                   "Injuries: ", totalinjuries,
    #                                   "<br/>",
    #                                   "Fatalities: ", totalfatalities,
    #                                   "<br/>",
    #                                   "Type: ", bytype,
    #                                   "<br/>",
    #                                   "Conditions: ", weather,
    #                                   "<br/>",
    #                                   "Alcohol involved: ", alcohol)
    #   ) %>% 
    #   addLegend("bottomleft", pal = pal, values = ~accidentseverity,
    #             title = "Accident Severity",
    #             opacity = 1)
    
    pal <- colorpal()
    palTwo <- colorpalTwo()
    

    
    ### Map dataset one
    if ("None" %in% input$Region) {
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        clearControls()
    } else if (input$radio == 1) {
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>%
      #addTiles() %>%
      addCircleMarkers(radius = 7,
                       stroke = TRUE,
                       opacity = 1,
                       weight = 1,
                       color = '#000000',
                       fillOpacity = 1,
                       fillColor = ~pal(Name),
                       popup = ~paste("<b>Name</b>", "<br/>", Name,
                                      "<br/>",
                                      "<b>Date</b>", "<br/>", Date,
                                      "<br/>",
                                      "<b>Location</b>", "<br/>", Location,
                                      "<br/>",
                                      "<b>Epigraphic Link</b>", "<br/>", "<a href=", URL, ">Click</a>"),
                       clusterOptions = markerClusterOptions()) %>%
       addLegend("bottomleft", pal = palTwo, values = dataTwo$NameT,
                  title = "Doric-Aeolic",
                  opacity = 1) %>%
       addLegend("bottomleft", pal = pal, values = data$Name,
                 title = "Attic-Ionic",
                  opacity = 1) 
    } else if (input$radio == 2) {
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        clearControls() %>%
        #addTiles() %>%
        addCircleMarkers(radius = 7,
                         stroke = TRUE,
                         opacity = 1,
                         weight = 1,
                         color = '#000000',
                         fillOpacity = 1,
                         fillColor = ~pal(Name),
                         popup = ~paste("<b>Name</b>", "<br/>", Name,
                                        "<br/>",
                                        "<b>Date</b>", "<br/>", Date,
                                        "<br/>",
                                        "<b>Location</b>", "<br/>", Location,
                                        "<br/>",
                                        "<b>Epigraphic Link</b>", "<br/>", "<a href=", URL, ">Click</a>"),
                         clusterOptions = markerClusterOptions(disableClusteringAtZoom = TRUE)) %>%
        addLegend("bottomleft", pal = palTwo, values = dataTwo$NameT,
                  title = "Doric-Aeolic",
                  opacity = 1) %>%
        addLegend("bottomleft", pal = pal, values = data$Name,
                  title = "Attic-Ionic",
                  opacity = 1) 
    } else if (input$radio == 3) {
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        clearControls() %>%
        #addTiles() %>%
        addCircleMarkers(radius = 7,
                         stroke = TRUE,
                         opacity = 1,
                         weight = 1,
                         color = '#000000',
                         fillOpacity = 1,
                         fillColor = '#d13c3c',
                         popup = ~paste("<b>Name</b>", "<br/>", Name,
                                        "<br/>",
                                        "<b>Date</b>", "<br/>", Date,
                                        "<br/>",
                                        "<b>Location</b>", "<br/>", Location,
                                        "<br/>",
                                        "<b>Epigraphic Link</b>", "<br/>", "<a href=", URL, ">Click</a>"),
                         clusterOptions = markerClusterOptions(disableClusteringAtZoom = TRUE)) %>%
        addLegend("bottomleft", colors = c("#d13c3c", "#5f50e5"), labels = c("Attic-Ionic", "Doric-Aeolic"),
                  title = "Legend",
                  opacity = 1)
    }
    
    ### Map dataset two
    if ("None" %in% input$RegionT) {
      leafletProxy("map", data = filteredDataTwo()) %>%
        clearMarkers()
    } else if (input$radio == 1) {
    leafletProxy("map", data = filteredDataTwo()) %>%
      clearMarkers() %>%
      clearControls() %>%
      #addTiles() %>%
      addCircleMarkers(radius = 7,
                         stroke = TRUE,
                         opacity = 1,
                         weight = 2,
                         color = '#ffffff',
                         fillOpacity = 1,
                         fillColor = ~palTwo(NameT),
                         popup = ~paste("<b>Name</b>", "<br/>", NameT,
                                      "<br/>",
                                      "<b>Date</b>", "<br/>", DateT,
                                      "<br/>",
                                      "<b>Location</b>", "<br/>", LocationT,
                                      "<br/>",
                                      "<b>Epigraphic Link</b>", "<br/>", "<a href=", URLT, ">Click</a>"),
                         clusterOptions = markerClusterOptions()) %>%
        addLegend("bottomleft", pal = palTwo, values = dataTwo$NameT,
                  title = "Doric-Aeolic",
                  opacity = 1) %>%
        addLegend("bottomleft", pal = pal, values = data$Name,
                  title = "Attic-Ionic",
                  opacity = 1) 
    } else if (input$radio == 2) {
      leafletProxy("map", data = filteredDataTwo()) %>%
        clearMarkers() %>%
        clearControls() %>%
        #addTiles() %>%
        addCircleMarkers(radius = 7,
                         stroke = TRUE,
                         opacity = 1,
                         weight = 2,
                         color = '#ffffff',
                         fillOpacity = 1,
                         fillColor = ~palTwo(NameT),
                         popup = ~paste("<b>Name</b>", "<br/>", NameT,
                                        "<br/>",
                                        "<b>Date</b>", "<br/>", DateT,
                                        "<br/>",
                                        "<b>Location</b>", "<br/>", LocationT,
                                        "<br/>",
                                        "<b>Epigraphic Link</b>", "<br/>", "<a href=", URLT, ">Click</a>"),
                         clusterOptions = markerClusterOptions(disableClusteringAtZoom = TRUE)) %>%
        addLegend("bottomleft", pal = palTwo, values = dataTwo$NameT,
                  title = "Doric-Aeolic",
                  opacity = 1) %>%
        addLegend("bottomleft", pal = pal, values = data$Name,
                  title = "Attic-Ionic",
                  opacity = 1) 
    } else if ("None" %in% input$Region && input$radio == 3) {
      leafletProxy("map", data = filteredDataTwo()) %>%
        clearMarkers() %>%
        #clearControls() %>%
        #addTiles() %>%
        addCircleMarkers(radius = 7,
                         stroke = TRUE,
                         opacity = 1,
                         weight = 1,
                         color = '#000000',
                         fillOpacity = 1,
                         fillColor = '#5f50e5',
                         popup = ~paste("<b>Name</b>", "<br/>", NameT,
                                                "<br/>",
                                                "<b>Date</b>", "<br/>", DateT,
                                                "<br/>",
                                                "<b>Location</b>", "<br/>", LocationT,
                                                "<br/>",
                                                "<b>Epigraphic Link</b>", "<br/>", "<a href=", URLT, ">Click</a>"),
                         clusterOptions = markerClusterOptions(disableClusteringAtZoom = TRUE)) %>%
          addLegend("bottomleft", colors = c("#d13c3c", "#5f50e5"), labels = c("Attic-Ionic", "Doric-Aeolic"),
                    title = "Legend",
                    opacity = 1)
    } else if (!("None" %in% input$Region) && input$radio == 3) {
      leafletProxy("map", data = filteredDataTwo()) %>%
        clearMarkers() %>%
        #clearControls() %>%
        #addTiles() %>%
        addCircleMarkers(radius = 7,
                         stroke = TRUE,
                         opacity = 1,
                         weight = 1,
                         color = '#000000',
                         fillOpacity = 1,
                         fillColor = '#5f50e5',
                         popup = ~paste("<b>Name</b>", "<br/>", NameT,
                                        "<br/>",
                                        "<b>Date</b>", "<br/>", DateT,
                                        "<br/>",
                                        "<b>Location</b>", "<br/>", LocationT,
                                        "<br/>",
                                        "<b>Epigraphic Link</b>", "<br/>", "<a href=", URLT, ">Click</a>"),
                         clusterOptions = markerClusterOptions(disableClusteringAtZoom = TRUE))
    }
  })

  
  # output$histCentile <- renderPlot({
  #   ggplot(filteredSeverity(), aes(x = accidentseverity)) +
  #     geom_bar(stat = "count", aes(fill = bytype)) +
  #     theme_minimal() +
  #     labs(title = paste("Accident Severity in", input$range)) +
  #     xlab("Accident Severity (1 = most severe)") +
  #     ylab("No. of Accidents")
  # })
  
  # output$lineTrend <- renderPlot({
  #   ggplot(omaha_data, aes(x = year, color = bytype)) +
  #     geom_line(stat = "count") +
  #     theme(legend.title = element_blank()) +
  #     labs(title = "Trend for All Years") +
  #     geom_vline(xintercept=as.numeric(input$range), linetype = 1)
  # })
  
}

shinyApp(ui, server)








