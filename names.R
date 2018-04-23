library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)

data <- readxl::read_excel("GreekData.xlsx")
dataTwo <- readxl::read_excel("ReamesDataTwo.xlsx")
dataThree <- readxl::read_excel("ReamesDataThree.xlsx")
names(data) <- c("Name", "Location", "Lat/Long", "Latitude", "Longitude", "Actual", "Date", "Region", "URL")
names(dataTwo) <- c("NameT", "LocationT", "Lat/LongT", "Latitude", "Longitude", "ActualT", "DateT", "RegionT", "URLT")


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100%"),
  tags$head(includeCSS("styles.css")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(h3("Usage of Hephestian"),
                id = "controls", class = "panel panel-default", top = 10, right = 10,
                fixed = TRUE, draggable = FALSE, width = 330, height = "auto",
                
                # Histogram 
                # plotOutput("histCentile", height = 200),
                # plotOutput("lineTrend", height = 140),
               
                # Region filters 
                selectInput("Region", "Region:", choices = NULL),
                selectInput("Name", "Name:", choices = NULL),
                
                selectInput("RegionT", "Region Two:", choices = NULL),
                selectInput("NameT", "Name Two:", choices = NULL)
                
                # 
                # tags$p(tags$small(includeHTML("attr.html")))
                
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
  
  pallete <- brewer.pal(9, "Set1")
  
  colorpal <- reactive({
    colorFactor(pallete, data$Name)
  })
  
  output$map <- renderLeaflet({
    leaflet(data) %>% 
      addProviderTiles(provider = "Stamen.Watercolor") %>% 
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  filteredData <- reactive({
    if ("All" %in% input$Region && "All" %in% input$Name){
      data
    }
    else if ("All" %in% input$Region && !("All" %in% input$Name)) {
      data %>% filter(Name == input$Name)
    }
    else if (!("All" %in% input$Region) && "All" %in% input$Name) {
      data %>% filter(Region == input$Region)
    }
    else {
      data %>% filter(Region == input$Region,
                      Name == input$Name)
    }
  })
  
  
  filteredDataTwo <- reactive({
    if ("All" %in% input$RegionT && "All" %in% input$NameT){
      dataTwo
    }
    else if ("All" %in% input$RegionT && !("All" %in% input$NameT)) {
      dataTwo %>% filter(NameT == input$NameT)
    }
    else if (!("All" %in% input$RegionT) && "All" %in% input$NameT) {
      dataTwo %>% filter(RegionT == input$RegionT)
    }
    else {
      dataTwo %>% filter(RegionT == input$RegionT,
                      NameT == input$NameT)
    }
  })
  
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
    
    ### Map dataset one
    if ("None" %in% input$Region) {
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearMarkers()  
    } else {
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
                       popup = ~paste(Name),
                       clusterOptions = markerClusterOptions())# %>%
      #addLegend("bottomleft", pal = pal, values = data$Name,
      #           title = "Names",
      #            opacity = 1)
    }
    
    ### Map dataset two
    if ("None" %in% input$RegionT) {
      leafletProxy("map", data = filteredDataTwo()) %>%
        clearMarkers()
    } else {
    leafletProxy("map", data = filteredDataTwo()) %>%
      clearMarkers() %>%
      #clearControls() %>%
      #addTiles() %>%
      addMarkers(popup = ~paste(NameT),
                 clusterOptions = markerClusterOptions())
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








