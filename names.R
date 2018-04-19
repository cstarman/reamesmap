library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)

data <- readxl::read_excel("GreekData.xlsx")
dataTwo <- readxl::read_excel("ReamesDataTwo.xlsx")
dataThree <- readxl::read_excel("ReamesDataThree.xlsx")
# omaha_data <- data %>% filter(CityName == "Omaha")
names(data) <- c("Name", "Location", "Lat/Long", "Latitude", "Longitude", "Actual", "Date", "Region", "URL")
names(dataTwo) <- c("NameT", "LocationT", "Lat/LongT", "Latitude", "Longitude", "ActualT", "DateT", "RegionT", "URLT")


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100%"),
  tags$head(includeCSS("styles.css")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(h3("Usage of Hephestian"),
                id = "controls", class = "panel panel-default", top = 10, right = 10,
                fixed = TRUE, draggable = FALSE, width = 330, height = "auto",
# 
#                 selectInput("range", "Year:",
#                             c("2008", "2009", "2010", "2011", "2012", "2013", "2014")),
                # 
                # # Type filter
                # selectInput("bytype", "Choose type: ", choices = NULL),
                
                # Histogram 
                # plotOutput("histCentile", height = 200),
                # plotOutput("lineTrend", height = 140),
               
                # Region filters 
                selectInput("Region", "Region:", choices = NULL),
                
                selectInput("RegionT", "Region:", choices = NULL)

                # 
                # tags$p(tags$small(includeHTML("attr.html")))
                
   )
)

server <- function(input, output, session) {
  
  region_list <- data$Region
  names(region_list) <- region_list
  regionChoice <- c("All", region_list)
  updateSelectInput(session, "Region", choices = regionChoice)
  
  region_listT <- dataTwo$RegionT
  names(region_listT) <- region_listT
  regionChoiceT <- c("All", region_listT)
  updateSelectInput(session, "RegionT", choices = regionChoiceT)
  
  # type_list <- omaha_data$bytype
  # names(type_list) <- type_list
  # updateSelectInput(session, "bytype", choices = type_list)
  
  pallete <- brewer.pal(10, "Paired")
  
  colorpal <- reactive({
    colorFactor(pallete, data$Name)
  })
  
  output$map <- renderLeaflet({
    leaflet(data) %>% 
      addProviderTiles(provider = "Stamen.Watercolor") %>% 
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  filteredData <- reactive({
   if ("All" %in% input$Region){
      data
   }
   else {
      data %>% filter(Region == input$Region)
   }
   })
  
  
  filteredDataTwo <- reactive({
    if ("All" %in% input$RegionT){
      dataTwo
    }
    else {
      dataTwo %>% filter(RegionT == input$RegionT)
    }
  })
  
  
  # filteredSeverity <- reactive({
  #   omaha_data %>% filter(year == input$range)
  # })
  
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
    #pal <- colorFactor(c("navy", "red", "green", "orange", "pink", "purple", "yellow" ), domain = c("Ἡφαιστίων", "Ἡφαιστόδωρος", "Ἡφαίστιος", "Ἡφαιστόκλῆς", "Ἡφαιστόδοτος", "Ἡφαιστῆς", "Ἡφαιστιάδης"))
   
   #if ("All" %in% input$Region) {
      # choose all the choices _except_ "Select All"
    #  selected_choices <- setdiff(names, "All")
    #  updateSelectInput(session, "Region", selected = selected_choices)
    #}
    
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>%
      addTiles() %>%
    # # addProviderTiles(provider = "Stamen.Watercolor") %>%
      addCircleMarkers(radius = 7,
                       stroke = TRUE,
                       fillOpacity = 1,
                       color = ~pal(Name),
                       popup = ~paste(Name),
                       clusterOptions = markerClusterOptions())
    
    leafletProxy("map", data = filteredDataTwo()) %>%
      #clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>%
      addTiles() %>%
      addMarkers(popup = ~paste(NameT))
    
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