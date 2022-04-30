library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(RSocrata)
library(dplyr)
library(ggplot2)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

setwd("~/Documents/GitHub/Group_J_Climate_Economics/")
df =  read.csv('final_to_use.csv')

countryfinal <- df %>% 
  group_by(Country) %>%
  filter(Year>=2010&Year<=2015)%>%
  summarize(sum_death = sum(Total.Deaths,na.rm=TRUE),
            sum_affected = sum(Total.Affected, na.rm=TRUE),
            affected_to_death = sum(Total.Deaths/Total.Affected,na.rm=TRUE),
            average_temp = mean(tmean,na.rm=TRUE),
            sum_freq = sum(Frequency,na.rm=TRUE),
            avg_gdp_cap_growth = mean(gdp_per_cap_growth, na.rm=TRUE),
            average_perc_urban = mean(X..Urban.Population,na.rm=TRUE),
            avg_gdp = mean(GDP),
            avg_gdp_cap = mean(gdp_per_cap,na.rm=TRUE),
            avg_temp = mean(tmean,na.rm=TRUE),
            average_perc_arable = mean(perc_arable_land_area, na.rm=TRUE),
            average_food_prod = mean(Food.Production, na.rm=TRUE),
            average_pop = mean(Population,na.rm=TRUE),
            avg_life_exp = mean(Life.Expectancy,na.rm=TRUE),
            avg_perc_forest = mean(perc_forest_land_area,na.rm=TRUE),
            avg_perc_energy_ren = mean(X..Final.Energy.from.Renewables,na.rm=TRUE),
            avg_co2 = mean(co2_emission,na.rm=TRUE),
            avg_remittances = mean(remittances_received,na.rm=TRUE),
            avg_elect_days = mean(days_for_electricity,na.rm=TRUE)
  )

world <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- merge(world, countryfinal, by.x="name", by.y="Country")

groups <- c('Average Temperature'= "average_temp",
            'Number of Natural Disasters' = 'sum_freq',
            'Percent of Forest' = 'avg_perc_forest',
            'Percent Urban Population'='average_perc_urban',
            'Total Population' = 'average_pop')



ui <- fluidPage(
  
  
  #sidebarLayout(
    selectInput("Ind","Independent Variable",choices = groups),
   # sidebarPanel(
    #  radioButtons(
    #    inputId = "group",
    #    label = "Select a group to map",
    #    choices = groups
    #  )
    #),
   # mainPanel(
      leafletOutput("map", height = "600")
    #)
  )
#)



server = function(input, output) {
  group_to_map <- reactive({
    input$Ind
  })
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g ",
    world_map$name_long, world_map$avg_temp
  ) %>% lapply(htmltools::HTML)
  
  output$map <- renderLeaflet({
    req(group_to_map())
    leaflet(world_map)%>%
      addPolygons(stroke = TRUE, smoothFactor = 0.5,
                  weight=1, color='#333333', opacity=1, 
                  fillColor = ~colorQuantile("Greens", average_temp)(average_temp), 
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label=labels)
    
  })
  
  observeEvent(input$Ind, {
    
    pal <- colorNumeric("viridis", range(world_map[[group_to_map()]]))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g ",
      world_map$name, world_map[[group_to_map()]]
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = world_map,
                  color = ~pal(world_map[[group_to_map()]]),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label=labels) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = world_map[[group_to_map()]],
      )
  })
  
}

shinyApp(ui, server)