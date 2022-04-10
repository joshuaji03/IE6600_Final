library(shiny)

total_d_country <- final_df %>% 
  group_by(country_name,Cause_Specifics, iso3) %>%
  summarise(total = sum(Deaths1))

map_tiles <- "/world_shape_file/"

world_spdf <- readOGR( 
  dsn= paste0(getwd(),map_tiles), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

map_disease <- function(caus) {
  df_to_map <- filter(total_d_country, Cause_Specifics == caus)
  try <- left_join(world_spdf@data, df_to_map, by = c("ISO3" = "iso3"))
  try$total[is.na(try$total)] <- 0
  
  mybins <- c(0,10000,20000,50000,100000,300000,Inf)
  mypalette <- colorBin(palette="YlOrBr", domain=try$total, na.color="transparent", bins=mybins)
  
  mytext <- paste(
    "Country: ", try$NAME,"<br/>", 
    "Total Death: ", try$total, "<br/>") %>%
    lapply(htmltools::HTML)
  
  leaflet(world_spdf) %>% 
    addTiles()  %>% 
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
      fillColor = ~mypalette(try$total), 
      stroke=TRUE, 
      fillOpacity = 0.9, 
      color="white", 
      weight=0.3,
      label = mytext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend(pal=mypalette, values=try$total, opacity=0.9, title = caus, position = "bottomleft" )
}


server <- function(input, output) {
  country_map <- reactive({
    
  })
  
  observe({
    output$mymap <- renderLeaflet(map_disease(input$Disease))
  })
}