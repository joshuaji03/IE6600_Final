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

map_disease <- function(caus, cty) {
  if (!isTruthy(cty)) {
    df_to_map <- total_d_country %>%
      filter(Cause_Specifics == caus) %>%
      filter(country_name == cty)
  } else {
    df_to_map <- filter(total_d_country, Cause_Specifics == caus)
  }
  try <- left_join(world_spdf@data, df_to_map, by = c("ISO3" = "iso3"))
  try$total[is.na(try$total)] <- 0
  
  mean <- mean(df_to_map$total)
  sd <- sd(df_to_map$total)
  
  mybins <- c(1,10000,20000,50000,100000,300000,Inf)
  mypalette <- colorBin(palette="YlOrBr", domain=try$total, na.color="transparent", bins=mybins)
  
  mytext <- paste(
    "Country: ", try$NAME,"<br/>", 
    "Total Death: ", try$total, "<br/>") %>%
    lapply(htmltools::HTML)
  
  lat = 10
  lon = 0
  if (cty != "") {
    temp <- try %>% filter(country_name == cty)
    lat = temp$LAT
    lon = temp$LON
  }
  
  leaflet(world_spdf) %>% 
    addTiles()  %>% 
    setView(lat=lat, lng=lon, zoom=3) %>%
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
  values <- reactiveValues(tp_c = NULL,
                           df_f = NULL,
                           get_d = NULL)
  
  observeEvent(input$Disease, {
    values$df_f <- filter(total_d_country, Cause_Specifics == input$Disease)
    values$tp_c <- sort(c(unique(values$df_f$country_name)))
  
    output$obs1 <- renderUI({
      selectInput(
        inputId = "cname_choice",
        label = "Choose name",
        choices =  values$tp_c,
        selected = NULL
        )
      })
    })
  
  output$aaa <- renderPrint({
    values$obs1
  })
  
  observe({
    output$init_map <- renderLeaflet(
      if (!is.null(req(input$Disease))) {
        map_disease(input$Disease,input$cname_choice)
      } else {
        map_disease(input$Disease, "")
      }
    )
  })
  
  observe({
    output$mymap <- renderLeaflet(
      map_disease(input$Disease,input$cname_choice)


      #if (isTruthy(input$cname_choice)) {
      #  map_disease(input$Disease, input$cname_choice)
      #} else {
      #  map_disease(input$Disease, "")
      #}
  )
  })
}