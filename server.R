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

addlinetoplot <- function(dataset, varx, vary) { 
  list(
    geom_line(data=dataset, aes_string(x=varx, y=vary)), 
    geom_point(data=dataset, aes_string(x=varx, y=vary))
  )
}

plot_chart <- function(caus, cty) {
  to_plot <- final_df %>%
    filter(country_name == cty) %>%
    filter(Cause_Specifics == caus) %>%
    group_by(Year, country_name, Sex) %>%
    summarise(total = sum(Deaths1))
  
  to_plot_2 <- final_df %>%
    filter(Cause_Specifics == caus) %>%
    group_by(Year) %>%
    summarise(
      Sex = "avg",
      total = mean(Deaths1))
  
  to_plot$Sex[to_plot$Sex == 1] <- "Male"
  to_plot$Sex[to_plot$Sex == 2] <- "Female"
  
  
  ggplot(to_plot, aes(x=Year, y=total, group = Sex)) +
    geom_line(aes(color = Sex)) +
    geom_point(aes(color=Sex)) +
    theme_light() +
    addlinetoplot(to_plot_2, varx = "Year", vary = "total") + 
    scale_colour_manual(values = c("Female"="red","Male"="cyan","Average" = "black"))
    
  #theme_classic()
  #theme_minimal()
  }

map_disease <- function(caus, cty) {
  df_to_map <- total_d_country %>%
    filter(Cause_Specifics == caus)
  try <- left_join(world_spdf@data, df_to_map, by = c("ISO3" = "iso3"))
  try$total[is.na(try$total)] <- 0
  
  
  mybins <- c(1,10000,20000,50000,100000,300000,Inf)
  mypalette <- colorBin(palette="YlOrBr", domain=try$total, na.color="transparent", bins=mybins)
  
  mytext <- paste(
    "Country: ", try$NAME,"<br/>", 
    "Total Death: ", try$total, "<br/>") %>%
    lapply(htmltools::HTML)
  lat = 10
  lon = 0
  if (isTruthy(cty)) {
    temp <- try %>% filter(country_name == cty)
    lat = temp$LAT
    lon = temp$LON
  }
  
  leaflet(world_spdf) %>% 
    addTiles()  %>% 
    setView(lat=lat, lng=lon, zoom=4) %>%
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
    req(length(input$Disease) > 0)
    values$df_f <- filter(total_d_country, Cause_Specifics == input$Disease)
    values$tp_c <- sort(c(unique(values$df_f$country_name)))
    
    output$obs1 <- 
      if (input$Disease != "") {
        renderUI({
        selectInput(
          inputId = "cname_choice",
          label = "Choose name",
          choices =  c("",values$tp_c)
        )})
      }
    })
  #observeEvent(input$cname_choice,{
  #  values$get_d <- final_df %>%
  #    filter(Cause_Specifics == input$Disease) %>%
  #    filter(country_name == intput$cname_choice) %>%
  #    group_by(country_name, Year, Sex) %>%
  #    summarise(      
  #      total_death = sum(Deaths1, na.rm = TRUE))
  #})
  
  output$aaa <- renderPrint({
    values$obs1
  })
  
  observe(
    output$chart <- renderPlot(
      plot_chart(input$Disease, input$cname_choice)
    )
  )
  
  observe({
    output$init_map <- renderLeaflet(
      if (isTruthy(input$cname_choice)) {
        map_disease(input$Disease, input$cname_choice)
      } else {
        map_disease(input$Disease, "")
      }
  )
  })
}