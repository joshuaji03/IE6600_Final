library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(grid)
library(ggnewscale)
library(ggtext)
library(shadowtext)
library(patchwork)
library(scales)
library(ggrepel)
library(waffle)
library(forcats)
library(ggplot2)
library(dplyr)
library(extrafont)
library(ggthemes)

total_d_country <- final_df %>% 
  group_by(country_name,Cause_Specifics, iso3) %>%
  summarise(total = sum(Deaths1))

map_tiles <- "/world_shape_file/"

world_spdf <- readOGR( 
  dsn= paste0(getwd(),map_tiles), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

Totaldeaths=function(Country,Year,Causes){
  totaldeath <- final_df[final_df$country_name==Country & final_df$Year==Year & final_df$Cause_Specifics==Causes,] %>% 
    filter(!is.na(Deaths1)) %>% 
    group_by(country_name,Sex,Year) %>%
    summarize(Totaldeath=sum(Deaths1))
  
  stringtotal=paste('Total Number of Deaths:',sum(sum(totaldeath$Totaldeath)))
  stringfemale=paste('Female:',totaldeath[totaldeath$Sex==2,]$Totaldeath)
  stringmale=paste('Male:',totaldeath[totaldeath$Sex==1,]$Totaldeath)
  paste(stringtotal,stringfemale,stringmale,sep='<br>'
  )
  }

pie_func <- function(Country,Year,Causes) {
  
  data = final_df[final_df$country_name==Country & 
                    final_df$Year==Year & 
                    final_df$Cause_Specifics==Causes,] %>%
    replace(is.na(.), 0) %>% 
    mutate(Age0_to_14=rowSums(.[10:16])) %>% 
    mutate(Age15_to_24=rowSums(.[17:18])) %>%
    mutate(Age25_to_44=rowSums(.[19:22])) %>%
    mutate(Age45_to_64=rowSums(.[23:26])) %>%
    mutate(Age65_to_84=rowSums(.[27:30])) %>%
    mutate(Age_Over_84=rowSums(.[31:33])) %>%
    gather(40:45,key="age",value = "deaths",na.rm=FALSE) %>%
    group_by(age) %>%
    summarise(deaths = sum(deaths))
  
  data <- data %>%
    mutate(prop = round((deaths / sum(deaths)*100), 2)) %>%
    mutate(text_y = cumsum(prop)) 
  
  data$pop_y <- c(100 - data$prop[1]/2, 
                  100 - data$prop[1] - data$prop[2]/2, 
                  100 - data$prop[1] - data$prop[2] - data$prop[3]/2,
                  100 - data$prop[1] - data$prop[2] - data$prop[3] - data$prop[4]/2,
                  100 - data$prop[1] - data$prop[2] - data$prop[3] - data$prop[4] - data$prop[5]/2, 
                  100 - data$prop[1] - data$prop[2] - data$prop[3] - data$prop[4] - data$prop[5] - data$prop[6]/2)
  
  ggplot(data, mapping = aes(x = "", y = prop, fill = factor(age))) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    geom_label_repel(aes(label = paste0(prop,"%"), y = pop_y), nudge_x = 0.5,
                     show.legend = FALSE) +
    guides(fill = guide_legend(title = "Age Group")) +
    labs(title = "Total deaths by age") +
    theme_void() 
}

pie_func_sex <- function(Country,Year,Disease){
  data=final_df[final_df$country_name==Country & final_df$Year==Year & final_df$Cause_Specifics==Disease,] %>% 
    filter(!is.na(Deaths1)) %>% 
    group_by(country_name,Sex,Year) %>%
    summarize(Totaldeath=sum(Deaths1))
  
  data$Sex[data$Sex == 9] <- "Other"
  data$Sex[data$Sex == 1] <- "Male"
  data$Sex[data$Sex == 2] <- "Female"

  ggplot(data = data,
         mapping = aes(x = "", y = Totaldeath, fill = Sex))+
    geom_bar(width=1,stat = "identity") +
    coord_polar("y", start = 0) +
    theme_classic() +
    theme(plot.title = element_text(hjust=0.5),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(aes(label = paste(round(Totaldeath / sum(Totaldeath) * 100, 1), "%")),
              position = position_stack(vjust = 0.5))+
    labs(fill = "Sex",
         x = NULL,
         y = NULL,
         title = "Total deaths by Sex")+
    theme_void() 
}


Top5Cause <- function(Country,Year){
  test=final_df[final_df$country_name==Country & final_df$Year==Year,]
  test=test %>% group_by(country_name,Year,Cause_Specifics) %>%
    summarise(count=sum(Deaths1))
  test %>% arrange(desc(count)) %>% slice(1:5) %>% ggplot(.,aes(x=reorder(Cause_Specifics,count),y=count,fill=count)) + 
    geom_bar(stat='identity') +scale_y_continuous(labels=comma) + scale_fill_distiller(palette = "Reds", guide = FALSE, direction = 1) +
    coord_flip()+ggtitle( paste('Top 5 Death Cause','in the',Country,'in',Year))+
    theme(legend.position="none",axis.title=element_blank(),panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),plot.title=element_text(hjust=0,size=12),plot.title.position = "plot") +
    theme_tufte()
  }

LineGraph <- function(Country,Cause){
  test=final_df[final_df$country_name==Country & final_df$Cause_Specifics==Cause,]
  test=test %>% group_by(country_name,Year) %>%
    summarise(count=sum(Deaths1))
  test1=final_df[final_df$country_name==Country,]
  test1=test1 %>% group_by(country_name,Year) %>%
    summarise(count=sum(Deaths1))
  test$type=Cause
  test1$type='Total Deaths summed by All Causes'
  graphdata=rbind(test,test1)
  graphdata$type=factor(graphdata$type,levels=c('Total Deaths summed by All Causes',Cause ))
  ggplot(graphdata,mapping=aes(x=Year,y=count))+geom_line(mapping=aes(color=type),size=2.4) +   geom_point(
    aes(fill = type), size = 5, pch = 21,color = "white", stroke = 1)+scale_y_continuous(labels=comma)+
    theme(panel.background = element_rect(fill = "white"),panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
          axis.ticks.length.y = unit(0, "mm"), 
          axis.ticks.length.x = unit(2, "mm"),
          axis.title = element_blank(),
          axis.line.x.bottom = element_line(color = "black"),
          axis.text.x = element_text(family = "Econ Sans Cnd", size = 16),
          axis.text.y = element_text(family = "Econ Sans Cnd", size = 10)) +
    new_scale_color() + 
    labs(title = paste(Cause,'vs','Total Deaths')) + 
    theme(
      # theme_markdown() is provided by ggtext and means the title contains 
      # Markdown that should be parsed as such (the '**' symbols)
      plot.title = element_markdown(
        family = "Econ Sans Cnd", 
        size = 12
      ) 
    ) +
    theme_minimal() 
}

plot_tree_10 <- function(cty) {
  t <- final_df %>%
    filter(country_name == cty)  %>%
    replace(is.na(.), 0) %>% 
    mutate(Age0_to_14=rowSums(.[10:16])) %>% 
    mutate(Age15_to_24=rowSums(.[17:18])) %>%
    mutate(Age25_to_44=rowSums(.[19:22])) %>%
    mutate(Age45_to_64=rowSums(.[23:26])) %>%
    mutate(Age65_to_84=rowSums(.[27:30])) %>%
    mutate(Age_Over_84=rowSums(.[31:33])) %>%
    gather(40:45,key="age",value = "deaths",na.rm=FALSE) %>%
    group_by(Cause_Specifics, age) %>%
    summarise(
      total = sum(deaths)
    )
  
  top_5 <- final_df %>%
    filter(country_name == cty)  %>%
    group_by(Cause_Specifics) %>%
    summarise(
      total = sum(Deaths1)
    ) %>%
    top_n(n = 10, wt = total)
  
  t_death <- top_5$total
  top_5 <- top_5$Cause_Specifics
  t <- t[t$Cause_Specifics %in% c(top_5), ]
  age <- unique(t$age)
  count <- rep(cty, 10)
  space <- rep("",10)
  
  labels = c(cty, t$age, unique(t$Cause_Specifics))
  parents = c("", t$Cause_Specifics, count)
  values = c("", t$total,t_death)
  
  plot_ly(
    type="treemap",
    #ids=unique(t$country_name),
    labels = labels,
    parents = parents,
    values = values
  ) %>%
    layout(paper_bgcolor='transparent')
}

help_find <- function(cty, caus) {
  t <- final_df %>%
    filter(country_name == cty)  %>%
    replace(is.na(.), 0) %>% 
    mutate(Age0_to_14=rowSums(.[10:16])) %>% 
    mutate(Age15_to_24=rowSums(.[17:18])) %>%
    mutate(Age25_to_44=rowSums(.[19:22])) %>%
    mutate(Age45_to_64=rowSums(.[23:26])) %>%
    mutate(Age65_to_84=rowSums(.[27:30])) %>%
    mutate(Age_Over_84=rowSums(.[31:33])) %>%
    gather(40:45,key="age",value = "deaths",na.rm=FALSE) %>%
    group_by(Cause_Specifics, Sex, age) %>%
    summarise(
      total = sum(deaths)
    ) %>% 
    filter(Cause_Specifics == caus)
  
  t <- head(t, 12)
  
  return (t)
}

help_look <- function(cty, df, caus) {
  id_1 <- paste0(cty,"-",caus)
  age_tags <- c()
  male_tags <- c()
  female_tags <- c()
  
  for (i in 1:6) {
    tag <- paste0(id_1,"-",unique(df$age)[i])
    age_tags[i] <- tag
    male_tags[i] <- paste0(tag,"-","male")
    female_tags[i] <- paste0(tag,"-","female")
  }
  count <- rep(tolower(cty), 6)
  id = c("main",tolower(cty),age_tags,male_tags,female_tags)
  labels = c("Main",cty, unique(df$age), df$Sex)
  parents = c("", "main",count, age_tags,age_tags) 
  
  look <- data.frame(id, labels, parents)
  return (look)
}


plot_tree <- function(cty1, cty2, caus) {
  t1 = help_find(cty1, caus)
  t2 = help_find(cty2, caus)
  
  t1$Sex[t1$Sex == 1] <- "Male"
  t1$Sex[t1$Sex == 2] <- "Female"
  death_by_age_1 <- t1 %>% group_by(age) %>% summarise(total = sum(total))
  
  t2$Sex[t2$Sex == 1] <- "Male"
  t2$Sex[t2$Sex == 2] <- "Female"
  death_by_age_2 <- t2 %>% group_by(age) %>% summarise(total = sum(total))
  
  look1 <- help_look(cty1, t1, caus)
  look2 <- help_look(cty2, t2, caus)
  look2 <- look2[-1,]
  sum1 <- sum(death_by_age_1$total)
  sum2 <- sum(death_by_age_2$total)
  
  id <- c(look1$id,look2$id)
  label <- c(look1$labels,look2$labels)
  parent <- c(look1$parents,look2$parents)
  values <- c("",sum1, death_by_age_1$total, t1$total,
              sum2, death_by_age_2$total, t2$total)
  plot_ly(
    type="treemap",
    ids=id,
    labels = label,
    parents = parent,
    values = values,
    marker=list(colorscale=("Cyans"))) %>%  #c("#e34234","#b76760","#98757d","#60b0b7","#43c9d4")
    #layout(grid=list(columns=1, rows=1)) %>%
    layout(paper_bgcolor='transparent') %>%
    layout(autosize = F, width = 920, height = 500)
}

lineGraphComparison <- function(Country1, Country2, Cause) {

  test=final_df[final_df$country_name==Country1 & final_df$Cause_Specifics==Cause,]
  test=test %>% group_by(country_name,Year) %>%
    summarise(count=sum(Deaths1))
  test1=final_df[final_df$country_name==Country2 & final_df$Cause_Specifics==Cause,]
  test1=test1 %>% group_by(country_name,Year) %>%
    summarise(count=sum(Deaths1))
  test$type=Country1
  test1$type=Country2
  graphdata=rbind(test,test1)
  ggplot(graphdata,mapping=aes(x=Year,y=count))+geom_line(mapping=aes(color=type),size=2.4) +   geom_point(
    aes(fill = type), size = 5, pch = 21,color = "white", stroke = 1)+scale_y_continuous(labels = scales::comma) +
    theme(panel.background = element_rect(fill = "white"),panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
          axis.ticks.length.y = unit(0, "mm"), 
          axis.ticks.length.x = unit(2, "mm"),
          axis.title = element_blank(),
          axis.line.x.bottom = element_line(color = "black"),
          axis.text.x = element_text(family = "Econ Sans Cnd", size = 16),
          axis.text.y = element_text(family = "Econ Sans Cnd", size = 10)) +
    new_scale_color() + 
    labs(title = paste(Country1,'vs',Country2,'Total Deaths on',Cause)) + 
    theme(
      plot.title = element_markdown(
        family = "Econ Sans Cnd", 
        size = 13
      )
    ) + 
    theme_economist()
}

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
    scale_colour_manual(values = c("Female"="red","Male"="cyan","Average Deaths Worldwide" = "black"))
    
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
                           df_2 = NULL,
                           find_d = NULL,
                           plot_year = NULL,
                           plt_disease = NULL)
  
  observeEvent(input$Country, {
    req(length(input$Country) > 0)
    values$plot_year <- sort(unique(filter(final_df, country_name == input$Country)$Year), decreasing = TRUE)
    #values$tp_c <- sort(c(unique(values$df_f$country_name)))
    
    output$year <- 
      if (input$Country != "") {
        renderUI({
          selectInput(
            inputId = "year_choice",
            label = "Choose Year",
            choices =  c("",values$plot_year)
          )})
      }
  })
  
  observeEvent(input$year_choice, {
    req(length(input$Country) > 0, length(input$year_choice) > 0)
    values$plt_disease <- sort(unique(filter(final_df, country_name == input$Country,
                                       Year == input$year_choice)$Cause_Specifics))
    
    output$d_choice <- 
      if (input$Country != "" & input$year_choice != "") {
        renderUI({
          selectInput(
            inputId = "disease_choice",
            label = "Choose Disease",
            choices =  c("",values$plt_disease)
          )})
      }
  })
  
  observe({
    req(input$Country,input$year_choice,input$disease_choice)
    output$piechart <- renderPlot(
      pie_func(input$Country,input$year_choice,input$disease_choice),
      bg="transparent")
    })
  
  observe({
    req(input$Country,input$year_choice,input$disease_choice)
    output$piechartbygender <- renderPlot(
      pie_func_sex(input$Country,input$year_choice,input$disease_choice),
      bg="transparent")
  })
  
  observe({
    req(input$Country,input$year_choice,input$disease_choice)
    output$top5 <- renderPlot(
      Top5Cause(input$Country,input$year_choice),
      bg="transparent")
  })
  
  observe({
    req(input$Country,input$year_choice,input$disease_choice)
    output$tablesum <- renderText({
      Totaldeaths(input$Country,input$year_choice,input$disease_choice)
    })
  })
  
  observe({
    req(input$Country,input$year_choice,input$disease_choice)
    output$LineGraph <- renderPlot(
      LineGraph(input$Country,input$disease_choice),
      bg="transparent")
  })
  
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
  
  observeEvent(list(input$c1, input$c2), {
    req(length(input$c1) > 0, length(input$c2 > 0))
    values$df_d <- filter(total_d_country, country_name == input$c1 | country_name == input$c2) %>%
      group_by(Cause_Specifics) %>% filter(n() == 2)
    values$find_d <- sort(c(unique(values$df_d$Cause_Specifics)))

    output$ren_disease <- 
      if (input$c1 != "" & input$c2 != "") {
        renderUI({
          selectInput(
            inputId = "d_list",
            label = "Choose name",
            choices =  c("",values$find_d)
          )})
      }
  })
  

  observe(
    output$plot_compare<- renderPlot({
      req(input$c1, input$c2, input$d_list)
      lineGraphComparison(input$c1, input$c2, input$d_list)
    }, bg="transparent")
  ) 
  
  observe(
    output$plot_tree<- renderPlotly({
      req(input$c1, input$c2, input$d_list)
      plot_tree(input$c1, input$c2, input$d_list)
    })
  )
  

  
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