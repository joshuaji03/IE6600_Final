library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(leaflet)
library(rgdal)

final_df <- read_csv('/Users/yixuanji/Desktop/Northeastern/IE\ 6600/IE6600_Final/Data/final_df.csv')

year <- sort(c(unique(final_df$Year)), decreasing = TRUE)
cname <- sort(c(unique(final_df$country_name)))
cause <- sort(c(unique(final_df$Cause_Specifics)))

ui <- fluidPage(titlePanel("Group 7 Project"),
                sidebarLayout(
                  sidebarPanel(
                    width = 4,
                    helpText("Visulization based on WHO dataset"),
                    
                    selectInput(
                      width="100%",
                      inputId = "Year",
                      label="Year:",
                      choices = year,
                      selected = NULL
                    ),
                    
                    selectInput(
                      width="100%",
                      inputId = "Country",
                      label="Country:",
                      choices = cname,
                      selected = NULL
                    ),
                    
                    selectInput(
                      width="100%",
                      inputId = "Disease",
                      label="Disease:",
                      choices = cause
                    ),
                    actionButton(
                      inputId = "reset",
                      label = "Reset Data",
                      icon = icon("refresh"),
                      width = "100%"
                    ),
                    verbatimTextOutput("aaa")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot",
                               plotOutput(
                                 "plotmap",
                                 width = "150%",
                                 height = "300px"
                                 )
                               ),
                      tabPanel("Map",leafletOutput("mymap")),
                      tabPanel("Table",tableOutput("table"))
                    ),
                    fluidPage(fluidRow(
                    column(6,
                           plotOutput(
                             "plotmap_2",
                             width = "150%",
                             height = "300px"
                           ))
                  )))
                ))

