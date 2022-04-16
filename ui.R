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
                  conditionalPanel(
                    condition = "input.tabs == 'Map'",
                    tags$style(
                      HTML("
                      #controls:hover{
                      opacity: 1;
                      }")),
                    absolutePanel(id = "controls",
                                  top = 120, 
                                  left = 25, 
                                  width = "75%",
                                  height = 50,
                                  draggable = TRUE,
                            sidebarPanel(
                              width = 4,
                              helpText("Visulization based on WHO dataset"),
                              selectInput(
                                  width="100%",
                                  inputId = "Disease",
                                  label="Disease:",
                                  choices = c("",cause),
                                  selected = NULL),
                            uiOutput("obs1"),
                            plotOutput("chart",
                                       height = 200,
                                       width = 300)),
                            style = "opacity: 0.75; z-index: 10;" 
                    )),
                  mainPanel(
                    tabsetPanel(
                      id = "tabs",
                      tabPanel("Plot"),
                      tabPanel("Map",
                               leafletOutput("init_map", 
                                             width = "150%",
                                             height = 800)),
                      tabPanel("Table")
                    ))
                )