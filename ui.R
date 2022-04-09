library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)

ui <- fluidPage(titlePanel("Group 7 Project"),
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    helpText("Visulization based on WHO dataset"),
                    
                    selectInput(
                      width="100%",
                      inputId = "Year",
                      label="Year:",
                      choices = c("All",unique(FinalData$Year),
                      selected = NULL
                    ),
                    
                    selectInput(
                      width="100%",
                      inputId = "Country",
                      label="Country:",
                      choices = c("All",unique(FinalData$Country),
                      selected = NULL
                    ),
                    
                    selectInput(
                      width="100%",
                      inputId = "Disease",
                      label="Disease:",
                      choices = c("All",unique(FinalData$`Cause.Specifics`)
                    ),
                    
                    uiOutput("obs1"),
                    actionButton(
                      inputId = "reset",
                      label = "Reset Data",
                      icon = icon("refresh"),
                      width = "100%"
                    ),
                    verbatimTextOutput("aaa")
                  ),
                  mainPanel(fluidPage(fluidRow(
                    column(6,
                           DT::dataTableOutput("dataSet")),
                    column(6,
                           plotOutput(
                             "plotChart",
                             width = "100%",
                             height = "300px"
                           ))
                  )))
                )))))

