library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)

df <- read_csv('/Users/yixuanji/Desktop/FinalData.csv')
year <- c(unique(df$Year))
country <- c(unique(df$Country))
cause <- c(unique(df$Cause))

ui <- fluidPage(titlePanel("Group 7 Project"),
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    helpText("Visulization based on WHO dataset"),
                    
                    selectInput(
                      width="100%",
                      inputId = "Year",
                      label="Year:",
<<<<<<< HEAD
                      choices = year,
=======
                      choices = c("All",unique(FinalData$Year),
>>>>>>> 7937a905e760e3677d14e26c66a33816ac26e640
                      selected = NULL
                    ),
                    
                    selectInput(
                      width="100%",
                      inputId = "Country",
                      label="Country:",
<<<<<<< HEAD
                      choices = country,
=======
                      choices = c("All",unique(FinalData$Country),
>>>>>>> 7937a905e760e3677d14e26c66a33816ac26e640
                      selected = NULL
                    ),
                    
                    selectInput(
                      width="100%",
                      inputId = "Disease",
                      label="Disease:",
<<<<<<< HEAD
                      choices = cause
=======
                      choices = c("All",unique(FinalData$`Cause.Specifics`)
>>>>>>> 7937a905e760e3677d14e26c66a33816ac26e640
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

