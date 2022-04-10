library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)

#df <- read_csv('/Users/yixuanji/Desktop/FinalData.csv')

#names(df)[names(df) == "Cause Specifics"] <- "Cause_Specifics"

year <- sort(c(unique(final_df$Year)), decreasing = TRUE)
country <- sort(c(unique(final_df$country_name)))
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
                      choices = country,
                      selected = NULL
                    ),
                    
                    selectInput(
                      width="100%",
                      inputId = "Disease",
                      label="Disease:",
                      choices = cause
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
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot",plotOutput("plot")),
                      tabPanel("Summary",verbatimTextOutput("summary")),
                      tabPanel("Table",tableOutput("table"))
                    ),
                    
                    fluidPage(fluidRow(
                    column(6,
                           DT::dataTableOutput("dataSet")),
                    column(6,
                           plotOutput(
                             "plotChart",
                             width = "100%",
                             height = "300px"
                           ))
                  )))
                ))

