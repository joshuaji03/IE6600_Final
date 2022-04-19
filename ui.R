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
r <- round(runif(1, min=1, max=3))
img <- paste0(r,".png")

sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}

ui <- fluidPage(
  setBackgroundColor(
    color = c("#F2E5D9", "#A18167"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel(
    fluidRow(
    column(9, h1("WHO Mortality Visualization")), 
    column(8, h3("WHO Mortality Visualization")),
    column(2, img(height = 175, width = 300,src=paste0(round(runif(1, min=1, max=3)),".png")),
           align = 'right')
    )
  ),
                  conditionalPanel(
                    condition = "input.tabs == 'Map'",
                    tags$style(
                      HTML("
                      #controls:hover{
                      opacity: 1;
                      }")),
                    absolutePanel(id = "controls",
                                  top = 320, 
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
                                       width = "100%")),
                            style = "opacity: 0.75; z-index: 10;" 
                    )),
                  mainPanel(
                    tabsetPanel(
                      id = "tabs",
                      tabPanel(
                        "Plot",
                        sidebarLayout(
                          sidebarPanel2(fluid=FALSE,width = 4,
                                      helpText("Visulization based on WHO dataset"),
                                      selectInput(
                                        width="100%",
                                        inputId = "Country",
                                        label="Country:",
                                        choices = c("",cname),
                                        selected = NULL
                                      ),
                                      uiOutput("year"),
                                      uiOutput("d_choice"),
                                      actionButton(
                                        inputId = "reset",
                                        label = "Reset Data",
                                        icon = icon("refresh"),
                                        width = "100%"
                                      ),
                                      out=h5(htmlOutput("tablesum")),
                                      style = "opacity: 0.85"),
                        mainPanel(
                          fluidRow(
                            column(width=12,
                                   splitLayout(cellWidths=c('90%','100%'),
                                               plotOutput("top5",
                                                          width='70%'),
                                               plotOutput("LineGraph",
                                                          width='80%'))),
                            column(width=12,
                                   splitLayout(cellWidths = 500,
                                               cellArgs = list(style = "padding: 6px"),
                                               
                                               plotOutput("piechart",
                                                          width = '95%'),
                                               plotOutput("piechartbygender",
                                                          width = '100%')))
                        ))),
                        
                        style='width: 1000px; height: 1000px'),
                      
                      tabPanel("Map",
                               leafletOutput("init_map", 
                                             width = "150%",
                                             height = 800)),
                      tabPanel("Comparison",
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 3,
                                   helpText("Comparing two countries' mortality information on a given disease"),
                                   selectInput(
                                     width="100%",
                                     inputId = "c1",
                                     label="Country 1:",
                                     choices = c("",cname),
                                     selected = NULL),
                                   selectInput(
                                     width="100%",
                                     inputId = "c2",
                                     label="Country 2:",
                                     choices = c("",cname),
                                     selected = NULL),
                                   uiOutput("ren_disease"),
                                   style = "opacity: 0.85"),
                               mainPanel(
                                 fluidRow(
                                   column(width = 12,
                                          offset = 1,
                                          plotOutput("plot_compare",
                                                     width='80%')),
                                   column(width = 12,
                                          plotlyOutput("plot_tree",
                                                       width='100%'))))
                               ),
                               style='width: 1500px; height: 1000px')
                    ))
)