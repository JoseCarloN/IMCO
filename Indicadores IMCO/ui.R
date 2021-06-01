library(highcharter)
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(dplyr)
library(purrr)
library(waiter)
library(shinyjs)
library(shinyBS)

ui = tagList(
  useShinyjs(),
  dashboardPage(
    enable_preloader = TRUE,
    loading_background = "#FFFFFF",
    
    dashboardHeader(
      
    ),
    
    dashboardSidebar(
      disable = TRUE
    ),
    
    dashboardBody(
      
      tags$script(HTML("setInterval(function(){ $('[title]').tooltip(); }, 1000)")),
      # Para forzar el color de las ValueBoxes
      tags$style(".small-box.bg-green { background-color: #009F93 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-red { background-color: #4CB059 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-aqua { background-color: #59BEE9 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-blue { background-color: #2876B1 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-light-blue { background-color: #696F83 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-fuchsia { background-color: #DFA32D !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-navy { background-color: #D76584 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-teal { background-color: #F28F4F !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-olive { background-color: #95B0B5 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-lime { background-color: #FF0000 !important; color: #FFFFFF !important; }"),
      
      fluidRow(
        valueBoxOutput(outputId = "vbox1", width = 3),
        valueBoxOutput(outputId = "vbox2", width = 3),
        valueBoxOutput(outputId = "vbox9", width = 3),
        valueBoxOutput(outputId = "vbox10", width = 3)
      ),
      fluidRow(
        valueBoxOutput(outputId = "vbox3", width = 2),
        valueBoxOutput(outputId = "vbox4", width = 2),
        valueBoxOutput(outputId = "vbox5", width = 2),
        valueBoxOutput(outputId = "vbox6", width = 2),
        valueBoxOutput(outputId = "vbox7", width = 2),
        valueBoxOutput(outputId = "vbox8", width = 2)
      ),
      fluidRow(
        highchartOutput(outputId = "indice", height = "80vh")
      )
    )
  )
) 