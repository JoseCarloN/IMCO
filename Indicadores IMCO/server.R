library(highcharter)
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(dplyr)
library(purrr)
library(waiter)
library(shinyjs)
library(shinyBS)

server = function(input, output) {
  
  onclick("vbox1", showModal(modalDialog(
    title = "Your title",
    footer = modalButton("Cerrar"),
    renderHighchart(hc_rank),
    size = "l",
    easyClose = TRUE,
  )))
  
  # ValueBoxes
  output$vbox1 = renderValueBox(vb_derecho)
  output$vbox2 = renderValueBox(vb_MA)
  output$vbox3 = renderValueBox(vb_sociedad)
  output$vbox4 = renderValueBox(vb_politico)
  output$vbox5 = renderValueBox(vb_gobierno)
  output$vbox6 = renderValueBox(vb_factores)
  output$vbox7 = renderValueBox(vb_economia)
  output$vbox8 = renderValueBox(vb_precursores)
  output$vbox9 = renderValueBox(vb_ri)
  output$vbox10 = renderValueBox(vb_innovacion)
  
  # HC charts
  output$indice = renderHighchart(hc_indices)
}