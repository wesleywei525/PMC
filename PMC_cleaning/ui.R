library(shiny)
library(dplyr)
library(readxl)
library(xlsx)
library(data.table)
library(DT)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("File cleaning app"),
  
  navbarPage(title = "File sourse",
             
             
             tabPanel("PayPal",
                      
                      sidebarPanel(
                        
                        fileInput(inputId = "pp_datafile", 
                                  label = "Choose a Paypal file (.csv or .xlsx)",
                                  multiple = FALSE,
                                  accept = c(".csv", ".xlsx")),
                        
                        actionButton(inputId = "pp_transform", label = "Clean Paypal data"),
                        
                        tags$hr(),
                        
                        downloadButton(outputId = "pp_downloadData", label = "Download")
                      ),
                      
                      
                      mainPanel(
                        
                        tabsetPanel(id = "ppTab",
                          
                          tabPanel("Original data", dataTableOutput("pp_data")),
                          
                          tabPanel("Result", dataTableOutput("pp_result"))
                        )
                      )
             ),
             
             
             tabPanel("MailChimp",
                      
                      sidebarPanel(
                        
                        fileInput("mc_datafile", "Choose a MailChimp file (.csv or .xlsx)",
                                  multiple = FALSE,
                                  accept = c(".csv", ".xlsx")),
                        
                        radioButtons("mc_type",
                                     "File type",
                                     c("Subscribed" = "Subscribed",
                                       "Cleaned" = "Cleaned",
                                       "Unsubscribed" = "Unsubscribed")),
                        
                        actionButton(inputId = "mc_transform", label = "Clean MailChimp data"),
                        
                        tags$hr(),
                        
                        downloadButton(outputId = "mc_downloadData", label = "Download")
                        
                        ),
                      
                      mainPanel(
                        
                        tabsetPanel(id = "mcTab",
                          
                          tabPanel("Original data", dataTableOutput("mc_data")),
                          
                          tabPanel("Result", dataTableOutput("mc_result"))
                        )
                      )
             ),
             
             
             tabPanel("Constant Contact",
                      
                      sidebarPanel(
                        
                        fileInput("cc_datafile", "Choose a Constant contact file (.csv or .xlsx)",
                                  multiple = FALSE,
                                  accept = c(".csv", ".xlsx")),
                        
                        actionButton("cc_transform", "Clean Constant Contact data"),
                        
                        tags$hr(),
                        
                        downloadButton("cc_downloadData","Download")
                      ),
                      
                      mainPanel(
                        
                        tabsetPanel(id = "ccTab",
                          
                          tabPanel("Original data", dataTableOutput("cc_data")),
                          
                          tabPanel("Result", dataTableOutput("cc_result"))
                        )
                      )
                      )
             )
  )
  )
