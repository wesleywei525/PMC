library(shiny)
library(shinyWidgets)
library(dplyr)
library(readxl)
library(xlsx)
library(data.table)
library(DT)
library(shinythemes)
library(tidyr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("File cleaning app"),
  
  navbarPage(title = "File sourse",
             
             #### GC UI ####               
             tabPanel("Group/Email list Cleaning",
                      
                      sidebarPanel(
                        
                        fileInput("cc_datafile", "Choose a file (.csv or .xlsx)",
                                  multiple = FALSE,
                                  accept = c(".csv", ".xlsx")),
                        
                        uiOutput("CC_group_field"),
                        helpText("Select the field you want to use to create groups"),
                        
                        selectInput(inputId = "group_sep",
                                     label = "Separator",
                                     choices = c("Comma [ , ]" = ",",
                                                 "Semicolon [ ; ]" = ";",
                                                 "Bar [ | ]" = "|",
                                                 "Slash [ / ]" = "/",
                                                 "Other" = "other")),
                        
                        tags$hr(),
                        
                        actionButton("cc_transform", "Check groups list"),
                        helpText("Click to check number of unique groups.", 
                                 "Create custom field if greater than 20."),
                        
                        tags$hr(),
                        
                        actionButton("group_clean", "Clean groups"),
                        
                        
                        tags$hr(),
                        
                        downloadButton("cc_downloadData","Download")
                      ),
                      
                      mainPanel(
                        
                        tabsetPanel(id = "ccTab",
                                    
                                    tabPanel("Original data", dataTableOutput("cc_data")),
                                    
                                    tabPanel("Group list", dataTableOutput("cc_result")),
                                    
                                    tabPanel("Group result", dataTableOutput("group_result"))
                        )
                      )
             ),
             
  #### PP UI ####           
             tabPanel("PayPal",
                      
                      sidebarPanel(
                        
                        fileInput(inputId = "pp_datafile", 
                                  label = "Choose a Paypal file (.csv or .xlsx)",
                                  multiple = FALSE,
                                  accept = c(".csv", ".xlsx")),
                        
                        uiOutput("pp_add_fields"),

                        tags$hr(),
                        
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
             
  #### MC UI ####               
             tabPanel("MailChimp",
                      
                      sidebarPanel(
                        
                        fileInput("mc_datafile", "Choose a MailChimp file (.csv or .xlsx)",
                                  multiple = FALSE,
                                  accept = c(".csv", ".xlsx")),
                        
                        radioButtons(inputId = "mc_type",
                                     label = "File type",
                                     choices = c("Subscribed" = "Subscribed",
                                                 "Cleaned" = "Cleaned",
                                                 "Unsubscribed" = "Unsubscribed")),
                        
                        tags$hr(),
                        
                        uiOutput("MC_add_field"),
                        
                        tags$hr(),
                        
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
             )
             

             )
  )
  )
