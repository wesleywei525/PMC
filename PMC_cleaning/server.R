library(shiny)
library(dplyr)
library(readxl)
library(xlsx)
library(data.table)
library(DT)
library(shinythemes)

shinyServer(function(input, output, session) {
  
  #### Paypal file ####
  getData_pp <- reactive({
    
    req(input$pp_datafile)
    
    if (length(grep("csv", input$pp_datafile$datapath)) + length(grep("CSV", input$pp_datafile$datapath)) == 0) {
      PayPal_data <- read_excel(input$pp_datafile$datapath)
    } else {
      PayPal_data <- read.csv(input$pp_datafile$datapath,fileEncoding="UTF-8-BOM")
    }
    
    names(PayPal_data) <- gsub("[\\. /]","_",names(PayPal_data))
    
    if (length(setdiff(PaypalVariables, names(PayPal_data))) > 0) {
      mis_field_note <- paste("Field(s) missing from file: ",paste(setdiff(PaypalVariables, names(PayPal_data)), collapse = ", "))
      showNotification(mis_field_note, duration = NULL,type = "message")
    }
    
    return(PayPal_data)
    })
  
  observeEvent(input$pp_datafile, {
    updateTabsetPanel(session, "ppTab", selected = "Original data")
  })
  
  output$pp_data <- renderDataTable(
    datatable(getData_pp(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  )
  
  PaypalVariables <- c("Date", "Time", "Name", "Gross", "From_Email_Address", "Transaction_ID", 
                       "Address_Line_1", "Address_Line_2_District_Neighborhood", "Town_City", 
                       "State_Province_Region_County_Territory_Prefecture_Republic", "Zip_Postal_Code", "Country", 
                       "Contact_Phone_Number", "Note")
  
  datasetTrans_pp <- eventReactive(input$pp_transform,{
    PayPal_data <- getData_pp()
    
    CleanPaypal <- PayPal_data[intersect(PaypalVariables, names(PayPal_data))] %>%
      setnames(old = c("Date", "Time", "Name", "Gross", "From_Email_Address", "Transaction_ID", 
                       "Address_Line_1", "Address_Line_2_District_Neighborhood", "Town_City", 
                       "State_Province_Region_County_Territory_Prefecture_Republic", "Zip_Postal_Code", "Country", 
                       "Contact_Phone_Number", "Note"),
               
               new = c("Date", "Time", "Full_name", "Amount", "Email", "Transaction_ID", 
                       "Address", "Address_2", "City", "State", "Zip_code", "Country", "Home_Phone", "Description"),
               
               skip_absent = TRUE)
    
    
    CleanPaypal <- CleanPaypal[CleanPaypal$Amount >= 0, ]
    
    CleanPaypal$Payment_Method <- ifelse(CleanPaypal$Amount == 0, 'In Kind', 'Paypal')
    
    CleanPaypal$Donation_Date <- paste(as.Date(CleanPaypal$Date,"%m/%d/%Y"), gsub("1899-12-31 ","",CleanPaypal$Time))
    
    CleanPaypal$group_no_name <- ifelse(CleanPaypal$Full_name == "" | is.na(CleanPaypal$Full_name), "yes", NA)
    
    CleanPaypal$Full_name <- ifelse(CleanPaypal$Full_name == "" | is.na(CleanPaypal$Full_name), CleanPaypal$Email, CleanPaypal$Full_name)
    
    pp_order <- c("Donation_Date", "Amount", "Payment_Method", "Full_name","Email", "Home_Phone",
                  "Address", "Address_2", "City", "State", "Zip_code", "Country",
                  "Description", "group_no_name", "Transaction_ID")
    
    CleanPaypal <- select(CleanPaypal, intersect(pp_order, names(CleanPaypal)))
    
    return(CleanPaypal)
    
  })
  
  observeEvent(input$pp_transform, {
    updateTabsetPanel(session, "ppTab", selected = "Result")
  })
  
  output$pp_result <- renderDataTable({
    datatable(datasetTrans_pp(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  })
  
  output$pp_downloadData <- downloadHandler(
    filename = "clean_paypal_file.xlsx",
    content = function(file){
      write.xlsx(as.data.frame(datasetTrans_pp()),file,row.names=FALSE, showNA=FALSE)
    }
  )
  
  
  #### MailChimp file ####
  getData_mc <- reactive({
    
    req(input$mc_datafile)
    
    if (length(grep("csv", input$mc_datafile$datapath)) + length(grep("CSV", input$mc_datafile$datapath)) == 0) {
      Mailchimp_data <- read_excel(input$mc_datafile$datapath, col_types = "text")
    } else {
      Mailchimp_data <- read.csv(input$mc_datafile$datapath, fileEncoding="UTF-8-BOM")
    }
    
    names(Mailchimp_data) <- gsub("[\\. /]","_",names(Mailchimp_data))
    
    if (length(setdiff(MailchimpVariables, names(Mailchimp_data))) > 0) {
      mis_field_note <- paste("Field(s) missing from file: ",paste(setdiff(MailchimpVariables, names(Mailchimp_data)), collapse = ", "))
      showNotification(mis_field_note, duration = NULL,type = "message")
    }
    
    return(Mailchimp_data)
  })
  
  observeEvent(input$mc_datafile, {
    updateTabsetPanel(session, "mcTab", selected = "Original data")
  })
  
  output$mc_data <- renderDataTable(
    datatable(getData_mc(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  )
  
  MailchimpVariables <- c("First_Name", "Last_Name", "Email_Address",  "Phone_Number", "Address","REGION", "CC","NOTES")
  
  mc_file_type <- reactive({input$mc_type})
  
  datasetTrans_mc <- eventReactive(input$mc_transform,{
    Mailchimp_data <- getData_mc()
    
    CleanMailchimp <- Mailchimp_data[intersect(MailchimpVariables, names(Mailchimp_data))] %>%
      setnames(old = c("First_Name", "Last_Name", "Email_Address", "Address", "Phone_Number", "CC", "REGION", "NOTES"),
               
               new = c("First_Name", "Last_Name", "Email", "Full_address", "Home_phone", "Country", "State", "user_notes"),
               
               skip_absent = TRUE)
    
    CleanMailchimp$State <- toupper(CleanMailchimp$State)
    
    CleanMailchimp$Country <- toupper(CleanMailchimp$Country)
    
    for (k in which(CleanMailchimp$First_Name == "" | is.na(CleanMailchimp$First_Name))) {
      CleanMailchimp$First_Name[k] <- CleanMailchimp$Last_Name[k]
      CleanMailchimp$Last_Name[k] <- NA
    }
    
    ifelse(input$mc_type == "Unsubscribed", CleanMailchimp$receive_emails <- FALSE, NA)
    
    CleanMailchimp$group_no_name <- ifelse(CleanMailchimp$First_Name == "" | is.na(CleanMailchimp$First_Name), "yes", NA)
    
    CleanMailchimp$First_Name <- ifelse(CleanMailchimp$First_Name == "" | is.na(CleanMailchimp$First_Name), CleanMailchimp$Email, CleanMailchimp$First_Name)
    
    CleanMailchimp[,paste("group_",input$mc_type,sep="")]<- "yes"
    
    return(CleanMailchimp)
    
  })
  
  observeEvent(input$mc_transform, {
    updateTabsetPanel(session, "mcTab", selected = "Result")
  })
  
  output$mc_result <- renderDataTable({
    datatable(datasetTrans_mc(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  })
  
  output$mc_downloadData <- downloadHandler(
    filename = function(){paste("clean",input$mc_type,"file.xlsx", sep="_")},
    content = function(file){
      write.xlsx(as.data.frame(datasetTrans_mc()),file,row.names=FALSE, showNA=FALSE)
    }
  )

  
  
  #### Constant Contact file ####
  getData_cc <- reactive({
    
    req(input$cc_datafile)
    
    if (length(grep("csv", input$cc_datafile$datapath)) + length(grep("CSV", input$cc_datafile$datapath)) == 0) {
      read_excel(input$cc_datafile$datapath, col_types = "text")
    } else {
      read.csv(input$cc_datafile$datapath, fileEncoding="UTF-8-BOM")
    }
  })
  
  observeEvent(input$cc_datafile, {
    updateTabsetPanel(session, "ccTab", selected = "Original data")
  })
  
  output$cc_data <- renderDataTable(
    datatable(getData_cc(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  )
  
  datasetTrans_cc <- eventReactive(input$cc_transform,{
    ConstantC <- getData_cc()
    
    CleanConstantC <- ConstantC
    
    return(CleanConstantC)
    
  })
  
  observeEvent(input$cc_transform, {
    updateTabsetPanel(session, "ccTab", selected = "Result")
  })
  
  output$cc_result <- renderDataTable({
    datatable(datasetTrans_cc(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  })
  
  output$cc_downloadData <- downloadHandler(
    filename = "clean_constant_contact_file.csv",
    content = function(file){
      write.csv(datasetTrans_cc(),file,na = "")
    }
  )
  
}###function ending
)
