library(shiny)
library(dplyr)
library(readxl)
library(xlsx)
library(data.table)
library(DT)
library(shinythemes)
library(tidyr)

shinyServer(function(input, output, session) {
  
  #### Paypal file ####
  getData_pp <- reactive({
    
    req(input$pp_datafile)
    
    if (length(grep("csv", input$pp_datafile$datapath)) + length(grep("CSV", input$pp_datafile$datapath)) == 0) {
      PayPal_data <- read_excel(input$pp_datafile$datapath)
    } else {
      PayPal_data <- read.csv(input$pp_datafile$datapath, stringsAsFactors = FALSE)
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
  
  
  PaypalVariables <- c("Date","Time","Name", "Gross", "From_Email_Address", "Transaction_ID", 
                       "Address_Line_1", "Address_Line_2_District_Neighborhood", "Town_City", 
                       "State_Province_Region_County_Territory_Prefecture_Republic", "Zip_Postal_Code", "Country", 
                       "Contact_Phone_Number", "Note")
  
  output$pp_add_fields <- renderUI({
    
    options <- setdiff(names(getData_pp()),PaypalVariables)
    
    pickerInput(inputId = "pp_add_field", label = "Addional field(s)",
                choices = options,
                multiple = TRUE,
                options = pickerOptions(actionsBox = TRUE, 
                                        selectedTextFormat = paste0("count > ", length(options)-1),
                                        countSelectedText = "All selected")
                
    )
    

  })
  
  output$pp_data <- renderDataTable(
    datatable(getData_pp(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  )

  
  datasetTrans_pp <- eventReactive(input$pp_transform,{
    PayPal_data <- getData_pp()
    
    CleanPaypal <- PayPal_data[intersect(c(PaypalVariables, input$pp_add_field), names(PayPal_data))] %>%
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
    
    CleanPaypal <- select(CleanPaypal, intersect(c(pp_order, input$pp_add_field), names(CleanPaypal)))

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
      Mailchimp_data <- read.csv(input$mc_datafile$datapath, stringsAsFactors = FALSE)
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
  
  output$MC_add_field <- renderUI({
    
    options <- setdiff(names(getData_mc()),MailchimpVariables)
    
    pickerInput(inputId = "mc_add_field", label = "Addional field(s)",
                choices = options,
                multiple = TRUE,
                options = pickerOptions(actionsBox = TRUE, 
                                        selectedTextFormat = paste0("count > ", length(options)-1),
                                        countSelectedText = "All selected") 
    )
  })
  
  datasetTrans_mc <- eventReactive(input$mc_transform,{
    Mailchimp_data <- getData_mc()
    
    CleanMailchimp <- Mailchimp_data[intersect(c(MailchimpVariables, input$mc_add_field), names(Mailchimp_data))] %>%
      setnames(old = c("First_Name", "Last_Name", "Email_Address", "Address", "Phone_Number", "CC", "REGION", "NOTES"),
               
               new = c("First_Name", "Last_Name", "Email", "Full_address", "Home_phone", "Country", "State", "user_notes"),
               
               skip_absent = TRUE)
    
    CleanMailchimp$State <- toupper(CleanMailchimp$State)
    
    CleanMailchimp$Country <- toupper(CleanMailchimp$Country)
    
    for (k in which(CleanMailchimp$First_Name == "" | is.na(CleanMailchimp$First_Name))) {
      CleanMailchimp$First_Name[k] <- CleanMailchimp$Last_Name[k]
      CleanMailchimp$Last_Name[k] <- NA
    }
    
    if ("First_Name" %in% names(CleanMailchimp)) {
      CleanMailchimp$group_no_name <- ifelse(CleanMailchimp$First_Name == "" | is.na(CleanMailchimp$First_Name), "yes", NA)
      
      CleanMailchimp$First_Name <- ifelse(CleanMailchimp$First_Name == "" | is.na(CleanMailchimp$First_Name), CleanMailchimp$Email, CleanMailchimp$First_Name)
      
    } else {
      CleanMailchimp$First_Name <- CleanMailchimp$Email
      
      CleanMailchimp$group_no_name <- "Yes"
    }
    
    ifelse(input$mc_type == "Unsubscribed", CleanMailchimp$receive_emails <- FALSE, NA)
    
    CleanMailchimp[,paste("group_",input$mc_type,sep="")]<- "yes"
    
    mc_order <- c("First_Name", "Last_Name", "Email", "Full_address", "Home_phone", "Country", "State", "user_notes",
                  "group_Subscribed","group_Cleaned","group_Unsubscribed","group_no_name")
    
    CleanMailchimp <- select(CleanMailchimp, intersect(c(mc_order, input$mc_add_field), names(CleanMailchimp)))
    
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
      read.csv(input$cc_datafile$datapath,stringsAsFactors = FALSE)
    }
  })
  
  observeEvent(input$cc_datafile, {
    updateTabsetPanel(session, "ccTab", selected = "Original data")
  })
  
  output$cc_data <- renderDataTable(
    datatable(getData_cc(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  )
  
  CCVariables <- c()
  
  sep <- reactive({
    input$group_sep
  })
  
  output$CC_group_field <- renderUI({
    
    options <- setdiff(names(getData_cc()),CCVariables)
    
    pickerInput(inputId = "cc_group_field", label = "Field for Groups",
                choices = options,
                multiple = FALSE,
                options = pickerOptions(actionsBox = TRUE, 
                                        selectedTextFormat = paste0("count > ", length(options)-1),
                                        countSelectedText = "All selected")
                
    )
    
    
  })
  
  datasetTrans_cc <- eventReactive(input$cc_transform,{
    ConstantC <- getData_cc()
    
    email_groups_name <- strsplit(unlist(ConstantC[input$cc_group_field]),sep()) %>%
      unlist() %>%
      trimws()%>%
      unique() %>%
      na.omit() %>%
      sort()
    
    group_len <- length(email_groups_name)
    
    num_groups_note <- paste(group_len, "unique groups create.")
    
    showNotification(num_groups_note, duration = 15,type = "message")
    
    return(email_groups_name)
    
  })
  
  observeEvent(input$cc_transform, {
    updateTabsetPanel(session, "ccTab", selected = "Group list")
  })
  
  output$cc_result <- renderDataTable({
    datatable(data.frame(datasetTrans_cc()), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  })
  
  ####
  
  group_result <- eventReactive(input$group_clean,{
    
    ConstantC <- getData_cc()
    emaillist <- ConstantC[input$cc_group_field]
    
    email_groups_name <- datasetTrans_cc()
    group_len <- length(email_groups_name)
    
    #Split the list column for mapping later
    pre_split <- data.frame(gsub(paste0(sep()," "),sep(),as.matrix(emaillist)))
    split_list <- separate(pre_split, 1, paste0("col", 1:group_len), sep = sep())
    split_list <- split_list[,colSums(is.na(split_list))<nrow(split_list)] %>%
      data.frame()
    
    
    result <- data.frame()
    result <- data.frame(emaillist, matrix(ncol = length(email_groups_name)))
    colnames(result)[-1] <- email_groups_name
    
    for (j in 1:max(1,ncol(split_list))){
      for (i in email_groups_name){
        result[which(split_list[,j] == i),i] <- i
      }
    }
    colnames(result)[-1] <- paste0("Group ",email_groups_name)
    
    return(result)
    
  })
  
  observeEvent(input$group_clean, {
    updateTabsetPanel(session, "ccTab", selected = "Group result")
  })
  
  output$group_result <- renderDataTable({
    datatable(group_result(), class = 'cell-border stripe nowrap', options = list(pageLength = 15))
  })
  
  output$cc_downloadData <- downloadHandler(
    filename = "emaillist_result.xlsx",
    content = function(file){
      write.csv(datasetTrans_cc(),file,na = "")
      
      write.xlsx(group_result, file, sheetName="clean data",showNA=FALSE)
      write.xlsx(datasetTrans_cc, file, sheetName="group name", append = T)
    }
  )
  
}###function ending
)
