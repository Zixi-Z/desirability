#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(desirability2)
library(desiR)

# UI definition
ui <- fluidPage(
  titlePanel("CSV File Upload and Desirability Calculation"),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      uiOutput("varSelectUI"),  # UI for selecting variables
      uiOutput("packageSelectUI"),  # UI for selecting the package
      uiOutput("responseDefUI"),  # UI for defining responses
      actionButton("calcButton", "Calculate Desirabilities", style = "width:100%")  # Action button for triggering calculations
    ),
    mainPanel(
      tableOutput("contents"),  # Display original dataset
      tableOutput("top10")  # Display top 10 results after calculation
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Read the uploaded file reactively
  dataset <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE)
  })
  
  # Dynamically generate the variable selection UI
  output$varSelectUI <- renderUI({
    req(dataset())  # Ensure dataset is available
    checkboxGroupInput("responseVars", "Select Response Variables:", choices = names(dataset()))
  })
  
  # UI for selecting the package
  output$packageSelectUI <- renderUI({
    req(input$responseVars)
    radioButtons("packageChoice", "Select Package:", choices = c("Desirability2", "DesiR"))
  })
  
  # Dynamically generate UI for defining response types based on package choice
  output$responseDefUI <- renderUI({
    req(input$packageChoice)
    if (input$packageChoice == "Desirability2") {
      lapply(input$responseVars, function(var) {
        fluidRow(
          column(3, selectInput(paste0("type_", var), 
                                label = paste("Type for", var), 
                                choices = c("Max", "Min", "Target"))),
          column(3, numericInput(paste0("low_", var), "Low", value = NA)),
          column(3, numericInput(paste0("high_", var), "High", value = NA)),
          column(3, conditionalPanel(
            condition = sprintf("input.type_%s == 'Target'", var),
            numericInput(paste0("target_", var), "Target", value = NA),
            numericInput(paste0("scale_low_", var), "Scale Low", value = 1),
            numericInput(paste0("scale_high_", var), "Scale High", value = 1)
          )),
          column(3, conditionalPanel(
            condition = sprintf("input.type_%s != 'Target'", var),
            numericInput(paste0("scale_", var), "Scale", value = 1)
          )),
          column(3, numericInput(paste0("importance_", var), "Importance", value = 1))
        )
      })
    } else if (input$packageChoice == "DesiR") {
      lapply(input$responseVars, function(var) {
        fluidRow(
          column(3, selectInput(paste0("type_", var), "Type for variable:", choices = c("High", "Low", "Central", "Ends"))),
          column(3, numericInput(paste0("cut1_", var), "Cut 1", value = NA)),
          column(3, numericInput(paste0("cut2_", var), "Cut 2", value = NA)),
          # Conditional UI for Cut 3 and Cut 4
          conditionalPanel(
            condition = sprintf("input.type_%s == 'Central' || input.type_%s == 'Ends'", var, var),
            column(3, numericInput(paste0("cut3_", var), "Cut 3", value = NA)),
            column(3, numericInput(paste0("cut4_", var), "Cut 4", value = NA))
          ),
          column(3, numericInput(paste0("scale_", var), "Scale", value = 1)),
          column(3, numericInput(paste0("importance_", var), "Importance", value = 1))
        )
      })
    }
  })
  
  # Reactive value to control the display of the original dataset
  showOriginalData <- reactiveVal(TRUE)
  
  # Function to format the data for display
  formatDataForDisplay <- function(data) {
    data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], function(x) format(x, digits = 4, nsmall = 3))
    data
  }
  
  # Display the original dataset (first 20 rows)
  output$contents <- renderTable({
    if(showOriginalData()) {
      formatDataForDisplay(head(dataset(), 20))
    }
  })
  
  # Function to format desirability columns for display
  formatDesirabilityForDisplay <- function(data) {
    desirability_cols <- grep("^d_", names(data), value = TRUE)
    data[desirability_cols] <- lapply(data[desirability_cols], function(x) format(round(x, 3), nsmall = 3))
    data$D <- format(round(data$D, 3), nsmall = 3)  # Apply to overall desirability 'D' column
    data
  }
  
  # Observe event for the action button
  observeEvent(input$calcButton, {
    req(dataset(), input$responseVars, input$packageChoice)
    data <- dataset()
    
    if (input$packageChoice == "Desirability2") {
      # Logic for desirability2 calculations
      for(var in input$responseVars) {
        type <- input[[paste0("type_", var)]]
        importance <- input[[paste0("importance_", var)]]
        if(type == "Max") {
          scale_value <- input[[paste0("scale_", var)]]
          data[[paste0("d_", var)]] <- d_max(data[[var]], input[[paste0("low_", var)]], input[[paste0("high_", var)]], scale = scale_value)
        } else if(type == "Min") {
          scale_value <- input[[paste0("scale_", var)]]
          data[[paste0("d_", var)]] <- d_min(data[[var]], input[[paste0("low_", var)]], input[[paste0("high_", var)]], scale = scale_value)
        } else if(type == "Target") {
          data[[paste0("d_", var)]] <- d_target(data[[var]], input[[paste0("low_", var)]], input[[paste0("target_", var)]], input[[paste0("high_", var)]], input[[paste0("scale_low_", var)]], input[[paste0("scale_high_", var)]])
        }
        data[[paste0("w_", var)]] <- importance
      }
    }
    else if (input$packageChoice == "DesiR") {
      for (var in input$responseVars) {
        type <- input[[paste0("type_", var)]]
        cut1 <- input[[paste0("cut1_", var)]]
        cut2 <- input[[paste0("cut2_", var)]]
        scale <- input[[paste0("scale_", var)]]
        importance <- ifelse(is.null(input[[paste0("importance_", var)]]), 1, input[[paste0("importance_", var)]])
        
        if (type == "High") {
          data[[paste0("d_", var)]] <- d.high(data[[var]], cut1, cut2, scale = scale)
        } else if (type == "Low") {
          data[[paste0("d_", var)]] <- d.low(data[[var]], cut1, cut2, scale = scale)
        } else if (type %in% c("Central", "Ends")) {
          cut3 <- input[[paste0("cut3_", var)]]
          cut4 <- input[[paste0("cut4_", var)]]
          if (type == "Central") {
            data[[paste0("d_", var)]] <- d.central(data[[var]], cut1, cut2, cut3, cut4, scale = scale)
          } else {  # type == "Ends"
            data[[paste0("d_", var)]] <- d.ends(data[[var]], cut1, cut2, cut3, cut4, scale = scale)
          }
        }
        data[[paste0("w_", var)]] <- importance
      }
    }
    
    # Calculate Overall Desirability
    d_cols <- names(data)[grepl("^d_", names(data))]
    w_cols <- names(data)[grepl("^w_", names(data))]
    if (input$packageChoice == "Desirability2") {
      data$D <- apply(data[, d_cols], 1, function(x) {
        w_values <- data[, w_cols]
        prod(x ^ w_values)^(1/sum(w_values, na.rm = TRUE))
      })
    } else if (input$packageChoice == "DesiR") {
      weights <- data[1, w_cols]
      data$D <- do.call(d.overall, c(data[, d_cols], list(weights = as.numeric(weights))))
    }
    
    # Sort and select the top 10 rows
    top_10 <- data %>% arrange(desc(D)) %>% head(10) %>% select(-matches("^w_"))
    
    # Format the desirability columns for display
    top_10 <- formatDesirabilityForDisplay(top_10)
    
    # Hide the original dataset and output the top 10 rows
    showOriginalData(FALSE)
    output$top10 <- renderTable(top_10)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
