# app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinyjs)
library(later) # For countdown functionality

# Source the module files
source("modules/four_arm_module.R")
source("modules/two_arm_module.R")
source("modules/six_arm_module.R")

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$script(HTML("
    function attachKeyListener() {
      document.addEventListener('keydown', keyListener);
      console.log('Key listener attached');
    }
    function detachKeyListener() {
      document.removeEventListener('keydown', keyListener);
      console.log('Key listener detached');
    }
    function keyListener(event) {
      // Customize key handling based on current olfactometer type
      const olfactometerType = document.getElementById('olfactometer_type').value;
      let validKeys = ['1', '2', '3', '4', '5'];
      
      if (olfactometerType === 'six') {
        validKeys = ['1', '2', '3', '4', '5', '6', '7'];
      } else if (olfactometerType === 'two') {
        validKeys = ['1', '2', '5'];
      }
      
      if(validKeys.includes(event.key)) {
        Shiny.setInputValue('key', {key: event.key, time: new Date().toISOString()}, {priority: 'event'});
      } else {
        console.log('Invalid key pressed: ' + event.key);
      }
    }

    // Handle visibility changes to ensure key listener is properly attached/detached
    document.addEventListener('visibilitychange', function() {
      if (document.visibilityState === 'visible') {
        attachKeyListener();
      } else {
        detachKeyListener();
      }
    });

    Shiny.addCustomMessageHandler('attachKeyListener', function(message) {
      attachKeyListener();
    });

    Shiny.addCustomMessageHandler('detachKeyListener', function(message) {
      detachKeyListener();
    });
  ")),
  includeCSS("custom.css"),
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'https://fonts.googleapis.com/css?family=Montserrat&display=swap'),
    tags$link(rel = 'icon', type = 'image/png', href = 'HAU-shield.png'),
    # Add Font Awesome for icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    # Add viewport meta tag for better mobile responsiveness
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
  ),
  titlePanel(
    title = tags$div(
      class = "title-panel",
      tags$div(
        class = "main-container",
        tags$div(
          class = "left-side",
          tags$div(
            tags$span("olfactometeR", class = "left-side-title")
          ),
          tags$div(
            tags$span("An App to Record and Analyse Insect Behavioural Responses in Olfactometer Bioassays", class = "left-side-subtitle")
          )
        ),
        tags$div(
          class = "right-side",
          tags$a(
            href = 'https://www.harper-adams.ac.uk/',
            tags$img(
              src = 'HAU.png',  
              height = 70,
              width = 215
            ),
            target = '_blank',
            class = 'right-side-logo'
          )
        )
      )
    ),
    windowTitle = 'olfactometeR'
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Olfactometer Type", class = "large-text"),
      selectInput("olfactometer_type", "Select Olfactometer Type:", 
                  choices = list("Four-Arm Olfactometer" = "four", 
                                 "Two-Arm Olfactometer" = "two", 
                                 "Six-Arm Olfactometer" = "six")),
      
      # Four-arm olfactometer inputs
      conditionalPanel(
        condition = "input.olfactometer_type == 'four'",
        h3("Arm Assignments", class = "large-text"),
        fluidRow(
          column(4, tags$span("Arm 1", class = "arm-key"),
                 tags$div("Key 1", align = "left", class = "key-label")),
          column(8, textInput("arm1_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 2", class = "arm-key"),
                 tags$div("Key 2", align = "left", class = "key-label")),
          column(8, textInput("arm2_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 3", class = "arm-key"),
                 tags$div("Key 3", align = "left", class = "key-label")),
          column(8, textInput("arm3_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 4", class = "arm-key"),
                 tags$div("Key 4", align = "left", class = "key-label")),
          column(8, textInput("arm4_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Center", class = "arm-key"),
                 tags$div("Key 5", align = "left", class = "key-label")),
          column(8, textInput("arm5_odour", "Center:", value = "Center"))
        ),
        
        # Add timer settings
        h3("Timer Settings", class = "large-text"),
        numericInput("trial_duration_four", "Trial Duration (seconds):", 
                     value = 300, min = 10, max = 3600),
        checkboxInput("auto_stop_four", "Auto-stop at time limit", value = TRUE),
        checkboxInput("use_countdown_four", "Use countdown before start", value = FALSE),
        conditionalPanel(
          condition = "input.use_countdown_four == true",
          numericInput("countdown_seconds_four", "Countdown seconds:", 
                       value = 3, min = 1, max = 10)
        ),
        
        textInput("trial_number_four", "Trial Number", value = "1"),
        actionButton("randomize_four_arm", "Randomise Arm Assignments", 
                     class = "btn btn-info btn-block mt-3")
      ),
      
      # Two-arm olfactometer inputs
      conditionalPanel(
        condition = "input.olfactometer_type == 'two'",
        textInput("num_released", "Number of Individuals Released", value = "1"),
        h3("Arm Assignments", class = "large-text"),
        fluidRow(
          column(4, tags$span("Arm 1", class = "arm-key")),
          column(8, textInput("two_arm_arm1_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 2", class = "arm-key")),
          column(8, textInput("two_arm_arm2_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Center", class = "arm-key")),
          column(8, textInput("two_arm_arm5_odour", "Center:", value = "Center"))
        ),
        
        # Add timer settings for two-arm
        h3("Timer Settings", class = "large-text"),
        numericInput("trial_duration_two", "Trial Duration (seconds):", 
                     value = 300, min = 10, max = 3600),
        
        textInput("trial_number_two", "Trial Number", value = "1"),
        actionButton("randomize_two_arm", "Randomize Arm Assignments", 
                     class = "btn btn-info btn-block mt-3")
      ),
      
      # Six-arm olfactometer inputs
      conditionalPanel(
        condition = "input.olfactometer_type == 'six'",
        textInput("num_released_six", "Number of Individuals Released", value = "1"),
        h3("Arm Assignments", class = "large-text"),
        fluidRow(
          column(4, tags$span("Arm 1", class = "arm-key")),
          column(8, textInput("arm1_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 2", class = "arm-key")),
          column(8, textInput("arm2_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 3", class = "arm-key")),
          column(8, textInput("arm3_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 4", class = "arm-key")),
          column(8, textInput("arm4_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 5", class = "arm-key")),
          column(8, textInput("arm5_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 6", class = "arm-key")),
          column(8, textInput("arm6_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Center", class = "arm-key")),
          column(8, textInput("arm7_six_odour", "Center:", value = "Center"))
        ),
        
        # Add timer settings for six-arm
        h3("Timer Settings", class = "large-text"),
        numericInput("trial_duration_six", "Trial Duration (seconds):", 
                     value = 300, min = 10, max = 3600),
        
        textInput("trial_number_six", "Trial Number", value = "1"),
        actionButton("randomize_six_arm", "Randomize Arm Assignments", 
                     class = "btn btn-info btn-block mt-3")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Instructions", 
                           uiOutput("introduction"),
                           h3("Using the App"),
                           tags$ol(
                             tags$li("Select Olfactometer Type: Choose the type of olfactometer you are using from the dropdown menu in the sidebar."),
                             tags$li("Enter Arm Assignments: Depending on the selected olfactometer type, enter the odour source for each arm."),
                             tags$li("Set Trial Information: Provide the trial number and other relevant details."),
                             tags$li("Set Timer: Configure the trial duration and auto-stop settings."),
                             tags$li("Start Recording: Press the 'Start Recording' button to begin the trial."),
                             tags$li("Record Arm Entries: Press the number key corresponding to each arm (1-6) as insects move between arms."),
                             tags$li("Stop Recording: Press the 'Stop Recording' button to end the trial."),
                             tags$li("Review and Download Data: Review the trial summary and download the data as a CSV file."),
                             tags$li("Reset or Add New Trial: Use the 'Reset Trial' or 'Add New Trial' buttons as needed.")
                           ),
                           br(),
                           h3("Best Practices"),
                           HTML('<p>The following table summarises best practices for conducting insect olfactometer bioassays. These guidelines are based on the comprehensive review provided in the paper by <a href="https://doi.org/10.1111/eea.13351" target="_blank">Roberts et al. (2023)</a>, which outlines critical considerations to ensure the reliability and reproducibility of bioassay results.</p>'),
                           tableOutput("best_practices")
                  ),
                  tabPanel("Four-Arm Olfactometer",
                           fourArmUI("four_arm")
                  ),
                  tabPanel("Two-Arm Olfactometer",
                           twoArmUI("two_arm")
                  ),
                  tabPanel("Six-Arm Olfactometer",
                           sixArmUI("six_arm")
                  ),
                  tabPanel("Data Analysis",
                           h2("Data Analysis", class = "large-text"),
                           br(),
                           fileInput("analysis_file", "Upload CSV File for Analysis",
                                     accept = c("text/csv", 
                                                "text/comma-separated-values,text/plain", 
                                                ".csv")),
                           uiOutput("analysis_ui")
                  )
      )
    )
  ),
  tags$footer(
    class = "footer",
    div(class = "row",
        div(class = "col-sm-6", style = "text-align: left;",
            HTML("&copy; Joe Roberts and Ben Clunie 2024")
        ),
        div(class = "col-sm-6", style = "text-align: right;",
            tags$a(
              href = 'https://github.com/Dr-Joe-Roberts/olfactometer_app', 
              target = '_blank',
              tags$img(src = 'https://github.githubassets.com/favicons/favicon.png', 
                       height = '20', width = '20'),
              " GitHub Repository"
            ),
            span(" | Version 1.1.0")
        )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Create a shared reactive timer that all modules can use
  timer <- reactiveTimer(1000)
  
  # Tab synchronization (optimized)
  observeEvent(input$olfactometer_type, {
    updateTabsetPanel(session, "tabs", 
                      selected = paste0(input$olfactometer_type, "-Arm Olfactometer"))
  })
  
  observeEvent(input$tabs, {
    # Only update if it's an olfactometer tab (not instructions or analysis)
    tab_name <- input$tabs
    if (grepl("-Arm Olfactometer$", tab_name)) {
      olfactometer_type <- tolower(sub("-Arm Olfactometer$", "", tab_name))
      updateSelectInput(session, "olfactometer_type", selected = olfactometer_type)
    }
  })
  
  # Randomize arm assignments functionality for four-arm
  observeEvent(input$randomize_four_arm, {
    # Get current odour sources
    odour_labels <- sapply(paste0("arm", 1:4, "_odour"), function(id) input[[id]])
    odour_labels <- odour_labels[odour_labels != ""]
    
    # Use default if none provided
    if (length(odour_labels) == 0) {
      odour_labels <- c("Odour A", "Odour B", "Odour C", "Odour D")
    }
    
    # Generate randomized order
    random_order <- sample(odour_labels)
    
    # Update inputs
    for (i in 1:min(4, length(random_order))) {
      updateTextInput(session, paste0("arm", i, "_odour"), value = random_order[i])
    }
    
    # Show notification
    showNotification("Arm assignments randomized successfully.", type = "message", duration = 3)
  })
  
  # Similar randomization for two-arm
  observeEvent(input$randomize_two_arm, {
    odour_labels <- c(input$two_arm_arm1_odour, input$two_arm_arm2_odour)
    odour_labels <- odour_labels[odour_labels != ""]
    
    # Use default if none provided
    if (length(odour_labels) == 0) {
      odour_labels <- c("Treatment", "Control")
    }
    
    # Generate randomized order
    random_order <- sample(odour_labels)
    
    # Update inputs
    updateTextInput(session, "two_arm_arm1_odour", value = random_order[1])
    updateTextInput(session, "two_arm_arm2_odour", value = random_order[2])
    
    # Show notification
    showNotification("Arm assignments randomized successfully.", type = "message", duration = 3)
  })
  
  # Randomization for six-arm
  observeEvent(input$randomize_six_arm, {
    odour_labels <- sapply(paste0("arm", 1:6, "_six_odour"), function(id) input[[id]])
    odour_labels <- odour_labels[odour_labels != ""]
    
    # Use default if none provided
    if (length(odour_labels) == 0) {
      odour_labels <- c("Odour A", "Odour B", "Odour C", "Odour D", "Odour E", "Odour F")
    }
    
    # Generate randomized order
    random_order <- sample(odour_labels)
    
    # Update inputs
    for (i in 1:min(6, length(random_order))) {
      updateTextInput(session, paste0("arm", i, "_six_odour"), value = random_order[i])
    }
    
    # Show notification
    showNotification("Arm assignments randomized successfully.", type = "message", duration = 3)
  })
  
  # Call the module servers with the shared timer
  fourArmServer("four_arm", input, timer)
  twoArmServer("two_arm", input, timer)
  sixArmServer("six_arm", input, timer)
  
  # Introduction UI
  output$introduction <- renderUI({
    HTML('
    <br>
    <h2>Introduction</h2>
    <p>This Shiny web app is designed to help you collect data from insect olfactometer bioassays. 
    There are currently three olfactometer designs supported:</p>
    <ol>
      <li><strong>Four-arm olfactometers</strong>: Track real-time movement of individual insects between arms</li>
      <li><strong>Two-arm olfactometers</strong>: Record final positions of multiple insects</li>
      <li><strong>Six-arm olfactometers</strong>: Record final positions of multiple insects in complex multi-choice designs</li>
    </ol>
    <p>All data can be downloaded as CSV files for further analysis, and basic analysis functionality is built into the app.</p>
    <br>
    <h3>Key Features</h3>
    <ul>
      <li><strong>Timer Control</strong>: Set trial duration and use auto-stop functionality to ensure consistent timing</li>
      <li><strong>Randomization</strong>: Randomize arm assignments to reduce experimental bias</li>
      <li><strong>Visual Tracking</strong>: See real-time visual representation of insect location</li>
      <li><strong>Data Export</strong>: Download data in CSV format for further analysis</li>
      <li><strong>Data Analysis</strong>: Upload previously recorded data for basic analysis</li>
    </ul>
    <br>')
  })
  
  # Best practices table
  output$best_practices <- renderTable({
    data.frame(
      "Consideration" = c(
        "Olfactometer Design", 
        "Environment", 
        "Stimuli Presentation", 
        "Cleaning Procedures", 
        "Physiological State", 
        "Releasing Organisms"
      ),
      "Best Practices" = c(
        "Ensure the design suits the research question and species; use chemically inert materials; pilot test configurations.", 
        "Report and control temperature and humidity; use consistent lighting; minimise external stimuli.",
        "Avoid mechanical damage to biological materials; consider solvent properties for synthetic chemicals.",
        "Thoroughly clean between replicates; use fragrance-free detergents; appropriately clean or replace activated charcoal filters.",
        "Report and standardise the physiological state; account for time-of-day effects; use blocking in design and analysis.",
        "Decide between individual or group releases based on interaction and social behaviors; treat each group release as a single replicate."
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # Data Analysis Tab Functionality
  output$analysis_ui <- renderUI({
    req(input$analysis_file)
    
    # Try to read the file and determine type
    tryCatch({
      df <- read.csv(input$analysis_file$datapath, stringsAsFactors = FALSE)
      
      # Determine file type based on columns
      has_count <- "Count" %in% colnames(df) || "count" %in% colnames(df)
      has_duration <- "Duration" %in% colnames(df) || "duration" %in% colnames(df) || 
        "Total_Duration" %in% colnames(df) || "total_duration" %in% colnames(df)
      
      if (has_count) {
        file_type <- "count"
      } else if (has_duration) {
        file_type <- "duration"
      } else {
        file_type <- "unknown"
      }
      
      # UI for analysis options
      tagList(
        h3("File Analysis"),
        verbatimTextOutput("file_summary"),
        
        # Only show visualization options if we know the file type
        if (file_type != "unknown") {
          tagList(
            h3("Visualization"),
            selectInput("plot_type", "Select Plot Type:", 
                        choices = c("Bar Chart" = "bar", 
                                    "Pie Chart" = "pie", 
                                    "Box Plot" = "box")),
            conditionalPanel(
              condition = "input.plot_type == 'bar' || input.plot_type == 'box'",
              selectInput("group_by", "Group By:", 
                          choices = c("Arm" = "arm", 
                                      "Odour Source" = "odour", 
                                      "Trial" = "trial"))
            ),
            plotOutput("analysis_plot", height = "400px"),
            downloadButton("download_plot", "Download Plot", 
                           class = "btn btn-download")
          )
        } else {
          div(class = "alert alert-warning",
              "Unable to determine file type. Please ensure your CSV file contains standard olfactometer data.")
        }
      )
    }, error = function(e) {
      div(class = "alert alert-danger",
          "Error reading file: ", e$message,
          br(), br(),
          "Please ensure your file is a valid CSV with data from the olfactometer app.")
    })
  })
  
  # Summary of the uploaded file
  output$file_summary <- renderPrint({
    req(input$analysis_file)
    
    tryCatch({
      df <- read.csv(input$analysis_file$datapath, stringsAsFactors = FALSE)
      
      cat("File Summary:\n")
      cat("Number of rows:", nrow(df), "\n")
      cat("Number of columns:", ncol(df), "\n")
      cat("Column names:", paste(colnames(df), collapse = ", "), "\n\n")
      
      # Try to determine and show olfactometer type
      if ("Arm" %in% colnames(df) || "arm" %in% colnames(df)) {
        arm_col <- if ("Arm" %in% colnames(df)) "Arm" else "arm"
        arms <- unique(df[[arm_col]])
        arms <- arms[!grepl("Center", arms, ignore.case = TRUE)]
        
        cat("Olfactometer type: ")
        if (length(arms) <= 2) {
          cat("Two-Arm Olfactometer\n")
        } else if (length(arms) <= 4) {
          cat("Four-Arm Olfactometer\n")
        } else if (length(arms) <= 6) {
          cat("Six-Arm Olfactometer\n")
        } else {
          cat("Unknown\n")
        }
      }
      
      # Display basic summary statistics
      if ("Duration" %in% colnames(df) || "duration" %in% colnames(df) || 
          "Total_Duration" %in% colnames(df) || "total_duration" %in% colnames(df)) {
        # Find the duration column
        dur_col <- NULL
        for (col in c("Duration", "duration", "Total_Duration", "total_duration")) {
          if (col %in% colnames(df)) {
            dur_col <- col
            break
          }
        }
        
        if (!is.null(dur_col)) {
          cat("\nDuration Statistics:\n")
          dur_summary <- summary(df[[dur_col]])
          print(dur_summary)
        }
      }
      
      if ("Count" %in% colnames(df) || "count" %in% colnames(df)) {
        # Find the count column
        count_col <- if ("Count" %in% colnames(df)) "Count" else "count"
        
        cat("\nCount Statistics:\n")
        count_summary <- summary(df[[count_col]])
        print(count_summary)
      }
      
    }, error = function(e) {
      cat("Error analyzing file:", e$message, "\n")
      cat("Please ensure your file is a valid CSV with data from the olfactometer app.")
    })
  })
  
  # Analysis plot
  output$analysis_plot <- renderPlot({
    req(input$analysis_file, input$plot_type)
    
    tryCatch({
      df <- read.csv(input$analysis_file$datapath, stringsAsFactors = FALSE)
      
      # Determine key columns
      arm_col <- NULL
      for (col in c("Arm", "arm")) {
        if (col %in% colnames(df)) {
          arm_col <- col
          break
        }
      }
      
      odour_col <- NULL
      for (col in c("Odour Source", "Odour_Source", "odour_source")) {
        if (col %in% colnames(df)) {
          odour_col <- col
          break
        }
      }
      
      trial_col <- NULL
      for (col in c("Trial", "trial")) {
        if (col %in% colnames(df)) {
          trial_col <- col
          break
        }
      }
      
      value_col <- NULL
      for (col in c("Count", "count", "Duration", "duration", 
                    "Total_Duration", "total_duration", "Total_Entries", "total_entries")) {
        if (col %in% colnames(df)) {
          value_col <- col
          break
        }
      }
      
      # Make sure we have the necessary columns
      req(arm_col, value_col)
      
      # Plot based on the selected type
      if (input$plot_type == "bar") {
        # Group by selected dimension
        group_col <- switch(input$group_by,
                            "arm" = arm_col,
                            "odour" = odour_col,
                            "trial" = trial_col)
        
        # Group and summarize data
        if (!is.null(group_col) && group_col %in% colnames(df)) {
          plot_data <- aggregate(df[[value_col]], by = list(Group = df[[group_col]]), FUN = sum)
          colnames(plot_data) <- c("Group", "Value")
          
          # Remove center for cleaner visualization
          plot_data <- plot_data[!grepl("Center", plot_data$Group, ignore.case = TRUE), ]
          
          # Create bar plot
          barplot(plot_data$Value, names.arg = plot_data$Group, 
                  col = rainbow(nrow(plot_data)),
                  main = paste("Total", value_col, "by", group_col),
                  xlab = group_col, ylab = value_col)
        } else {
          # Fallback to arm if selected group is not available
          plot_data <- aggregate(df[[value_col]], by = list(Group = df[[arm_col]]), FUN = sum)
          colnames(plot_data) <- c("Group", "Value")
          
          # Remove center for cleaner visualization
          plot_data <- plot_data[!grepl("Center", plot_data$Group, ignore.case = TRUE), ]
          
          barplot(plot_data$Value, names.arg = plot_data$Group, 
                  col = rainbow(nrow(plot_data)),
                  main = paste("Total", value_col, "by", arm_col),
                  xlab = arm_col, ylab = value_col)
        }
      } else if (input$plot_type == "pie") {
        # Group by odour source if available, otherwise by arm
        group_col <- if (!is.null(odour_col) && odour_col %in% colnames(df)) odour_col else arm_col
        
        # Group and summarize data
        plot_data <- aggregate(df[[value_col]], by = list(Group = df[[group_col]]), FUN = sum)
        colnames(plot_data) <- c("Group", "Value")
        
        # Remove center for cleaner visualization
        plot_data <- plot_data[!grepl("Center", plot_data$Group, ignore.case = TRUE), ]
        
        # Create pie chart
        pie(plot_data$Value, labels = plot_data$Group, 
            col = rainbow(nrow(plot_data)),
            main = paste("Distribution of", value_col, "by", group_col))
      } else if (input$plot_type == "box") {
        # Group by selected dimension
        group_col <- switch(input$group_by,
                            "arm" = arm_col,
                            "odour" = odour_col,
                            "trial" = trial_col)
        
        # Make sure we have the group column
        if (!is.null(group_col) && group_col %in% colnames(df)) {
          # Remove center for cleaner visualization
          plot_data <- df[!grepl("Center", df[[group_col]], ignore.case = TRUE), ]
          
          # Create box plot
          boxplot(plot_data[[value_col]] ~ plot_data[[group_col]], 
                  col = rainbow(length(unique(plot_data[[group_col]]))),
                  main = paste("Distribution of", value_col, "by", group_col),
                  xlab = group_col, ylab = value_col)
        } else {
          # Fallback to arm if selected group is not available
          plot_data <- df[!grepl("Center", df[[arm_col]], ignore.case = TRUE), ]
          
          boxplot(plot_data[[value_col]] ~ plot_data[[arm_col]], 
                  col = rainbow(length(unique(plot_data[[arm_col]]))),
                  main = paste("Distribution of", value_col, "by", arm_col),
                  xlab = arm_col, ylab = value_col)
        }
      }
    }, error = function(e) {
      # Show error message in plot
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", 
           main = "Error Creating Plot")
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Download handler for the plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("olfactometer_analysis_", format(Sys.Date(), "%Y-%m-%d"), ".png", sep = "")
    },
    content = function(file) {
      # Set up the png device
      png(file, width = 800, height = 600, res = 100)
      
      # Recreate the plot
      tryCatch({
        df <- read.csv(input$analysis_file$datapath, stringsAsFactors = FALSE)
        
        # Determine key columns (same code as in renderPlot)
        # ... [repeat the column detection code] ...
        
        # [Repeat the plotting code from renderPlot]
        
      }, error = function(e) {
        # Show error message in plot
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", 
             main = "Error Creating Plot")
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      })
      
      # Close the device
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)