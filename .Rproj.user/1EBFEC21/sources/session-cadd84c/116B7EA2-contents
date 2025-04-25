# modules/six_arm_module.R

# UI Module
sixArmUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Six-Arm Olfactometer", class = "large-text"),
    br(),
    fluidRow(
      column(6, 
             h4("Trial Timer", class = "large-text"), 
             verbatimTextOutput(ns("trial_timer")),
             # Simple manual progress bar (no package dependency)
             div(class = "progress",
                 div(id = ns("time_progress"), class = "progress-bar",
                     role = "progressbar", style = "width: 0%;",
                     "aria-valuenow" = "0", "aria-valuemin" = "0", "aria-valuemax" = "100",
                     "0%")
             )
      )
    ),
    # Fixed button layout
    div(
      class = "button-container",
      div(class = "button-row",
          actionButton(ns("start"), "START RECORDING", class = "btn btn-start"),
          actionButton(ns("stop"), "STOP RECORDING", class = "btn btn-stop"),
          actionButton(ns("reset_trial"), "RESET TRIAL", class = "btn btn-reset")
      ),
      div(class = "button-row",
          actionButton(ns("add_trial"), "ADD NEW TRIAL", class = "btn btn-add"),
          downloadButton(ns("downloadData"), "DOWNLOAD DATA AS CSV", class = "btn btn-download")
      )
    ),
    tags$br(), 
    
    # Data entry section
    fluidRow(
      column(12, 
             h4("Data Entry", class = "large-text"),
             div(class = "data-entry-container",
                 fluidRow(
                   column(4, 
                          div(class = "arm-count-inputs",
                              numericInput(ns("arm1_count"), "Arm 1 Count:", 0, min = 0),
                              numericInput(ns("arm2_count"), "Arm 2 Count:", 0, min = 0),
                              numericInput(ns("arm3_count"), "Arm 3 Count:", 0, min = 0)
                          )
                   ),
                   column(4, 
                          div(class = "arm-count-inputs",
                              numericInput(ns("arm4_count"), "Arm 4 Count:", 0, min = 0),
                              numericInput(ns("arm5_count"), "Arm 5 Count:", 0, min = 0),
                              numericInput(ns("arm6_count"), "Arm 6 Count:", 0, min = 0)
                          )
                   ),
                   column(4, 
                          div(class = "arm-count-inputs",
                              numericInput(ns("central_count"), "Center Count:", 0, min = 0),
                              div(style = "margin-top: 15px;"),
                              div(
                                style = "text-align: center; margin-top: 15px;",
                                actionButton(ns("submit"), "Submit Counts", class = "btn btn-primary btn-block")
                              )
                          )
                   )
                 )
             )
      )
    ),
    
    tags$br(),
    
    # Use tabs for different data views
    tabsetPanel(
      tabPanel("Summary Table",
               h4("Trial Summary", class = "large-text"),
               tableOutput(ns("results_table")),
               tags$div(class = "export-options",
                        downloadButton(ns("downloadSummary"), "Download Summary", 
                                       class = "btn btn-sm btn-download")
               )
      ),
      tabPanel("Visualization",
               h4("Distribution by Arm", class = "large-text"),
               plotOutput(ns("distribution_plot"), height = "300px"),
               h4("Trial Comparison", class = "large-text"),
               plotOutput(ns("trial_comparison_plot"), height = "300px")
      )
    )
  )
}

# Server Module
sixArmServer <- function(id, parent_input, shared_timer = NULL) {
  moduleServer(id, function(input, output, session) {
    # Use the ns function to create namespaced IDs
    ns <- session$ns
    
    # Initialize reactive values
    arms <- reactiveValues(
      data = data.frame(
        Trial = character(0),
        Arm = character(0),
        Odour_Source = character(0),
        Count = numeric(0),
        Timestamp = character(0),
        stringsAsFactors = FALSE
      ),
      start_time = NULL,
      trial_running = FALSE,
      elapsed_time = 0,
      total_elapsed_time = 0,
      max_time = 300,  # Default max time (5 minutes)
      # Add default odour sources
      arm1_odour = "Odour A",
      arm2_odour = "Odour B",
      arm3_odour = "Odour C",
      arm4_odour = "Odour D",
      arm5_odour = "Odour E",
      arm6_odour = "Odour F",
      center_odour = "Center"
    )
    
    # Get trial duration from parent input
    observe({
      if (!is.null(parent_input$trial_duration_six)) {
        arms$max_time <- as.numeric(parent_input$trial_duration_six)
      }
    })
    
    # Start recording
    observeEvent(input$start, {
      # Set default values if inputs are empty
      arms$arm1_odour <- if (parent_input$arm1_six_odour == "") "Odour A" else parent_input$arm1_six_odour
      arms$arm2_odour <- if (parent_input$arm2_six_odour == "") "Odour B" else parent_input$arm2_six_odour
      arms$arm3_odour <- if (parent_input$arm3_six_odour == "") "Odour C" else parent_input$arm3_six_odour
      arms$arm4_odour <- if (parent_input$arm4_six_odour == "") "Odour D" else parent_input$arm4_six_odour
      arms$arm5_odour <- if (parent_input$arm5_six_odour == "") "Odour E" else parent_input$arm5_six_odour
      arms$arm6_odour <- if (parent_input$arm6_six_odour == "") "Odour F" else parent_input$arm6_six_odour
      
      arms$start_time <- Sys.time()
      arms$trial_running <- TRUE
      
      # Update UI to show trial is running
      shinyjs::addClass(selector = "body", class = "recording-active")
      
      # Disable inputs during recording
      shinyjs::disable("start")
      shinyjs::enable("stop")
      
      # Show a notification
      showNotification(
        "Recording started. Timer running...",
        type = "message",
        duration = 3
      )
      
      # Reset progress bar
      shinyjs::runjs(sprintf(
        "$('#%s').find('.progress-bar').css('width', '0%%').attr('aria-valuenow', 0).text('0%%');",
        ns("time_progress")
      ))
    })
    
    # Stop recording
    observeEvent(input$stop, {
      if (arms$trial_running) {
        end_time <- Sys.time()
        duration <- as.numeric(difftime(end_time, arms$start_time, units = "secs"))
        arms$total_elapsed_time <- arms$total_elapsed_time + duration
      }
      
      arms$trial_running <- FALSE
      arms$start_time <- NULL
      
      # Update UI to show trial is stopped
      shinyjs::removeClass(selector = "body", class = "recording-active")
      
      # Re-enable inputs
      shinyjs::enable("start")
      shinyjs::disable("stop")
      
      # Show a notification
      showNotification(
        "Recording stopped. Enter insect counts and click Submit.",
        type = "message",
        duration = 3
      )
    })
    
    # Add new trial
    observeEvent(input$add_trial, {
      # Reset for new trial
      reset_trial_data()
      
      # Increment trial number
      new_trial_num <- as.numeric(parent_input$trial_number_six) + 1
      updateTextInput(session, "trial_number_six", value = as.character(new_trial_num))
      
      # Show notification
      showNotification(
        paste("Trial", parent_input$trial_number_six, "saved. Starting new trial", new_trial_num),
        type = "message",
        duration = 3
      )
    })
    
    # Reset trial
    observeEvent(input$reset_trial, {
      # Confirm reset
      showModal(modalDialog(
        title = "Confirm Reset",
        "Are you sure you want to reset the current trial? All unsaved data for this trial will be lost.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "Reset Trial", class = "btn btn-danger")
        )
      ))
    })
    
    # Confirm reset action
    observeEvent(input$confirm_reset, {
      reset_trial_data()
      reset_counts()
      removeModal()
      showNotification("Trial reset successfully.", type = "message", duration = 3)
    })
    
    # Auto-stop timer when reaching max time
    observe({
      if (!is.null(shared_timer)) {
        shared_timer()
      } else {
        invalidateLater(1000)
      }
      
      if (arms$trial_running) {
        current_time <- Sys.time()
        elapsed <- as.numeric(difftime(current_time, arms$start_time, units = "secs")) + 
          arms$total_elapsed_time
        
        # Check if we've exceeded the max time
        if (elapsed >= arms$max_time) {
          # Auto-stop the recording
          arms$trial_running <- FALSE
          arms$total_elapsed_time <- arms$max_time
          arms$start_time <- NULL
          
          # Update UI
          shinyjs::removeClass(selector = "body", class = "recording-active")
          shinyjs::enable("start")
          shinyjs::disable("stop")
          
          # Show notification
          showNotification(
            "Trial automatically stopped at time limit.",
            type = "message",
            duration = 5
          )
        }
      }
    })
    
    # Submit counts
    observeEvent(input$submit, {
      # Convert counts to numeric
      arm1_count <- as.numeric(input$arm1_count)
      arm2_count <- as.numeric(input$arm2_count)
      arm3_count <- as.numeric(input$arm3_count)
      arm4_count <- as.numeric(input$arm4_count)
      arm5_count <- as.numeric(input$arm5_count)
      arm6_count <- as.numeric(input$arm6_count)
      central_count <- as.numeric(input$central_count)
      
      # Calculate total and validate against released count
      total_count <- arm1_count + arm2_count + arm3_count + arm4_count + 
        arm5_count + arm6_count + central_count
      expected_count <- as.numeric(parent_input$num_released_six)
      
      if (total_count != expected_count) {
        showModal(modalDialog(
          title = "Error",
          paste("The total count (", total_count, ") does not match the number of insects released (", 
                expected_count, ").", sep = ""),
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        # Current timestamp
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        # Add data for each arm
        arms$data <- arms$data %>%
          add_row(Trial = parent_input$trial_number_six, Arm = "Arm 1", 
                  Odour_Source = arms$arm1_odour, Count = arm1_count, Timestamp = timestamp) %>%
          add_row(Trial = parent_input$trial_number_six, Arm = "Arm 2", 
                  Odour_Source = arms$arm2_odour, Count = arm2_count, Timestamp = timestamp) %>%
          add_row(Trial = parent_input$trial_number_six, Arm = "Arm 3", 
                  Odour_Source = arms$arm3_odour, Count = arm3_count, Timestamp = timestamp) %>%
          add_row(Trial = parent_input$trial_number_six, Arm = "Arm 4", 
                  Odour_Source = arms$arm4_odour, Count = arm4_count, Timestamp = timestamp) %>%
          add_row(Trial = parent_input$trial_number_six, Arm = "Arm 5", 
                  Odour_Source = arms$arm5_odour, Count = arm5_count, Timestamp = timestamp) %>%
          add_row(Trial = parent_input$trial_number_six, Arm = "Arm 6", 
                  Odour_Source = arms$arm6_odour, Count = arm6_count, Timestamp = timestamp) %>%
          add_row(Trial = parent_input$trial_number_six, Arm = "Center", 
                  Odour_Source = "Center", Count = central_count, Timestamp = timestamp)
        
        # Show success notification
        showNotification(
          "Counts submitted successfully!",
          type = "message",
          duration = 3
        )
        
        # Reset count inputs for next entry
        reset_counts()
      }
    })
    
    # Helper function to reset counts
    reset_counts <- function() {
      updateNumericInput(session, "arm1_count", value = 0)
      updateNumericInput(session, "arm2_count", value = 0)
      updateNumericInput(session, "arm3_count", value = 0)
      updateNumericInput(session, "arm4_count", value = 0)
      updateNumericInput(session, "arm5_count", value = 0)
      updateNumericInput(session, "arm6_count", value = 0)
      updateNumericInput(session, "central_count", value = 0)
    }
    
    # Helper function to reset trial data
    reset_trial_data <- function() {
      arms$start_time <- NULL
      arms$trial_running <- FALSE
      arms$elapsed_time <- 0
      arms$total_elapsed_time <- 0
      arms$data <- data.frame(
        Trial = character(0),
        Arm = character(0),
        Odour_Source = character(0),
        Count = numeric(0),
        Timestamp = character(0),
        stringsAsFactors = FALSE
      )
      
      # Reset progress bar
      shinyjs::runjs(sprintf(
        "$('#%s').find('.progress-bar').css('width', '0%%').attr('aria-valuenow', 0).text('0%%');",
        ns("time_progress")
      ))
    }
    
    # Results table
    output$results_table <- renderTable({
      if (nrow(arms$data) == 0) {
        return(data.frame(
          Trial = character(0),
          Arm = character(0),
          "Odour Source" = character(0),
          Count = numeric(0),
          Timestamp = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      arms$data %>%
        select(
          Trial,
          Arm,
          "Odour Source" = Odour_Source,
          Count,
          Timestamp
        )
    })
    
    # Distribution plot
    output$distribution_plot <- renderPlot({
      if (nrow(arms$data) == 0) {
        plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "",
             main = "No data available yet")
        text(0.5, 0.5, "Submit counts to see visualization", cex = 1.2)
        return()
      }
      
      # Calculate percentages for arms
      plot_data <- arms$data %>%
        filter(Arm != "Center") %>%  # Exclude center for this plot
        group_by(Trial, Odour_Source) %>%
        summarize(Count = sum(Count), .groups = 'drop') %>%
        group_by(Trial) %>%
        mutate(Percentage = Count / sum(Count) * 100)
      
      # Create plot
      barplot(
        height = plot_data$Percentage,
        names.arg = plot_data$Odour_Source,
        col = rainbow(length(unique(plot_data$Odour_Source))),
        main = "Percentage Distribution by Odour Source (Excluding Center)",
        xlab = "Odour Source",
        ylab = "Percentage (%)",
        ylim = c(0, 100),
        las = 1
      )
      
      # Add grid lines
      abline(h = seq(0, 100, by = 20), col = "lightgray", lty = "dotted")
    })
    
    # Trial comparison plot
    output$trial_comparison_plot <- renderPlot({
      if (nrow(arms$data) == 0) {
        plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "",
             main = "No data available yet")
        text(0.5, 0.5, "Submit counts to see visualization", cex = 1.2)
        return()
      }
      
      # Prepare data grouping arm types
      plot_data <- arms$data %>%
        mutate(Arm_Group = case_when(
          Arm == "Center" ~ "Center",
          TRUE ~ "Arms"
        )) %>%
        group_by(Trial, Arm_Group, Arm, Odour_Source) %>%
        summarize(Count = sum(Count), .groups = 'drop')
      
      # Create plot with faceting by arm group
      if (length(unique(plot_data$Trial)) > 1) {
        # Multiple trials - do a comparison
        plot_data_summary <- plot_data %>%
          filter(Arm != "Center") %>%
          group_by(Trial, Odour_Source) %>%
          summarize(Count = sum(Count), .groups = 'drop')
        
        barplot(
          height = plot_data_summary$Count,
          names.arg = paste(plot_data_summary$Trial, "-", plot_data_summary$Odour_Source),
          col = rainbow(length(unique(plot_data_summary$Odour_Source))),
          main = "Count Comparison Across Trials",
          xlab = "Trial - Odour Source",
          ylab = "Count",
          las = 2
        )
        
        # Add grid lines
        max_y <- max(plot_data_summary$Count)
        abline(h = seq(0, max_y, length.out = 6), col = "lightgray", lty = "dotted")
      } else {
        # Single trial - show count by arm
        pie(
          plot_data$Count,
          labels = paste(plot_data$Arm, " (", plot_data$Count, ")", sep = ""),
          col = rainbow(nrow(plot_data)),
          main = paste("Distribution for Trial", unique(plot_data$Trial))
        )
      }
    })
    
    # Timer output and progress bar update
    observe({
      if (!is.null(shared_timer)) {
        shared_timer()
      } else {
        invalidateLater(1000)
      }
      
      # Update timer display
      if (arms$trial_running) {
        current_time <- Sys.time()
        elapsed <- as.numeric(difftime(current_time, arms$start_time, units = "secs")) + 
          arms$total_elapsed_time
        
        output$trial_timer <- renderText({
          paste0(format_time(elapsed), " / ", format_time(arms$max_time))
        })
        
        # Update progress bar
        progress_pct <- min(100, elapsed / arms$max_time * 100)
        shinyjs::runjs(sprintf(
          "$('#%s').find('.progress-bar').css('width', '%f%%').attr('aria-valuenow', %f).text('%d%%');",
          ns("time_progress"), progress_pct, progress_pct, round(progress_pct)
        ))
      } else {
        output$trial_timer <- renderText({
          paste0(format_time(arms$total_elapsed_time), " / ", format_time(arms$max_time))
        })
      }
    })
    
    # Helper function to format time
    format_time <- function(seconds) {
      hours <- floor(seconds / 3600)
      mins <- floor((seconds %% 3600) / 60)
      secs <- round(seconds %% 60)
      
      if (hours > 0) {
        sprintf("%02d:%02d:%02d", hours, mins, secs)
      } else {
        sprintf("%02d:%02d", mins, secs)
      }
    }
    
    # Download handler for summary
    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste("six_arm_summary_", parent_input$trial_number_six, "_", 
              format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
      },
      content = function(file) {
        # Get the summary data
        summary_data <- arms$data %>%
          group_by(Trial, Arm, Odour_Source) %>%
          summarize(Count = sum(Count), .groups = 'drop')
        
        write.csv(summary_data, file, row.names = FALSE)
      }
    )
    
    # Main download handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("six_arm_olfactometer_data_", parent_input$trial_number_six, "_", 
              format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
      },
      content = function(file) {
        # Try to create an Excel file with multiple sheets
        tryCatch({
          # If openxlsx package is available
          if (requireNamespace("openxlsx", quietly = TRUE)) {
            # Create a workbook with multiple sheets
            wb <- openxlsx::createWorkbook()
            
            # Prepare summary data
            summary_data <- arms$data %>%
              group_by(Trial, Arm, Odour_Source) %>%
              summarize(Count = sum(Count), .groups = 'drop')
            
            # Calculate relative preferences (percentages)
            preference_data <- arms$data %>%
              filter(Arm != "Center") %>%
              group_by(Trial) %>%
              mutate(Total = sum(Count)) %>%
              group_by(Trial, Arm, Odour_Source, Total) %>%
              summarize(
                Count = sum(Count),
                Percentage = round(Count / first(Total) * 100, 2),
                .groups = 'drop'
              ) %>%
              select(-Total)
            
            # Add sheets and write data
            openxlsx::addWorksheet(wb, "Raw Data")
            openxlsx::addWorksheet(wb, "Summary")
            openxlsx::addWorksheet(wb, "Preferences")
            
            openxlsx::writeData(wb, sheet = "Raw Data", x = arms$data)
            openxlsx::writeData(wb, sheet = "Summary", x = summary_data)
            openxlsx::writeData(wb, sheet = "Preferences", x = preference_data)
            
            # Save workbook
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          } else {
            # Fallback to CSV
            write.csv(arms$data, file, row.names = FALSE)
          }
        }, error = function(e) {
          # Fallback to CSV if Excel export fails
          write.csv(arms$data, file, row.names = FALSE)
        })
      }
    )
  })
}