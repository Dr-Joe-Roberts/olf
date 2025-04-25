# modules/four_arm_module.R

# UI Module
fourArmUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Four-Arm Olfactometer", class = "large-text"),
    br(),
    fluidRow(
      column(6, 
             h4("Current Arm", class = "large-text"), 
             verbatimTextOutput(ns("current_arm")),
             uiOutput(ns("current_arm_display"))
      ),
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
    
    # Improved olfactometer visual representation
    fluidRow(
      column(12, 
             h4("Olfactometer Status", class = "large-text"),
             div(class = "olfactometer-visual",
                 # Channels
                 div(class = "channel channel-vertical channel-1"),
                 div(class = "channel channel-horizontal channel-2"),
                 div(class = "channel channel-vertical channel-3"),
                 div(class = "channel channel-horizontal channel-4"),
                 
                 # Arms with labels
                 div(id = ns("arm1_visual"), class = "arm arm1", "1"),
                 div(class = "arm-label arm1-label", textOutput(ns("arm1_label"))),
                 
                 div(id = ns("arm2_visual"), class = "arm arm2", "2"),
                 div(class = "arm-label arm2-label", textOutput(ns("arm2_label"))),
                 
                 div(id = ns("arm3_visual"), class = "arm arm3", "3"),
                 div(class = "arm-label arm3-label", textOutput(ns("arm3_label"))),
                 
                 div(id = ns("arm4_visual"), class = "arm arm4", "4"),
                 div(class = "arm-label arm4-label", textOutput(ns("arm4_label"))),
                 
                 div(id = ns("center_visual"), class = "arm center", "C")
             )
      )
    ),
    
    tags$br(),
    
    # Use tabs for different data views
    tabsetPanel(
      tabPanel("Summary Table",
               h4("Trial Summary", class = "large-text"),
               tableOutput(ns("arm_times")),
               tags$div(class = "export-options",
                        downloadButton(ns("downloadSummary"), "Download Summary", 
                                       class = "btn btn-sm btn-download")
               )
      ),
      tabPanel("Real-time Data",
               h4("Recorded Arm Entries", class = "large-text"),
               tableOutput(ns("raw_data")),
               tags$div(class = "export-options",
                        downloadButton(ns("downloadRaw"), "Download Raw Data", 
                                       class = "btn btn-sm btn-download")
               )
      )
    )
  )
}

# Server Module
fourArmServer <- function(id, parent_input, shared_timer = NULL) {
  moduleServer(id, function(input, output, session) {
    # Use the ns function to create namespaced IDs
    ns <- session$ns
    
    # Initialize reactive values
    arms <- reactiveValues(
      current_arm = "Center",
      start_time = NULL,
      times = data.frame(
        Arm = character(0),
        Duration = numeric(0),
        Count = integer(0),
        Odour_Source = character(0),
        Trial = character(0),
        Entry_Time = character(0),
        Exit_Time = character(0),
        stringsAsFactors = FALSE
      ),
      all_times = data.frame(
        Arm = character(0),
        Duration = numeric(0),
        Count = integer(0),
        Odour_Source = character(0),
        Trial = character(0),
        Entry_Time = character(0),
        Exit_Time = character(0),
        stringsAsFactors = FALSE
      ),
      trial_start = NULL,
      trial_running = FALSE,
      elapsed_time = 0,
      total_elapsed_time = 0,
      max_time = 300,  # Default max time (5 minutes)
      # Add default odour sources
      arm1_odour = "Odour A",
      arm2_odour = "Odour B",
      arm3_odour = "Odour C",
      arm4_odour = "Odour D",
      center_odour = "Center",
      movement_history = data.frame(
        From = character(0),
        To = character(0),
        Time = numeric(0),
        Trial = character(0),
        stringsAsFactors = FALSE
      )
    )
    
    # Get trial duration from parent input
    observe({
      if (!is.null(parent_input$trial_duration_four)) {
        arms$max_time <- as.numeric(parent_input$trial_duration_four)
      }
    })
    
    # Start recording
    observeEvent(input$start, {
      # Set default values if inputs are empty
      arms$arm1_odour <- if (parent_input$arm1_odour == "") "Odour A" else parent_input$arm1_odour
      arms$arm2_odour <- if (parent_input$arm2_odour == "") "Odour B" else parent_input$arm2_odour
      arms$arm3_odour <- if (parent_input$arm3_odour == "") "Odour C" else parent_input$arm3_odour
      arms$arm4_odour <- if (parent_input$arm4_odour == "") "Odour D" else parent_input$arm4_odour
      
      # Check if countdown is enabled
      if (!is.null(parent_input$use_countdown_four) && parent_input$use_countdown_four) {
        countdown_seconds <- as.numeric(parent_input$countdown_seconds_four)
        
        # Disable buttons during countdown
        shinyjs::disable("start")
        shinyjs::disable("stop")
        shinyjs::disable("reset_trial")
        shinyjs::disable("add_trial")
        
        # Start countdown
        output$current_arm_display <- renderUI({
          div(class = "countdown-display",
              "Recording will start in:",
              div(class = "countdown-number", countdown_seconds)
          )
        })
        
        # Update countdown every second
        for (i in 1:countdown_seconds) {
          local({
            local_i <- i
            delay <- local_i * 1000
            later::later(function() {
              remaining <- countdown_seconds - local_i
              if (remaining > 0) {
                output$current_arm_display <- renderUI({
                  div(class = "countdown-display",
                      "Recording will start in:",
                      div(class = "countdown-number", remaining)
                  )
                })
              } else {
                # Start actual recording after countdown
                start_recording()
              }
            }, delay)
          })
        }
      } else {
        # Start recording immediately
        start_recording()
      }
    })
    
    # Define the start_recording function
    start_recording <- function() {
      arms$start_time <- Sys.time()
      if (is.null(arms$trial_start)) {
        arms$trial_start <- Sys.time()
      }
      arms$trial_running <- TRUE
      arms$current_arm <- "Center"
      
      # Update UI to show trial is running
      shinyjs::addClass(selector = "body", class = "recording-active")
      
      # Attach key listener
      session$sendCustomMessage(type = "attachKeyListener", message = list())
      
      # Update buttons
      shinyjs::disable("start")
      shinyjs::enable("stop")
      
      # Show a notification
      showNotification(
        "Recording started. Press keys 1-5 to record arm movements.",
        type = "message",
        duration = 3
      )
      
      # Reset progress
      shinyjs::runjs(sprintf(
        "$('#%s').find('.progress-bar').css('width', '0%%').attr('aria-valuenow', 0).text('0%%');",
        ns("time_progress")
      ))
      
      # Update arm visuals
      update_arm_visuals("Center")
    }
    
    # Stop recording
    observeEvent(input$stop, {
      if (!is.null(arms$current_arm) && !is.null(arms$start_time)) {
        record_arm_duration()
      }
      arms$current_arm <- NULL
      arms$trial_running <- FALSE
      arms$start_time <- NULL
      
      # Update UI to show trial is stopped
      shinyjs::removeClass(selector = "body", class = "recording-active")
      
      # Detach key listener
      session$sendCustomMessage(type = "detachKeyListener", message = list())
      
      # Re-enable inputs
      shinyjs::enable("start")
      shinyjs::disable("stop")
      
      # Show a notification
      showNotification(
        "Recording stopped.",
        type = "message",
        duration = 3
      )
    })
    
    # Add new trial
    observeEvent(input$add_trial, {
      # First save current trial data
      arms$all_times <- bind_rows(arms$all_times, arms$times)
      
      # Reset for new trial
      reset_trial_data()
      
      # Increment trial number
      new_trial_num <- as.numeric(parent_input$trial_number_four) + 1
      updateTextInput(session, "trial_number_four", value = as.character(new_trial_num))
      
      # Show notification
      showNotification(
        paste("Trial", parent_input$trial_number_four, "saved. Starting new trial", new_trial_num),
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
      
      if (arms$trial_running && !is.null(parent_input$auto_stop_four) && parent_input$auto_stop_four) {
        current_time <- Sys.time()
        elapsed <- as.numeric(difftime(current_time, arms$start_time, units = "secs")) + 
          arms$total_elapsed_time
        
        # Check if we've exceeded the max time
        if (elapsed >= arms$max_time) {
          # Auto-stop the recording
          arms$trial_running <- FALSE
          
          # Record final arm duration
          if (!is.null(arms$current_arm) && !is.null(arms$start_time)) {
            record_arm_duration()
          }
          
          arms$current_arm <- NULL
          arms$start_time <- NULL
          
          # Update UI
          shinyjs::removeClass(selector = "body", class = "recording-active")
          session$sendCustomMessage(type = "detachKeyListener", message = list())
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
    
    # Key press handler with movement history tracking
    observeEvent(parent_input$key, {
      if (arms$trial_running) {
        prev_arm <- arms$current_arm
        new_arm <- switch(parent_input$key$key,
                          `1` = "Arm 1",
                          `2` = "Arm 2",
                          `3` = "Arm 3",
                          `4` = "Arm 4",
                          `5` = "Center",
                          arms$current_arm)
        
        if (new_arm != prev_arm) {
          # Record exit from previous arm
          if (!is.null(prev_arm) && !is.null(arms$start_time)) {
            curr_time <- Sys.time()
            duration <- as.numeric(difftime(curr_time, arms$start_time, units = "secs"))
            odour_source <- get_odour_source(prev_arm)
            
            # Add to times dataframe
            arms$times <- arms$times %>%
              add_row(
                Arm = prev_arm,
                Duration = round(duration, 2),
                Count = 1,
                Odour_Source = odour_source,
                Trial = parent_input$trial_number_four,
                Entry_Time = format(arms$start_time, "%Y-%m-%d %H:%M:%OS3"),
                Exit_Time = format(curr_time, "%Y-%m-%d %H:%M:%OS3")
              )
            
            # Update total elapsed time
            arms$total_elapsed_time <- arms$total_elapsed_time + duration
            
            # Add to movement history
            arms$movement_history <- arms$movement_history %>%
              add_row(
                From = prev_arm,
                To = new_arm,
                Time = as.numeric(difftime(curr_time, arms$trial_start, units = "secs")),
                Trial = parent_input$trial_number_four
              )
          }
          
          # Set start time for new arm
          arms$start_time <- Sys.time()
          arms$current_arm <- new_arm
          
          # Update visual indicator
          update_arm_visuals(new_arm)
        }
      }
    })
    
    # Helper function to update visual indicators
    update_arm_visuals <- function(current_arm) {
      # Clear all highlight classes
      for (arm in c("arm1_visual", "arm2_visual", "arm3_visual", "arm4_visual", "center_visual")) {
        shinyjs::removeClass(arm, "active")
      }
      
      # Add highlight to current arm
      if (current_arm == "Arm 1") {
        shinyjs::addClass("arm1_visual", "active")
      } else if (current_arm == "Arm 2") {
        shinyjs::addClass("arm2_visual", "active")
      } else if (current_arm == "Arm 3") {
        shinyjs::addClass("arm3_visual", "active")
      } else if (current_arm == "Arm 4") {
        shinyjs::addClass("arm4_visual", "active")
      } else if (current_arm == "Center") {
        shinyjs::addClass("center_visual", "active")
      }
    }
    
    # Helper function to record arm duration
    record_arm_duration <- function() {
      curr_time <- Sys.time()
      duration <- as.numeric(difftime(curr_time, arms$start_time, units = "secs"))
      odour_source <- get_odour_source(arms$current_arm)
      
      arms$times <- arms$times %>%
        add_row(
          Arm = arms$current_arm,
          Duration = round(duration, 2),
          Count = 1,
          Odour_Source = odour_source,
          Trial = parent_input$trial_number_four,
          Entry_Time = format(arms$start_time, "%Y-%m-%d %H:%M:%OS3"),
          Exit_Time = format(curr_time, "%Y-%m-%d %H:%M:%OS3")
        )
      arms$total_elapsed_time <- arms$total_elapsed_time + duration
    }
    
    # Helper function to get odour source
    get_odour_source <- function(arm) {
      switch(arm,
             "Arm 1" = arms$arm1_odour,
             "Arm 2" = arms$arm2_odour,
             "Arm 3" = arms$arm3_odour,
             "Arm 4" = arms$arm4_odour,
             "Center" = arms$center_odour)
    }
    
    # Helper function to reset trial data
    reset_trial_data <- function() {
      arms$current_arm <- "Center"
      arms$start_time <- NULL
      arms$times <- data.frame(
        Arm = character(0),
        Duration = numeric(0),
        Count = integer(0),
        Odour_Source = character(0),
        Trial = character(0),
        Entry_Time = character(0),
        Exit_Time = character(0),
        stringsAsFactors = FALSE
      )
      arms$trial_start <- NULL
      arms$elapsed_time <- 0
      arms$total_elapsed_time <- 0
      arms$movement_history <- data.frame(
        From = character(0),
        To = character(0),
        Time = numeric(0),
        Trial = character(0),
        stringsAsFactors = FALSE
      )
      
      # Reset visual indicators
      update_arm_visuals("Center")
      
      # Reset progress
      shinyjs::runjs(sprintf(
        "$('#%s').find('.progress-bar').css('width', '0%%').attr('aria-valuenow', 0).text('0%%');",
        ns("time_progress")
      ))
    }
    
    # Current arm output
    output$current_arm <- renderText({
      if(is.null(arms$current_arm)) {
        "Not Recording"
      } else {
        arms$current_arm
      }
    })
    
    # Current arm visual indicator
    output$current_arm_display <- renderUI({
      if (is.null(arms$current_arm)) {
        div(class = "arm-indicator not-recording", 
            icon("circle", class = "text-muted"), "Not Recording")
      } else {
        arm_class <- tolower(gsub(" ", "-", arms$current_arm))
        odour_source <- get_odour_source(arms$current_arm)
        
        div(
          class = paste("arm-indicator", arm_class),
          icon("circle", class = "recording-icon"),
          span(arms$current_arm, class = "arm-name"),
          if (arms$current_arm != "Center") {
            span(class = "odour-label", odour_source)
          } else {
            span(class = "odour-label", "Center")
          }
        )
      }
    })
    
    # Arm label outputs for visual
    output$arm1_label <- renderText({ arms$arm1_odour })
    output$arm2_label <- renderText({ arms$arm2_odour })
    output$arm3_label <- renderText({ arms$arm3_odour })
    output$arm4_label <- renderText({ arms$arm4_odour })
    
    # Create summary of times
    summarized_times <- reactive({
      combined_data <- bind_rows(arms$all_times, arms$times)
      
      if(nrow(combined_data) == 0) {
        return(data.frame(
          Trial = character(0),
          Arm = character(0),
          Odour_Source = character(0),
          Total_Duration = numeric(0),
          Total_Entries = integer(0),
          Avg_Duration = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      combined_data %>%
        group_by(Trial, Arm, Odour_Source) %>%
        summarize(
          Total_Duration = sum(Duration, na.rm = TRUE),
          Total_Entries = n(),
          Avg_Duration = mean(Duration, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(Trial, desc(Total_Duration))
    })
    
    # Format the summary table
    output$arm_times <- renderTable({
      summarized_times() %>%
        mutate(
          Total_Duration_Fmt = sprintf("%.1f", Total_Duration),
          Avg_Duration_Fmt = sprintf("%.1f", Avg_Duration)
        ) %>%
        select(
          Trial, 
          Arm, 
          "Odour Source" = Odour_Source,
          "Total Duration (s)" = Total_Duration_Fmt,
          "Avg Duration (s)" = Avg_Duration_Fmt,
          "Total Entries" = Total_Entries
        )
    })
    
    # Raw data table
    output$raw_data <- renderTable({
      combined_data <- bind_rows(arms$all_times, arms$times)
      
      if(nrow(combined_data) == 0) {
        return(data.frame(
          Trial = character(0),
          Arm = character(0),
          "Odour Source" = character(0),
          "Duration (s)" = numeric(0),
          "Entry Time" = character(0),
          "Exit Time" = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      combined_data %>%
        select(
          Trial,
          Arm,
          "Odour Source" = Odour_Source,
          "Duration (s)" = Duration,
          "Entry Time" = Entry_Time,
          "Exit Time" = Exit_Time
        )
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
        
        # Display remaining time if auto-stop is enabled
        if (!is.null(parent_input$auto_stop_four) && parent_input$auto_stop_four) {
          remaining <- max(0, arms$max_time - elapsed)
          output$trial_timer <- renderText({
            paste0(format_time(elapsed), " / ", format_time(arms$max_time), 
                   " (", format_time(remaining), " remaining)")
          })
        } else {
          output$trial_timer <- renderText({
            format_time(elapsed)
          })
        }
        
        # Update progress bar
        progress_pct <- min(100, elapsed / arms$max_time * 100)
        shinyjs::runjs(sprintf(
          "$('#%s').find('.progress-bar').css('width', '%f%%').attr('aria-valuenow', %f).text('%d%%');",
          ns("time_progress"), progress_pct, progress_pct, round(progress_pct)
        ))
      } else {
        if (!is.null(parent_input$auto_stop_four) && parent_input$auto_stop_four) {
          output$trial_timer <- renderText({
            paste0(format_time(arms$total_elapsed_time), " / ", 
                   format_time(arms$max_time))
          })
        } else {
          output$trial_timer <- renderText({
            format_time(arms$total_elapsed_time)
          })
        }
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
    
    # Download handlers
    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste("four_arm_summary_", parent_input$trial_number_four, "_", 
              format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
      },
      content = function(file) {
        # Get the summary data
        write.csv(summarized_times(), file, row.names = FALSE)
      }
    )
    
    output$downloadRaw <- downloadHandler(
      filename = function() {
        paste("four_arm_raw_data_", parent_input$trial_number_four, "_", 
              format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
      },
      content = function(file) {
        # Get the raw data
        combined_data <- bind_rows(arms$all_times, arms$times)
        write.csv(combined_data, file, row.names = FALSE)
      }
    )
    
    # Main download handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("four_arm_olfactometer_data_", parent_input$trial_number_four, "_", 
              format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
      },
      content = function(file) {
        # Include both summary and raw data
        combined_data <- bind_rows(arms$all_times, arms$times)
        summary_data <- summarized_times()
        
        # Try to create an Excel file with multiple sheets
        tryCatch({
          # If openxlsx package is available
          if (requireNamespace("openxlsx", quietly = TRUE)) {
            # Create a workbook with multiple sheets
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb, "Summary")
            openxlsx::addWorksheet(wb, "Raw Data")
            openxlsx::addWorksheet(wb, "Movement History")
            
            # Write data to sheets
            openxlsx::writeData(wb, sheet = "Summary", x = summary_data)
            openxlsx::writeData(wb, sheet = "Raw Data", x = combined_data)
            openxlsx::writeData(wb, sheet = "Movement History", x = arms$movement_history)
            
            # Save workbook
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          } else {
            # Fallback to CSV
            write.csv(combined_data, file, row.names = FALSE)
          }
        }, error = function(e) {
          # Fallback to CSV if Excel export fails
          write.csv(combined_data, file, row.names = FALSE)
        })
      }
    )
  })
}