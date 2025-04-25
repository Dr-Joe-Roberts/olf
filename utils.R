# utils.R - Common utility functions
format_duration <- function(seconds) {
  hours <- floor(seconds / 3600)
  mins <- floor((seconds %% 3600) / 60)
  secs <- round(seconds %% 60)
  
  if (hours > 0) {
    sprintf("%02d:%02d:%02d", hours, mins, secs)
  } else {
    sprintf("%02d:%02d", mins, secs)
  }
}

reset_trial_data <- function(reactiveObj, trial_number_input = NULL) {
  reactiveObj$current_arm <- "Center"
  reactiveObj$start_time <- NULL
  reactiveObj$times <- data.frame(
    Arm = character(0),
    Duration = numeric(0),
    Count = integer(0),
    Odour_Source = character(0),
    Trial = character(0),
    stringsAsFactors = FALSE
  )
  reactiveObj$trial_start <- NULL
  reactiveObj$elapsed_time <- 0
  reactiveObj$total_elapsed_time <- 0
  
  if (!is.null(trial_number_input)) {
    updateTextInput(session, trial_number_input, 
                    value = as.character(as.numeric(input[[trial_number_input]]) + 1))
  }
}

validate_counts <- function(counts, expected_total, show_error = TRUE) {
  total <- sum(counts, na.rm = TRUE)
  
  if (total != expected_total) {
    if (show_error) {
      showModal(modalDialog(
        title = "Error",
        paste("The total count (", total, ") does not match the expected total (", 
              expected_total, ").", sep = ""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    return(FALSE)
  }
  return(TRUE)
}