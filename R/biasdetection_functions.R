#' @title Read .csv file
#'
#' @description This function reads the .csv file which is originated from Lime Survey.
#' It also calculates the number of time groups automatically for detecting random responses based on the time spent in filling out parts of the survey.
#' @param csv_file The file we want to process
#' @return A list containing the data from the .csv file
#' read_csv()

read_csv <- function(csv_file) {
  survey_responses <- read.csv(csv_file)

  # Get number of time groups
  names <- colnames(survey_responses)
  num_time_groups <<- sum(str_count(names, pattern = "Time_Group"), na.rm = FALSE)

  return(survey_responses)
}


#' @title Remove duplicate
#'
#' @description Removes the duplicate entries from the panel data based on the id of the respondent to detect random responses.
#' @param csv_data A list containing the responses
#' @return A list containing unique responses (based on id) from the original panel data
#' remove_duplicates()

remove_duplicates <- function(csv_data) {
  rr_data <- csv_data %>% distinct(id, .keep_all = TRUE)
  return(rr_data)
}


#' @title Random response time filter
#'
#' @description Calculates the first quartiles of each Time Group to identify the random responses. It displays them as well.
#' @param rr_data A list containing unique responses (based on id) from the original panel data
#'
#' rr_time_filter()

rr_time_filter <- function(rr_data) {
  for(i in 1:num_time_groups) {
    time_group <- paste("Time_Group",toString(i), sep="")
    print(summary(rr_data[[time_group]]))
  }

  print(summary(rr_data$interviewtime))
}

#' @title Random responding time filter
#'
#' @description For each Time Group and for each respondent, the results of the random responses are recorded as binary.
#' It also creates 2 additional columns (RR_Total and percent_RR_Total) containing the sum of the random responses for each
#' Time Group, and the total percentage of random responses for each respondent, respectively.
#' @param rr_data A list containing unique responses (based on id) from the original panel data
#' @param excluded_time_groups A list containing the indices of excluded time groups
#' @return A list containing the updated data
#' random_responding_time_filter()

random_responding_time_filter <- function(rr_data, excluded_time_groups) {
  rr_data$RR_Total <- NA # Creates new column with the sum of the random responses for each Time Group
  rr_data$percent_RR_Total <- NA # Created a new column with the total percentage of random responses for each respondent

  for(i in 1:num_time_groups) {
    if(!(i %in% excluded_time_groups)){
      time_group <- paste("Time_Group",toString(i), sep="")

      rr_column <- paste("RR_Gr",toString(i), sep="")
      rr_data[[rr_column]] <- NA # RR_Gr[i] <- Na

      first_quartile <- summary(rr_data[[time_group]])[2]
      first_quartile <- as.double(first_quartile)

      rr_data[[rr_column]] <- ifelse(rr_data[[time_group]]<first_quartile,1,0) # RR_Gr[i] <- ifelse(RR_Data$Time_Group[i]<XX.XX,1,0)

      # Total Random responding
      if (is.na(rr_data$RR_Total)) { # For initialization
        rr_data$RR_Total <- rr_data[[rr_column]]
      }
      else {
        rr_data$RR_Total <- rr_data$RR_Total + rr_data[[rr_column]]
      }
    }
  }
  # Percentage of Random Responding
  num_total_groups <- num_time_groups - length(excluded_time_groups)
  rr_data$percent_RR_Total <- rr_data$RR_Total/num_total_groups * 100

  write.csv(rr_data, "Random Responding Time filter.csv")

  return(rr_data)
}


#' @title Write the updated .csv file
#'
#' @description Given a .csv file with panel data, excludes duplicate entries, applies time filter for random responses
#' and writes the result in another .csv file.
#' @param csv_file The file we want to process
#' @param excluded_time_groups A list containing the indices of excluded time groups
#' write_rr()

write_rr <- function(csv_file, excluded_time_groups) {
  excluded_time_groups <<- excluded_time_groups
  data <- read_csv(csv_file)
  rr_data <- remove_duplicates(data)
  rr_time_filter(rr_data)
  rr_data <- random_responding_time_filter(rr_data, excluded_time_groups)
}

# csv_file = "Survey Responses Modified.csv"
# excluded_time_groups <<- list(1) # excluding time_group1, as it included only introduction and description of the survey which people might overlook

