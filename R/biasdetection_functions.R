#' @title Read .csv file
#'
#' @description This function reads the .csv file which is originated from Lime Survey.
#' It also calculates the number of time groups automatically for detecting random responses based on the time spent in filling out parts of the survey.
#' @param csv_file The file we want to process
#' @return A list containing the data from the .csv file
#' read_csv()
#' @export

read_csv <- function(csv_file) {
  working_dataset <<- read.csv(csv_file)

  # Get number of time groups
  names <- colnames(working_dataset)
  num_time_groups <<- sum(str_count(names, pattern = "Time_Group"), na.rm = FALSE)

  return(working_dataset)
}


#' @title Remove duplicate
#'
#' @description Removes the duplicate entries from the panel data based on the id of the respondent to detect random responses.
#' @param csv_data A list containing the responses
#' @return A list containing unique responses (based on id) from the original panel data
#' remove_duplicates()
#' @export

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
#' @export

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
#' @export

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
#' @return A list containing the updated data
#' rr_function()
#' @export

rr_function <- function(csv_file, excluded_time_groups) {
  excluded_time_groups <<- excluded_time_groups
  data <- read_csv(csv_file)
  rr_data <- remove_duplicates(data)
  rr_time_filter(rr_data)
  rr_data <- random_responding_time_filter(rr_data, excluded_time_groups)
  return(rr_data)
}

#' @title Create Plots Random Responding
#'
#' @description This function creates plots from the resulting data in random responding
#' (bar plot for group wise and bar plot, density plot and histogram for respondent wise)
#' @param rr_data A list containing unique responses (based on id) from the original panel data
#' @param excluded_time_groups A list containing the indices of excluded time groups
#' create_plots_rr()
#' @export

create_plots_rr <- function(rr_data, excluded_time_groups) {
  rr_group_wise_percentage <<- c()
  rr_groups_used_names <<- c()

  j <- 1 # to keep track of number of time groups used
  for(i in 1:num_time_groups) {
    if(!(i %in% excluded_time_groups)){
      rr_column <- paste("RR_Gr",toString(i), sep="")

      total_rr_column <- paste("Total_RR_Gr",toString(i), sep="")
      assign(total_rr_column, sum(rr_data[[rr_column]])) # Total_RR_Gr[i] <- sum(RR_Data$RR_Gr[i])

      percent_rr_column <- paste("percent_RR_Gr",toString(i), sep="")
      assign(percent_rr_column, get(total_rr_column)/length(rr_data[[rr_column]])* 100) # percent_RR_Gr[i] <- Total_RR_Gr[i] / len(RR_Data$RR_Gr[i])

      rr_group_wise_percentage[j] <- get(percent_rr_column)
      rr_groups_used_names[j] <- paste("Gr ",toString(i), sep="")

      j <- j + 1
    }
  }
  # Bar Plot
  plot_name <- "Random Responding Time filter Group wise"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  barplot(rr_group_wise_percentage, main=plot_name, xlab="Groups", ylab="% of Random respondents time filter", names.arg=rr_groups_used_names,
           border="red", density=(rr_group_wise_percentage))
  dev.off()

  #Respondent wise
  #Density plots
  plot_name <- "Kernel density of percentage of Random Responding of all respondents"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  d <- density(rr_data$percent_RR_Total)
  plot(d, main = plot_name)
  polygon(d, col="blue", border = "red")

  ##Histogram plots
  plot_name <- "Histogram of percentage of random responding"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  hist(rr_data$percent_RR_Total, breaks=12, col='red', main = plot_name,
       xlab="percentage of random responding", ylab = "frequency")
  dev.off()

  ##Bar plots
  plot_name <- "percentage of Random responding distribution"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  counts <- table(rr_data$percent_RR_Total)
  ylim <- c(0, 1.2*max(counts))
  xx <-barplot(counts, ylim = ylim, main=plot_name,
               xlab="percentage of random responding", ylab = "No of respondents")
  text(x = xx, y = counts, label = counts, pos = 3, cex = 0.8, col = "red")
  dev.off()
}

# csv_file <- "C:/Users/HP/Desktop/Study Project/data analysis/Package Environment/Survey Responses Modified.csv"
# excluded_time_groups <<- list(1) # excluding time_group1, as it included only introduction and description of the survey which people might overlook
# rr_data <- rr_function(csv_file, excluded_time_groups)
# create_plots_rr <- function(rr_data, excluded_time_groups)

#############################################################################################################################################
#' @title Extreme Responding Style
#'
#' @description
#' @param csv_file The file we want to process
#' @param likert_columns A list containing the column names to use for ERS
#' @param max_value Maximum rate in the likert scale
#' @param min_value Minimum rate in the likert scale
#' @return A list containing the updated data
#' ers_function()
#' @export

# Extreme Response Styles
ers_function <- function(csv_file, likert_columns, max_value, min_value) {
  working_dataset <- read_csv(csv_file)

  #Removing duplicates based on 'id'
  ers_data <- working_dataset %>% distinct(id, .keep_all = TRUE)

  ers_data$ERS_Total <- NA
  ers_data$percent_ERS_Total <- NA
  ers_data$ERS_P_Total <- NA
  ers_data$percent_P_ERS_Total <- NA
  ers_data$ERS_N_Total <- NA
  ers_data$percent_N_ERS_Total <- NA

  num_likert_columns <- length(likert_columns)

  for(i in 1:num_likert_columns) {
    likert_column <- unlist(likert_columns)[i]

    ers_column <- paste("ERS_",likert_column, sep="")
    ers_positive_column <- paste("ERS_P_",likert_column, sep="")
    ers_negative_column <- paste("ERS_N_",likert_column, sep="")

    ers_data[[ers_column]] <- ifelse(ers_data[[likert_column]] == min_value | ers_data[[likert_column]]== max_value, 1, 0)
    ers_data[[ers_positive_column]] <- ifelse(ers_data[[likert_column]] == max_value, 1, 0)
    ers_data[[ers_negative_column]] <- ifelse(ers_data[[likert_column]] == min_value, 1, 0)

    if (is.na(ers_data$ERS_Total)) { # For initialization
      ers_data$ERS_Total <- ers_data[[ers_column]]
    }
    else {
      ers_data$ERS_Total <- ers_data$ERS_Total + ers_data[[ers_column]]
    }

    if (is.na(ers_data$ERS_P_Total)) { # For initialization
      ers_data$ERS_P_Total <- ers_data[[ers_positive_column]]
    }
    else {
      ers_data$ERS_P_Total <- ers_data$ERS_P_Total + ers_data[[ers_positive_column]]
    }

    if (is.na(ers_data$ERS_N_Total)) { # For initialization
      ers_data$ERS_N_Total <- ers_data[[ers_negative_column]]
    }
    else {
      ers_data$ERS_N_Total <- ers_data$ERS_N_Total + ers_data[[ers_negative_column]]
    }
  }
  ers_data$percent_ERS_Total <- (ers_data$ERS_Total/num_likert_columns)*100
  ers_data$percent_P_ERS_Total <- (ers_data$ERS_P_Total/num_likert_columns)*100
  ers_data$percent_N_ERS_Total <- (ers_data$ERS_N_Total/num_likert_columns)*100

  return(ers_data)
}

#' @title Create Plots Extreme Response Style
#'
#' @description This function creates plots from the resulting data in extreme response style
#' (question wise bar plot and respondent wise bar plot (for positive, negative and total),
#' kernel density plot for respondent wise).
#' @param ers_data Resulting data from the ers_function
#' create_plots_ers()
#' @export

create_plots_ers <- function(ers_data) {
  ers_group_wise_percentage <<- c()
  ers_positive_group_wise_percentage <<- c()
  ers_negative_group_wise_percentage <<- c()
  ers_groups_used_names <<- c()

  for(i in 1:num_likert_columns) {
    likert_column <- unlist(likert_columns)[i]
    ers_column <- paste("ERS_",likert_columns[i], sep="")
    ers_positive_column <- paste("ERS_P_",likert_column, sep="")
    ers_negative_column <- paste("ERS_N_",likert_column, sep="")

    total_ers_column <- paste("Total_ERS_",likert_column, sep="")
    assign(total_ers_column, sum(ers_data[[ers_column]])) # Total_ERS_PC <- sum(ERS_Data$ERS_PC)
    total_ers_column_positive <- paste("Total_ERS_P_",likert_column, sep="")
    assign(total_ers_column_positive, sum(ers_data[[ers_positive_column]])) # Total_ERS_P_PC <- sum(ERS_Data$ERS_P_PC)
    total_ers_column_negative <- paste("Total_ERS_N_",likert_column, sep="")
    assign(total_ers_column_negative, sum(ers_data[[ers_negative_column]])) # Total_ERS_N_PC <- sum(ERS_Data$ERS_N_PC)


    percent_ers_column <- paste("percent_ERS_",likert_column, sep="")
    assign(percent_ers_column, get(total_ers_column)/length(ers_data[[ers_column]])* 100) # percent_ERS_PC <- Total_ERS_PC / len(ERS_Data$ERS_PC)
    percent_ers_column_positive <- paste("percent_ERS_P_",likert_column, sep="")
    assign(percent_ers_column_positive, get(total_ers_column_positive)/length(ers_data[[ers_positive_column]])* 100) # percent_ERS_P_PC <- Total_ERS_P_PC / len(ERS_Data$ERS_P_PC)
    percent_ers_column_negative <- paste("percent_ERS_N_",likert_column, sep="")
    assign(percent_ers_column_negative, get(total_ers_column_negative)/length(ers_data[[ers_negative_column]])* 100) # percent_ERS_N_PC <- Total_ERS_N_PC / len(ERS_Data$ERS_N_PC)


    ers_group_wise_percentage[i] <- get(percent_ers_column)
    ers_positive_group_wise_percentage[i] <- get(percent_ers_column_positive)
    ers_negative_group_wise_percentage[i] <- get(percent_ers_column_negative)
    ers_groups_used_names[i] <- likert_column
  }

  #Plots
  #Question wise bar plots
  #Total ERS
  plot_name <- "Extreme Responding Question wise"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  ylim <- c(0, 1.2*max(ers_group_wise_percentage))
  pp <-barplot(ers_group_wise_percentage, ylim = ylim, main=plot_name, xlab="Question wise ERS", ylab="% of extreme response styles respondents",
               names.arg=ers_groups_used_names,border="red", density=ers_group_wise_percentage)
  text(x = pp, y = ers_group_wise_percentage, label = ers_group_wise_percentage, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #Positive ERS
  plot_name <- "Positive Extreme Responding Question wise"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  P_ERS_B <- ers_positive_group_wise_percentage
  ylim <- c(0, 1.2*max(P_ERS_B))
  xx <-barplot(P_ERS_B, ylim = ylim, main=plot_name, xlab="Question wise positive ERS", ylab="% of positive extreme response styles respondents",
               names.arg=ers_groups_used_names, border="red", density=ers_positive_group_wise_percentage)
  text(x = xx, y = P_ERS_B, label = P_ERS_B, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #Negative ERS
  plot_name <- "Negative Extreme Responding Question wise"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  N_ERS_B <- ers_negative_group_wise_percentage
  ylim <- c(0, 1.2*max(N_ERS_B))
  xx <-barplot(N_ERS_B, ylim = ylim, main=plot_name , xlab="Question wise negative ERS", ylab="% of Negative extreme response styles respondents",
               names.arg=ers_groups_used_names, border="red", density=ers_negative_group_wise_percentage)
  text(x = xx, y = N_ERS_B, label = N_ERS_B, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #Respondent wise bar plots
  #Total ERS
  plot_name <- "percentage of Total Extreme Response Style distribution"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  counts <- table(ers_data$percent_ERS_Total)
  ylim <- c(0, 1.2*max(counts))
  qq <-barplot(counts, ylim = ylim, main=plot_name,
               xlab="percentage of Total ERS", ylab = "No of respondents")
  text(x = qq, y = counts, label = counts, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #Positive ERS
  plot_name <- "percentage of positive ERS distribution"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  counts <- table(ers_data$percent_P_ERS_Total)
  ylim <- c(0, 1.2*max(counts))
  xx <-barplot(counts, ylim = ylim, main=plot_name,
               xlab="percentage of positive ERS", ylab = "No of respondents")
  text(x = xx, y = counts, label = counts, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #Negative ERS
  plot_name <- "percentage of negative ERS distribution"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  counts <- table(ers_data$percent_N_ERS_Total)
  ylim <- c(0, 1.2*max(counts))
  xx <-barplot(counts, ylim = ylim, main=plot_name,
               xlab="percentage of negative ERS", ylab = "No of respondents")
  text(x = xx, y = counts, label = counts, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #Respondent wise
  #Kernel Density plots
  plot_name <- "Kernel density of percentage of Extreme Response Style Responding of all respondents"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  d <- density(ers_data$percent_ERS_Total)
  plot(d, main = plot_name)
  polygon(d, col="blue", border = "red")
  dev.off()

  write.csv(ers_data,"Extreme Response styles including both + & -.csv")
}

# likert_columns <- list("ParkingCosts.SQ001.", "CongestionCosts.SQ001.", "LowIncLikert.SQ001.", "AffordableTrLikert.SQ001.")
# max_value <- 5
# min_value <- 1
# ers_data <- ers_function(csv_file, likert_columns, max_value, min_value)
# create_plots_ers(ers_data)

#############################################################################################################################################
#' @title Mid point Response Styles
#'
#' @description
#' @param csv_file The file we want to process
#' @param likert_columns A list containing the column names to use for ERS
#' @param mid_value Mid rate in the likert scale
#' @return A list containing the updated data
#' mrs_function()
#' @export

# Mid point Response Styles
mrs_function <- function(csv_file, likert_columns, mid_value) {
  working_dataset <- read_csv(csv_file)

  #Removing duplicates based on 'id'
  mrs_data <- working_dataset %>% distinct(id, .keep_all = TRUE)

  #Adding blank columns to get the outputs of MRS
  mrs_data$MRS_Total <-NA
  mrs_data$percent_MRS_Total <-NA

  for(i in 1:num_likert_columns) {
    likert_column <- unlist(likert_columns)[i]

    mrs_column <- paste("MRS_",likert_column, sep="")

    #Respondent wise MRS
    mrs_data[[mrs_column]] <- ifelse(mrs_data[[likert_column]] == mid_value,1,0)

    if (is.na(mrs_data$MRS_Total)) { # For initialization
      mrs_data$MRS_Total <- mrs_data[[mrs_column]]
    }
    else {
      mrs_data$MRS_Total <- mrs_data$MRS_Total + mrs_data[[mrs_column]]
    }
  }
  mrs_data$percent_MRS_Total <- (mrs_data$MRS_Total/num_likert_columns)*100

  return(mrs_data)

}

#' @title Create Plots Mid point Response Style
#'
#' @description This function creates plots from the resulting data in mid point response style
#' (question and respondent wise bar plots).
#' @param mrs_data Resulting data from the mrs_function
#' create_plot_mrs()
#' @export

create_plots_mrs <- function(mrs_data) {
  mrs_group_wise_percentage <<- c()
  mrs_groups_used_names <<- c()

  for(i in 1:num_likert_columns) {
    likert_column <- unlist(likert_columns)[i]
    mrs_column <- paste("MRS_",likert_column, sep="")

    total_mrs_column <- paste("Total_MRS_",likert_column, sep="")
    assign(total_mrs_column, sum(mrs_data[[mrs_column]])) # Total_MRS_PC <- sum(MRS_Data$MRS_PC)


    percent_mrs_column <- paste("percent_MRS_",likert_column, sep="")
    assign(percent_mrs_column, get(total_mrs_column)/length(mrs_data[[mrs_column]])* 100) # percent_MRS_PC <- Total_MRS_PC / len(MRS_Data$MRS_PC)

    mrs_group_wise_percentage[i] <- get(percent_mrs_column)
    mrs_groups_used_names[i] <- likert_column
  }

  #Plots
  # Question wise
  plot_name <- "Mid point Responding Question wise"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  MRS <- mrs_group_wise_percentage
  ylim <- c(0, 1.2*max(MRS))
  xx <-barplot(MRS, ylim = ylim, main= plot_name, xlab="Question wise MRS", ylab="% of Mid point response styles respondents",
               names.arg=mrs_groups_used_names, border="red", density=mrs_group_wise_percentage)
  text(x = xx, y = MRS, label = MRS, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #Respondent wise
  plot_name <- "Percentage of MRS distribution"
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  counts <- table(mrs_data$percent_MRS_Total)
  ylim <- c(0, 1.2*max(counts))
  xx <-barplot(counts, ylim = ylim, main=plot_name,
               xlab="percentage of MRS", ylab = "No of respondents")
  text(x = xx, y = counts, label = counts, pos = 3, cex = 0.8, col = "red")
  dev.off()

  write.csv(mrs_data,"Midpoint Response Styles.csv")
}

# likert_columns <- list("ParkingCosts.SQ001.", "CongestionCosts.SQ001.", "LowIncLikert.SQ001.", "AffordableTrLikert.SQ001.")
# mid_value <- 3
# mrs_data <- mrs_function(csv_file, likert_columns, mid_value)
# create_plots_mrs(mrs_data)

#############################################################################################################################################
#' @title Lexicographic Response Style Function
#'
#' @description
#' @param csv_file The file we want to process
#' @param total_scenarios A list containing all the SP scenarios
#' @param scenarios A list containing the relevant SP scenarios for the specific attribute of LRS
#' @param alternatives A list containing the alternatives for the SP experiment
#' @param attribute The attribute chosen for LRS
#' @param attribute_short The short form of the attribute name (for table names)
#' @return A list containing the updated data
#' lrs_general_function()
#' @export

lrs_general_function <- function(csv_file, total_scenarios, scenarios, alternatives, attribute, attribute_short){
  working_dataset <- read_csv(csv_file)

  lrs_group_wise_percentage <- c()
  lrs_groups_used_names <- c()
  num_scenarios <- length(scenarios)
  columns_to_aggregate <- list()

  for(i in 1:num_scenarios) {
    scenario_column <- unlist(scenarios)[i]
    lrs_column <- paste("LRS_", attribute_short, "_", scenario_column, sep="") # e.g. LRS_CC_S17

    lrs_groups_used_names[i] <- scenario_column
    working_dataset[[lrs_column]] <- ifelse(working_dataset$Scenario == scenarios[i] , ifelse(working_dataset$Choice == alternatives[i] , 1, 0), 0)

    columns_to_aggregate <- list.append(columns_to_aggregate, working_dataset[[lrs_column]])
  }

  #Removing duplicates based on 'id'
  lrs_data <- working_dataset[order(as.integer(working_dataset$id),decreasing = FALSE), ]

  current_ncol <- ncol(working_dataset) # Determines the current number of columns
  id_col_num <- which(colnames(working_dataset)=="id")

  lrs_data <- lrs_data %>% select(id_col_num, (current_ncol - (num_scenarios - 1)):current_ncol)

  agg = aggregate(columns_to_aggregate, by = list(lrs_data$id), FUN = sum)

  colnames(agg)[1] <- "id" # First is id

  total_lrs_attribute <- paste("Total_LRS_", attribute_short, sep="")
  agg[[total_lrs_attribute]] <- NA


  for(i in 1:num_scenarios) {
    colnames(agg)[i+1] <- scenarios[i]
    scenario_column <- unlist(scenarios)[i]

    total_lrs_column <- paste("Total_LRS_", attribute_short, "_", scenario_column, sep="")
    assign(total_lrs_column, (sum(agg[[scenario_column]])/length(agg[[scenario_column]]))*100) # Total_ERS_P_PC <- sum(ERS_Data$ERS_P_PC)

    lrs_group_wise_percentage[i] <- signif(get(total_lrs_column), digits=4)

    if (is.na(agg[[total_lrs_attribute]])) { # For initialization
      agg[[total_lrs_attribute]] <- agg[[scenario_column]]
    }
    else {
      agg[[total_lrs_attribute]] <- agg[[total_lrs_attribute]] + agg[[scenario_column]]
    }
  }

  percent_lrs_attribute <- paste("Percent_LRS_", attribute_short, sep="")
  agg[[percent_lrs_attribute]] <- (agg[[total_lrs_attribute]]/length(total_scenarios))* 100

  file_name <-paste("Lexicographic ", attribute, ".csv", sep="")
  write.csv(agg, file_name)

  ##Plots
  #scenario wise
  plot_name <- paste("Lexicopgraphic ", attribute, " based scenario wise", sep="")
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  LRS <- lrs_group_wise_percentage
  ylim <- c(0, 1.2*max(LRS))
  xx <-barplot(LRS, ylim = ylim, main= plot_name, xlab= paste("Scenario wise Lexicographic ", attribute, sep=""),
                                                                 ylab=paste("Percent of LRS ", attribute, " respondents", sep=""),
                                                                 names.arg=lrs_groups_used_names,border="red",
                                                                 density=lrs_group_wise_percentage)
  text(x = xx, y = LRS, label = LRS, pos = 3, cex = 0.8, col = "red")
  dev.off()

  #respondent wise
  plot_name <- paste("Percentage of LRS ", attribute, " distribution", sep="")
  png(filename=paste(plot_name, ".png", sep=""), width=900, bg="white")
  counts <- table(agg[[percent_lrs_attribute]])
  ylim <- c(0, 1.5*max(counts))
  xx <-barplot(counts, ylim = ylim, main=plot_name, xlab=paste("Percentage of LRS ", attribute, sep=""), ylab = "No of respondents")
  text(x = xx, y = counts, label = counts, pos = 3, cex = 0.8, col = "red")
  dev.off()

  return(lrs_data)
}

# total_scenarios <- list("S17","S18","S19","S20","S21","S22","S23","S24")
#
# attribute_cc <- "Cheapest Cost"
# attribute_short_cc <- "CC"
# scenarios_cc <- list("S17","S19","S20","S21","S22")
# alternatives_cc <- list("Alt2", "Alt1", "Alt2", "Alt1", "Alt2")
#
# attribute_ftt <- "Fastest Travel Time"
# attribute_short_ftt <- "FTT"
# scenarios_ftt <- list("S19","S20","S21","S22","S23")
# alternatives_ftt <- list("Alt2", "Alt1", "Alt2", "Alt2", "Alt1")
#
# attribute_hcr <- "Highest Congestion Reduction"
# attribute_short_hcr <- "HCR"
# scenarios_hcr <- list("S17","S18","S21","S22","S23","S24")
# alternatives_hcr <- list("Alt2", "Alt2", "Alt1", "Alt1", "Alt1", "Alt2")
#
# attribute_ppb <- "Percentage of People Benefiting"
# attribute_short_ppb <- "PPB"
# scenarios_ppb <- list("S17","S19","S20","S21","S23","S24")
# alternatives_ppb <- list("Alt1", "Alt1", "Alt2", "Alt1", "Alt2", "Alt2")
#
#
# lrs_data <- lrs_general_function(working_dataset, total_scenarios, scenarios_cc, alternatives_cc, attribute_cc, attribute_short_cc)
# lrs_data <- lrs_general_function(working_dataset, total_scenarios, scenarios_ftt, alternatives_ftt, attribute_ftt, attribute_short_ftt)
# lrs_data <- lrs_general_function(working_dataset, total_scenarios, scenarios_hcr, alternatives_hcr, attribute_hcr, attribute_short_hcr)
# lrs_data <- lrs_general_function(working_dataset, total_scenarios, scenarios_ppb, alternatives_ppb, attribute_ppb, attribute_short_ppb)

#############################################################################################################################################
#' @title Attribute Non Attendance Function (for Inconsistent Bias and Non Trading)
#'
#' @description
#' @param csv_file The file we want to process
#' @param total_scenarios A list containing all the SP scenarios
#' @param scenarios A list containing the relevant SP scenarios for the specific attribute of LRS
#' @param alternatives A list containing the alternatives for the SP experiment
#' @param attribute The attribute chosen for LRS
#' @param attribute_short The short form of the attribute name (for table names)
#' @return A list containing the updated data
#' attribute_non_attendance_function()
#' @export

attribute_non_attendance_function <- function(csv_file, total_scenarios, scenarios, alternatives, attribute, attribute_short){
  working_dataset <- read_csv(csv_file)
  ana_data <- remove_duplicates(working_dataset)
  num_total_scenarios <- length(total_scenarios)

  for(l in 1:nrow(ana_data)) {
    scenarios_variables <- list()
    data_idx <- which(colnames(working_dataset)=="id")
    ids_list <- which(working_dataset[data_idx] == as.character(ana_data[l,"id"]))

    for(j in 1:length(ids_list)){
      j <- ids_list[j]

      for(i in 1:num_total_scenarios) {
        scenario_column <- unlist(total_scenarios)[i]
        attribute_column <- paste(attribute_short, "_",scenario_column, sep="")

        if (working_dataset[j,"Scenario"] == scenario_column){
          assign(attribute_column, as.character(working_dataset[j,"Choice"]))

          if(scenario_column %in% scenarios){
            scenarios_variables <- list.append(scenarios_variables, get(attribute_column))
          }
        }
      }
    }

    ana_data[l,attribute_short] = 0 # If every Inconsistency test fails, it will be 0

    num_alternative_choices <- length(alternatives)
    for(i in 1:num_alternative_choices) {

      is_equal <- identical(unlist(scenarios_variables), unlist(alternatives[i]))

      if(is_equal == TRUE){
        ana_data[l,attribute_short] = 1
      }
    }
  }

  file_name <-paste(attribute, ".csv", sep="")
  write.csv(ana_data, file_name)
  return(ana_data)
}


# total_scenarios <- list("S17","S18","S19","S20","S21","S22","S23","S24")
#
# attribute_ib <- "Inconsistent Bias"
# attribute_short_ib <- "IB"
# scenarios_ib <- list("S19","S20")
# alternatives_ib <- list(list("Alt2", "Alt2"),list("Alt1", "Alt1"))
#
# attribute_nt <- "Non Trading"
# attribute_short_nt <- "NT"
# scenarios_nt <- list("S17","S18","S19","S20","S21","S22","S23","S24")
# alternatives_nt <- list(list("Alt1", "Alt1","Alt1", "Alt1","Alt1", "Alt1","Alt1", "Alt1"),
#                                list("Alt2", "Alt2","Alt2", "Alt2","Alt2", "Alt2","Alt2", "Alt2"),
#                                list("Alt3", "Alt3","Alt3", "Alt3","Alt3", "Alt3","Alt3", "Alt3"))
#
# attribute_non_attendance_function(csv_file, total_scenarios, scenarios_ib, alternatives_ib, attribute_ib, attribute_short_ib)
# attribute_non_attendance_function(csv_file, total_scenarios, scenarios_nt, alternatives_nt, attribute_nt, attribute_short_nt)

