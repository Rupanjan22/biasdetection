library(dplyr)
library(stringr)
library(rlist)

csv_file <- "./csv/SurveyResponses.csv"
excluded_time_groups <- list(1) # excluding time_group1, as it included only introduction and description of the survey which people might overlook
num_time_groups <- get_time_groups(csv_file)

rr_data <- rr_function(csv_file, excluded_time_groups, num_time_groups)
create_plots_rr(rr_data, excluded_time_groups, num_time_groups)

likert_columns <- list("ParkingCosts.SQ001.", "CongestionCosts.SQ001.", "LowIncLikert.SQ001.", "AffordableTrLikert.SQ001.")
max_value <- 5
min_value <- 1
ers_data <- ers_function(csv_file, likert_columns, max_value, min_value)
create_plots_ers(ers_data, likert_columns)

likert_columns <- list("ParkingCosts.SQ001.", "CongestionCosts.SQ001.", "LowIncLikert.SQ001.", "AffordableTrLikert.SQ001.")
mid_value <- 3
mrs_data <- mrs_function(csv_file, likert_columns, mid_value)
create_plots_mrs(mrs_data, likert_columns)

total_scenarios <- list()

for(i in 1:8)
{
  total_scenarios <- list.append(total_scenarios,list())
  for(j in 1:8)
  {
    n <- j+(i-1)*8

    total_scenarios[[i]][[j]] <- paste("S", n, sep="")
  }
}

#total_scenarios <- list(list(),list(),list("S17","S18","S19","S20","S21","S22","S23","S24"),...)
#
# total_scenarios_overall <- list(list("S1",..."S8"),list("S9",.."S16"),list(),list(),list(),list(),list(),list())


attribute_cc <- "Cheapest_Cost"
attribute_short_cc <- "CC"
scenarios_cc <- list(list("S1","S3","S4","S5", "S8"), list("S9","S11","S12","S13","S14"),list("S17","S18","S19","S24"),list("S26","S27","S30","S32"),
                     list("S33","S34","S35","S36","S37", "S38", "S40"), list("S41","S43","S44","S45","S47", "S48"), list("S49","S50","S52","S53","S54"),
                     list("S57","S59","S60","S61","S63")
                       )
alternatives_cc <- list(list("Alt1", "Alt2", "Alt2", "Alt1", "Alt2"),list("Alt2", "Alt1", "Alt2", "Alt1", "Alt2"), list("Alt1", "Alt2", "Alt1", "Alt2"),
                        list("Alt2", "Alt1", "Alt2", "Alt2"), list("Alt1", "Alt2", "Alt2", "Alt1", "Alt2", "Alt1","Alt2"), list("Alt2", "Alt1", "Alt1", "Alt2", "Alt1", "Alt2"),
                        list("Alt1", "Alt1", "Alt1", "Alt2", "Alt1"), list("Alt2", "Alt1", "Alt1", "Alt1", "Alt2"))

attribute_ftt <- "Fastest_Travel_Time"
attribute_short_ftt <- "FTT"
scenarios_ftt <- list(list("S1","S3","S4","S5", "S7"),list("S11","S12","S13","S14","S15"),list("S17","S18","S19","S24"),list("S25","S26","S29","S30","S31","S32"),
                      list("S33","S34","S35", "S39", "S40"), list("S41","S42","S43","S44","S45","S47"), list("S50","S51","S54","S55","S56"),
                      list("S57","S59","S60","S61","S63","S64"))
alternatives_ftt <- list(list("Alt2", "Alt1", "Alt1", "Alt1", "Alt2"), list("Alt2", "Alt1", "Alt2", "Alt2", "Alt1"), list("Alt2", "Alt1", "Alt2", "Alt1"), list("Alt1", "Alt1", "Alt1", "Alt2", "Alt2","Alt2"),
                         list("Alt1", "Alt1", "Alt2", "Alt2", "Alt1"), list("Alt1", "Alt2", "Alt2", "Alt2", "Alt1", "Alt2"),list("Alt2", "Alt1", "Alt2", "Alt2", "Alt1"),
                         list("Alt1", "Alt1", "Alt2", "Alt2", "Alt1", "Alt2")
                         )

attribute_hcr <- "Highest_Congestion_Reduction"
attribute_short_hcr <- "HCR"
scenarios_hcr <- list(list("S2","S5","S6","S7","S8"),list("S9","S10","S13","S14","S15","S16"), list("S20","S21","S23"), list("S25","S26","S29","S30","S31","S32"),
                      list("S33","S35","S36", "S37", "S38", "S39"), list("S41","S42","S46","S47","S48"), list("S49","S50","S52","S53","S54","S55","S56"),
                      list("S57","S58","S59","S60","S61"))
alternatives_hcr <- list(list("Alt2", "Alt1", "Alt2", "Alt2", "Alt1"), list("Alt2", "Alt2", "Alt1", "Alt1", "Alt1", "Alt2"), list("Alt2", "Alt2", "Alt1"),list("Alt2", "Alt1", "Alt2", "Alt2", "Alt1","Alt1"),
                         list("Alt2", "Alt1", "Alt2", "Alt1", "Alt2", "Alt1"),list("Alt1", "Alt1", "Alt2", "Alt1", "Alt2"),list("Alt1", "Alt1", "Alt2", "Alt2", "Alt1", "Alt1", "Alt2"),
                         list("Alt1", "Alt2", "Alt2", "Alt2", "Alt1"))

attribute_ppb <- "Percentage_of_People_Benefiting"
attribute_short_ppb <- "PPB"
scenarios_ppb <- list(list("S1","S2","S3","S4","S5","S6","S7"),list("S9","S11","S12","S13","S15","S16"),list("S17","S21","S22","S23"), list("S25","S27", "S28","S30"),
                      list("S33","S35","S36", "S37","S40"), list("S41","S42","S46","S48"),  list("S49","S51","S52","S53","S54","S55"),
                      list("S57","S58","S60","S61","S62", "S64"))
alternatives_ppb <- list(list("Alt2", "Alt2", "Alt1", "Alt1", "Alt2","Alt2","Alt1"),list("Alt1", "Alt1", "Alt2", "Alt1", "Alt2","Alt2"), list("Alt1", "Alt1", "Alt2", "Alt2"), list("Alt1", "Alt2", "Alt2", "Alt1"),
                         list("Alt1", "Alt2", "Alt1", "Alt2", "Alt2"), list("Alt1", "Alt1", "Alt2", "Alt1"),list("Alt2", "Alt2", "Alt1", "Alt1", "Alt1","Alt2"),
                         list("Alt1", "Alt2", "Alt2", "Alt1", "Alt2","Alt1"))

num_blocks <- 8

lrs_general_function(csv_file, total_scenarios, num_blocks, scenarios_cc, alternatives_cc, attribute_cc, attribute_short_cc)
lrs_general_function(csv_file, total_scenarios, num_blocks, scenarios_ftt, alternatives_ftt, attribute_ftt, attribute_short_ftt)
lrs_general_function(csv_file, total_scenarios, num_blocks, scenarios_hcr, alternatives_hcr, attribute_hcr, attribute_short_hcr)
lrs_general_function(csv_file, total_scenarios, num_blocks, scenarios_ppb, alternatives_ppb, attribute_ppb, attribute_short_ppb)


attribute_ib <- "Inconsistent_Bias"
attribute_short_ib <- "IB"
scenarios_ib <- list("S19","S20")
alternatives_ib <- list(list("Alt2", "Alt2"),list("Alt1", "Alt1"))

attribute_nt <- "Non_Trading"
attribute_short_nt <- "NT"
scenarios_nt <- list( list("S1","S2","S3","S4","S5","S6","S7","S8"),  list("S9","S10","S11","S12","S13","S14","S15","S16"),list("S17","S18","S19","S20","S21","S22","S23","S24"), list("S25","S26","S27","S28","S29","S30","S31","S32"),
                      list("S33","S34","S35","S36","S37","S38","S39","S40"),  list("S41","S42","S43","S44","S45","S46","S47","S48"),  list("S49","S50","S51","S52","S53","S54","S55","S56"),
                      list("S57","S58","S59","S60","S61","S62","S63","S64"))
alternatives_nt <- list(list("Alt1", "Alt1","Alt1", "Alt1","Alt1", "Alt1","Alt1", "Alt1"),
                        list("Alt2", "Alt2","Alt2", "Alt2","Alt2", "Alt2","Alt2", "Alt2"),
                        list("Alt3", "Alt3","Alt3", "Alt3","Alt3", "Alt3","Alt3", "Alt3"))

#attribute_non_attendance_function(csv_file, total_scenarios, num_blocks, scenarios_ib, alternatives_ib, attribute_ib, attribute_short_ib)
attribute_non_attendance_function(csv_file, total_scenarios, num_blocks, scenarios_nt, alternatives_nt, attribute_nt, attribute_short_nt)

