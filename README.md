---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# biasdetection

<!-- badges: start -->
<!-- badges: end -->

The goal of biasdetection is to ...

## Installation

You can install the released version of biasdetection from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("biasdetection")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
install.packages("devtools")
library(devtools)
devtools::install_github("Rupanjan22/biasdetection")
```
## Example to test package

This example will reproduce the results in the study project.

```
library(dplyr)
library(stringr)
library(rlist)
library(biasdetection)

csv_file <- <path_of_.csv_file>
excluded_time_groups <<- list(1) # excluding time_group1, as it included only introduction and description of the survey which people might overlook
rr_data <- rr_function(csv_file, excluded_time_groups)

create_plots_rr(rr_data, excluded_time_groups)

likert_columns <- list("ParkingCosts.SQ001.", "CongestionCosts.SQ001.", "LowIncLikert.SQ001.", "AffordableTrLikert.SQ001.")
max_value <- 5
min_value <- 1
ers_data <- ers_function(csv_file, likert_columns, max_value, min_value)
create_plots_ers(ers_data)

likert_columns <- list("ParkingCosts.SQ001.", "CongestionCosts.SQ001.", "LowIncLikert.SQ001.", "AffordableTrLikert.SQ001.")
mid_value <- 3
mrs_data <- mrs_function(csv_file, likert_columns, mid_value)
create_plots_mrs(mrs_data)


total_scenarios <- list("S17","S18","S19","S20","S21","S22","S23","S24")

attribute_cc <- "Cheapest Cost"
attribute_short_cc <- "CC"
scenarios_cc <- list("S17","S19","S20","S21","S22")
alternatives_cc <- list("Alt2", "Alt1", "Alt2", "Alt1", "Alt2")

attribute_ftt <- "Fastest Travel Time"
attribute_short_ftt <- "FTT"
scenarios_ftt <- list("S19","S20","S21","S22","S23")
alternatives_ftt <- list("Alt2", "Alt1", "Alt2", "Alt2", "Alt1")

attribute_hcr <- "Highest Congestion Reduction"
attribute_short_hcr <- "HCR"
scenarios_hcr <- list("S17","S18","S21","S22","S23","S24")
alternatives_hcr <- list("Alt2", "Alt2", "Alt1", "Alt1", "Alt1", "Alt2")

attribute_ppb <- "Percentage of People Benefiting"
attribute_short_ppb <- "PPB"
scenarios_ppb <- list("S17","S19","S20","S21","S23","S24")
alternatives_ppb <- list("Alt1", "Alt1", "Alt2", "Alt1", "Alt2", "Alt2")


lrs_data <- lrs_general_function(csv_file, total_scenarios, scenarios_cc, alternatives_cc, attribute_cc, attribute_short_cc)
lrs_data <- lrs_general_function(csv_file, total_scenarios, scenarios_ftt, alternatives_ftt, attribute_ftt, attribute_short_ftt)
lrs_data <- lrs_general_function(csv_file, total_scenarios, scenarios_hcr, alternatives_hcr, attribute_hcr, attribute_short_hcr)
lrs_data <- lrs_general_function(csv_file, total_scenarios, scenarios_ppb, alternatives_ppb, attribute_ppb, attribute_short_ppb)

total_scenarios <- list("S17","S18","S19","S20","S21","S22","S23","S24")

attribute_ib <- "Inconsistent Bias"
attribute_short_ib <- "IB"
scenarios_ib <- list("S19","S20")
alternatives_ib <- list(list("Alt2", "Alt2"),list("Alt1", "Alt1"))

attribute_nt <- "Non Trading"
attribute_short_nt <- "NT"
scenarios_nt <- list("S17","S18","S19","S20","S21","S22","S23","S24")
alternatives_nt <- list(list("Alt1", "Alt1","Alt1", "Alt1","Alt1", "Alt1","Alt1", "Alt1"),
                               list("Alt2", "Alt2","Alt2", "Alt2","Alt2", "Alt2","Alt2", "Alt2"),
                               list("Alt3", "Alt3","Alt3", "Alt3","Alt3", "Alt3","Alt3", "Alt3"))

attribute_non_attendance_function(csv_file, total_scenarios, scenarios_ib, alternatives_ib, attribute_ib, attribute_short_ib)
attribute_non_attendance_function(csv_file, total_scenarios, scenarios_nt, alternatives_nt, attribute_nt, attribute_short_nt)

```

