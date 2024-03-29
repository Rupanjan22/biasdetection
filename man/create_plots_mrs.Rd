% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biasdetection_functions.R
\name{create_plots_mrs}
\alias{create_plots_mrs}
\title{Create Plots Mid point Response Style}
\usage{
create_plots_mrs(mrs_data, likert_columns)
}
\arguments{
\item{mrs_data}{Resulting data from the mrs_function}

\item{likert_columns}{A list containing the column names to use for MRS
create_plot_mrs()}
}
\description{
This function creates plots from the resulting data in mid point response style
(question and respondent wise bar plots).
}
\examples{
\dontrun{
create_plots_mrs(dataframe, list("ParkingCosts.SQ001.",
                                 "CongestionCosts.SQ001.",
                                 "LowIncLikert.SQ001.",
                                 "AffordableTrLikert.SQ001."))
}
}
