% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biasdetection_functions.R
\name{rr_time_filter}
\alias{rr_time_filter}
\title{Random response time filter}
\usage{
rr_time_filter(rr_data, num_time_groups)
}
\arguments{
\item{rr_data}{The dataframe used in detecting random responding and is generated from the original panel dataframe. It is a list containing unique responses (based on id) of the respondent.}

\item{num_time_groups}{Number of time groups is used in detecting random responding. It is generated from the groups of the survey. The dataframe contains different time groups (columns) where
each time group contains the time spent by each respondent in filling up different groups of the survey.
rr_time_filter()}
}
\description{
Calculates the first quartiles of each Time Group to identify the random responses. It displays them as well.
}
\examples{
\dontrun{
rr_time_filter(dataframe, 4)
}
}
