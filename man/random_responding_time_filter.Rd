% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biasdetection_functions.R
\name{random_responding_time_filter}
\alias{random_responding_time_filter}
\title{Random responding time filter}
\usage{
random_responding_time_filter(rr_data, excluded_time_groups, num_time_groups)
}
\arguments{
\item{rr_data}{A dataframe containing unique responses (based on id) from the original panel data}

\item{excluded_time_groups}{A list containing the indices of excluded time groups. Excluded time groups could be the ones which record time spent by respondents in
filling up groups like introduction.}

\item{num_time_groups}{Number of time groups}
}
\value{
A list containing the updated data having a new column containing the percentage of random responding for each respondent
random_responding_time_filter()
}
\description{
Detects the random responses in the stated preference dataset. It is based on the time spent by respondents in filling up different parts of the survey. The
responses having time group values lower than the first quartiles of the respective time group are identified as random responding.
For each Time Group and for each respondent, the results of the random responses are recorded as binary (where 1 is true and 0 is false ).
It also creates 2 additional columns (RR_Total and percent_RR_Total) containing the sum of the random responses for each
Time Group, and the total percentage of random responses for each respondent, respectively.
}
\examples{
\dontrun{
random_responding_time_filter(dataframe, list(1,4,7), 8)
}
}
