# `random_responding_time_filter`: Random responding time filter

## Description


 For each Time Group and for each respondent, the results of the random responses are recorded as binary.
 It also creates 2 additional columns (RR_Total and percent_RR_Total) containing the sum of the random responses for each
 Time Group, and the total percentage of random responses for each respondent, respectively.


## Usage

```r
random_responding_time_filter(rr_data, excluded_time_groups)
```


## Arguments

Argument      |Description
------------- |----------------
```rr_data```     |     A list containing unique responses (based on id) from the original panel data
```excluded_time_groups```     |     A list containing the indices of excluded time groups

## Value


 A list containing the updated data
 random_responding_time_filter()


