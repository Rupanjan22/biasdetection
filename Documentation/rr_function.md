# `rr_function`: Write the updated .csv file

## Description


 Given a .csv file with panel data, excludes duplicate entries, applies time filter for random responses
 and writes the result in another .csv file.


## Usage

```r
rr_function(csv_file, excluded_time_groups)
```


## Arguments

Argument      |Description
------------- |----------------
```csv_file```     |     The file we want to process
```excluded_time_groups```     |     A list containing the indices of excluded time groups

## Value


 A list containing the updated data
 rr_function()


