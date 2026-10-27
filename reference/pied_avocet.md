# Sample CR-Birding dataset

A sample CR-Birding dataset, formatted as exported from the website (in
English).

## Usage

``` r
pied_avocet
```

## Format

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 340 rows and 31 columns.

## Source

<https://submit.cr-birding.org/projects/37/>

## Details

This sample is derived from the study [Pied Avocet, white ring with 4
letters (first=E)](https://submit.cr-birding.org/projects/37/),
containing observations of Pied avocet (*Recurvirostra avosetta*) ringed
in Antwerp, Belgium.

## Examples

``` r
if (FALSE) { # \dontrun{
# The data in pied_avocet was created with the code below
pied_avocet <- readr::read_csv(
  system.file(
    "extdata/crbirding_export_project_37_en.csv",
    package = "crbirding"
  )
)
usethis::use_data(pied_avocet, overwrite = TRUE)
} # }
```
