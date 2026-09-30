#' Sample CR-Birding dataset
#'
#' A sample CR-Birding dataset, formatted as exported from the website (in
#' English).
#'
#' This sample is derived from the study [Pied Avocet, white ring with 4 letters
#' (first=E)](https://submit.cr-birding.org/projects/37/), containing
#' observations of Pied avocet (_Recurvirostra avosetta_) ringed in Antwerp,
#' Belgium.
#' @source <https://submit.cr-birding.org/projects/37/>
#' @family sample data
#' @examples
#' \dontrun{
#' # The data in pied_avocet was created with the code below
#' pied_avocet <- readr::read_csv(
#'   system.file(
#'     "extdata/crbirding_export_project_37_en.csv",
#'     package = "crbirding"
#'   )
#' )
#' usethis::use_data(pied_avocet, overwrite = TRUE)
#' }
"pied_avocet"
