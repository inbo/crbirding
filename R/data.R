#' Example CR-birding dataset
#'
#' Contains the
#' [CR-birding Pied Avocet data](https://submit.cr-birding.org/projects/37/)
#' downloaded on 08/09/2026.
#' @family sample data
#' @examples
#' \dontrun{
#' # The data in pied_avocet was created with the code below
#' pied_avocet <-
#'  read.csv(system.file("extdata", "crbirding_export_project_37.csv", package = "crbirding"))
#' usethis::use_data(pied_avocet, overwrite = TRUE)
#' }
"pied_avocet"
