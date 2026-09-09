#' Transform CR-Birding data to a Darwin Core Archive
#'
#' Transforms data downloaded from [CR-Birding](https://cr-birding.org/) to a
#' [Darwin Core Archive](https://dwc.tdwg.org/text/).
#'
#' The resulting files can be uploaded to an [IPT](https://www.gbif.org/ipt) for
#' publication to GBIF and/or OBIS.
#'
#' @param data A data frame with all observation data from a certain CR-birding
#' project, downloaded from [CR-Birding](https://submit.cr-birding.org/). It is
#' expected to be in English.
#' @param directory Path to local directory to write files to.
#' @param dataset_id Identifier for the dataset.
#' @param dataset_name Title of the dataset.
#' @param license License of the dataset.
#' @param rights_holder Acronym of the organization owning or managing the
#'   rights over the data.
#' @returns CSV and `meta.xml` files written to disk.
#'   And invisibly, a data frame with the transformed data.
#' @family transformation functions
#' @export
#' @examples
#' write_dwc(example_dataset(), directory = "my_directory")
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_dwc <- function(data, directory, dataset_id = NULL, dataset_name = NULL,
                      license = NULL, rights_holder = NULL) {
  cleaned_data <- clean_data(data)
  ref_occurrence <- create_ref_occurrence(cleaned_data)
  # resighting_occurrence <- create_resighting_occurrence(cleaned_data)

  # Bind the occurrence df from the helper functions
  occurrence <-
    ref_occurrence |>
    # dplyr::bind_rows(resighting_occurrence) |>
    dplyr::mutate(
      # DATASET-LEVEL
      type = "Event",
      license = license,
      rightsHolder = rights_holder,
      datasetID = dataset_id,
      institutionCode = "Sovon",
      collectionCode = "CR-Birding",
      datasetName = dataset_name,
      .before = "basisOfRecord"
    ) |>
    dplyr::arrange(.data$parentEventID, .data$eventDate)

  # Write files
  occurrence_path <- file.path(directory, "occurrence.csv")
  meta_xml_path <- file.path(directory, "meta.xml")
  cli::cli_h2("Writing files")
  cli::cli_ul(c(
    "{.file {occurrence_path}}",
    "{.file {meta_xml_path}}",
  ))
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  readr::write_csv(occurrence, occurrence_path, na = "")
  file.copy(
    system.file("extdata", "meta.xml", package = "crbirding"), # Static meta.xml
    meta_xml_path
  )

  # Return Darwin Core data invisibly
  invisible(dplyr::as_tibble(occurrence))
}

