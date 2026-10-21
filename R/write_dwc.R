#' Transform CR-Birding data to a Darwin Core Archive
#'
#' Transforms observation data downloaded from [CR-Birding](
#' https://submit.cr-birding.org/) to a [Darwin Core Archive](
#' https://dwc.tdwg.org/text/).
#'
#' The resulting files can be uploaded to an [IPT](https://www.gbif.org/ipt) for
#' publication to GBIF and/or OBIS.
#'
#' @param data A data frame with observation data downloaded from [CR-Birding](
#'   https://submit.cr-birding.org/).
#'   Data are expected to be in English and from a single study.
#' @param directory Path to local directory to write files to.
#' @param dataset_id Identifier for the dataset.
#' @param dataset_name Title of the dataset. Defaults to `animal_project`.
#' @param license License of the dataset.
#' @param rights_holder Acronym of the organization owning or managing the
#'   rights over the data.
#' @returns CSV and `meta.xml` files written to disk.
#'   And invisibly, a list of data frames with the transformed data.
#' @family transformation functions
#' @export
#' @section Transformation details:
#' This function transforms data to:
#' - An [Occurrence core](
#'   https://rs.gbif.org/core/dwc_occurrence_2025-07-10.xml).
#' - An [Extended Measurement Or Facts extension](
#'   https://rs.gbif.org/extension/obis/extended_measurement_or_fact_2023-08-28.xml)
#' - A `meta.xml` file.
#'
#' Key features of the Darwin Core transformation:
#' - Deployments (animal+ring associations) are parent events, with ringing and
#'   resightings as child events.
#'   No information about the parent event is provided other than its ID,
#'   meaning that data can be expressed in an Occurrence core with one row per
#'   observation and `parentEventID` shared by all occurrences in a deployment.
#' - The ringing event often contains metadata about the animal (sex, bird age).
#'   Bird age is conservatively mapped to `lifeStage`, with pullus being mapped
#'   to juvenile.
#'   Other values are not mapped, as life stage is species-dependent.
#'   Sex and bird age are additionally provided in an Extended Measurement Or
#'   Facts extension, where values are mapped to a controlled vocabulary
#'   recommended by [OBIS](https://obis.org/).
#' - Observations without `bird_id` or with `observation_status = "impossible"`
#'   or `"unlikely"` are excluded.
#' - Parameters are used to set the following record-level terms:
#'   - `dwc:datasetID`: `dataset_id`.
#'   - `dwc:datasetName`: `dataset_name`.
#'   - `dcterms:license`: `license`.
#'   - `dcterms:rightsHolder`: `rights_holder`.
#'
#' @section Required data:
#' The source data should have at least the following fields:
#' - `observation_id`
#' - `observation_date`
#' - `observation_lat`
#' - `obervation_lon`
#' - `bird_species`
#' - `bird_id`
#' - `bird_shorthand`
#' - `observation_is_capture`
#' - `observation_status`
#' @examples
#' write_dwc(pied_avocet, directory = "my_directory")
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_dwc <- function(data, directory, dataset_id = NULL,
                      dataset_name = unique(data$animal_project),
                      license = NULL, rights_holder = NULL) {
  # Set properties to NA when missing
  dataset_id <- dataset_id %||% NA_character_
  license <- license %||% NA_character_
  rights_holder <- rights_holder %||% NA_character_

  # Check that dataset_name is a single value
  if (length(dataset_name) != 1) {
    cli::cli_abort(
      c(
        "i" = "{.var data} must be a single study with a single animal project
        and a single {.var dataset_name}.",
        "x" = "{.var dataset_name} has {length(dataset_name)} values."
      ),
      class = "crbirding_error_datasetname_invalid"
    )
  }

  # Clean and filter data
  data <-
    clean_data(data) |>
    # Exclude records without bird_id
    dplyr::filter(!is.na(.data$bird_id)) |>
    # Exclude impossible and unlikely records
    dplyr::filter(!.data$observation_status %in% c("impossible", "unlikely"))

  # Start transformation
  cli::cli_h2("Transforming data to Darwin Core")
  ringing_occurrence <- create_ringing_occurrence(data)
  ringing_ids <- dplyr::pull(ringing_occurrence, "observation_id")
  resighting_occurrence <- create_resighting_occurrence(data, ringing_ids)

  # Create extended measurements or facts
  emof <- create_ringing_emof(ringing_occurrence)

  # Bind occurrence data returned by helper functions
  occurrence <-
    ringing_occurrence |>
    dplyr::select(-"observation_id", -"bird_age_ringing") |>
    dplyr::bind_rows(resighting_occurrence) |>
    dplyr::mutate(
      # DATASET-LEVEL
      type = "Event",
      license = license,
      rightsHolder = rights_holder,
      datasetID = dataset_id,
      collectionCode = "CR-Birding",
      datasetName = dataset_name,
      .before = "basisOfRecord"
    ) |>
    dplyr::arrange(.data$parentEventID, .data$eventDate)

  # Write files
  occurrence_path <- file.path(directory, "occurrence.csv")
  meta_xml_path <- file.path(directory, "meta.xml")
  emof_path <- file.path(directory, "emof.csv")
  cli::cli_h2("Writing files")
  cli::cli_ul(c(
    "{.file {occurrence_path}}",
    "{.file {meta_xml_path}}",
    "{.file {emof_path}}"
  ))
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  readr::write_csv(occurrence, occurrence_path, na = "")
  readr::write_csv(emof, emof_path, na = "")
  file.copy(
    system.file("extdata", "meta.xml", package = "crbirding"), # Static meta.xml
    meta_xml_path
  )

  # Return list with Darwin Core data invisibly
  return <- list(
    occurrence = dplyr::as_tibble(occurrence),
    emof = dplyr::as_tibble(emof)
  )

  # Return Darwin Core data invisibly
  invisible(return)
}
