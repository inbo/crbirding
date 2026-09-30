#' Create Extended Measurement Or Facts from Darwin Core Occurrence data
#'
#' Pulls the **sex** and **life stage** information from the Darwin Core
#' Occurrence data created with `create_ref_occurrence()` and maps these values
#' to a controlled vocabulary recommended by [OBIS](https://obis.org/).
#'
#' @param ref_occurrence Data frame with Darwin Core occurrences derived from
#'   ringing events, as returned by `create_ref_occurrence()`.
#' @returns Data frame with [Extended Measurement Or Facts](
#'   https://rs.gbif.org/extension/obis/extended_measurement_or_fact_2023-08-28.xml).
#' @family dwc functions
#' @noRd
create_ref_emof <- function(ref_occurrence) {
  sex <-
    ref_occurrence |>
    dplyr::mutate(
      .keep = "none",
      occurrenceID = .data$occurrenceID,
      measurementType = "sex",
      measurementTypeID =
        "http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/",
      measurementValue = .data$sex, # Value as is
      measurementValueID = dplyr::recode_values(
        .data$sex,
        "female" ~ "http://vocab.nerc.ac.uk/collection/S10/current/S102/",
        "male" ~ "http://vocab.nerc.ac.uk/collection/S10/current/S103/",
        "unknown" ~ "http://vocab.nerc.ac.uk/collection/S10/current/S105/", # indeterminate
        NA ~ "http://vocab.nerc.ac.uk/collection/S10/current/S104/", # not specified
        default = NA_character_ # Don't map other values
      ),
      measurementUnit = NA_character_,
      measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/XXXX/"
    )

  bird_age <-
    ref_occurrence |>
    dplyr::mutate(
      .keep = "none",
      occurrenceID = .data$occurrenceID,
      measurementType = "bird age",
      measurementTypeID = NA_character_, # chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://euring.org/files/documents/data_and_codes/code-manuel_new-euring_1979.pdf
      measurementValue = dplyr::recode_values(
        .data$bird_age_ringing,
        "pullus" ~ "pullus",
        "1 cy" ~ "1st calendar year",
        "2 cy" ~ "2st calendar year",
        "3 cy" ~ "3rd calendar year",
        "4 cy" ~ "4th calendar year",
        ">4 cy" ~ ">4th calendar year",
        "5 cy" ~ "5th calendar year",
        "unknown" ~ "unknown",
        default = "unknown"
      ),
      measurementValueID = NA_character_,
      measurementUnit = NA_character_,
      measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/XXXX/"
    )

  emof <-
    dplyr::bind_rows(sex, bird_age) |>
    dplyr::arrange(.data$occurrenceID)

  # Remove the measurementType if all values of that type are NA in ref_occurrence
  if (all(is.na(ref_occurrence$sex))) {
    emof <- dplyr::filter(emof, .data$measurementType != "sex")
  }
  if (all(is.na(ref_occurrence$bird_age))) {
    emof <- dplyr::filter(emof, .data$measurementType != "bird age")
  }

  return(emof)
}
