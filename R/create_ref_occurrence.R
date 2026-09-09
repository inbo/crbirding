#' Create Darwin Core Occurrence from reference (ringing) data
#'
#' @param cleaned_data A data frame, as returned by `clean_data()`.
#' @returns A data frame with Darwin Core occurrences derived from ringing
#' events.
#' @family dwc functions
#' @noRd
create_ref_occurrence <- function(cleaned_data) {
  cleaned_data |>
    # Ringing event is the first observation that is either a capture or a capture+release
    dplyr::filter(observation_type %in% c("capture", "capture+release")) |>
    dplyr::slice_head(n = 1, by = bird_id) |>
    dplyr::mutate(
      .keep = "none",
      basisOfRecord = "HumanObservation",
      occurrenceID = paste(
        .data$bird_id, .data$bird_shorthand_clean, "start",
        sep = "_" # Same as eventID
      ),
      sex = dplyr::case_when(
        bird_sex == "F" ~ "female",
        bird_sex == "M" ~ "male",
        bird_sex == "U" ~ "unknown"
      ),
      lifeStage = dplyr::case_when(
        bird_age_ringing == "pullus" ~ "pullus",
        bird_age_ringing == "1 cy" ~ "1st calendar year",
        bird_age_ringing == "2 cy" ~ "2st calendar year",
        bird_age_ringing == "3 cy" ~ "3rd calendar year",
        bird_age_ringing == "4 cy" ~ "4th calendar year",
        bird_age_ringing == ">4 cy" ~ ">4th calendar year",
        bird_age_ringing == "5 cy" ~ "5th calendar year",
        bird_age_ringing == "unknown" ~ "unknown",
        TRUE ~ "unknown"
      ),
      occurrenceStatus = "present",
      organism_id = bird_id,
      eventID = paste(
        .data$bird_id, .data$bird_shorthand_clean, "start",
        sep = "_" # Same as occurrenceID
      ),
      parentEventID = paste(.data$bird_id, .data$bird_shorthand_clean, sep = "_"),
      eventType = "ringing",
      eventDate = observation_datetime,
      samplingProtocol = "ringing",
      decimalLatitude = observation_lat,
      decimalLongitude = observation_lon,
      identificationVerificationStatus = "verified by expert",
      scientificName = bird_scientific_name,
      kingdom = "Animalia"
    )
}
