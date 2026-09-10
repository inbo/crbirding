#' Create Darwin Core Occurrence from resighting data
#'
#' @param cleaned_data A data frame, as returned by `clean_data()`.
#' @param ref_ids A vector of observation IDs that are already included in the
#' reference occurrence data frame, as returned by `create_ref_occurrence()`.
#' @returns A data frame with Darwin Core occurrences derived from resighting
#' events.
#' @family dwc functions
#' @noRd
create_resighting_occurrence <- function(cleaned_data, ref_ids) {
  cleaned_data |>
    # Ringing event is the first observation that is either a capture or a capture+release
    dplyr::filter(!observation_id %in% ref_ids) |>
    dplyr::mutate(
      .keep = "none",
      basisOfRecord = "HumanObservation",
      occurrenceID = as.character(.data$observation_id),
      sex = NA_character_,
      lifeStage = dplyr::recode_values(
        .data$custom.status.full.grown.bird,
        "not applicable (chick)" ~ "pullus",
        "breeding bird" ~ "adult",
        "not a breeding bird" ~ "adult",
        "in colony, unknown if breeding" ~ "adult",
        "in colony, not breeding" ~ "adult",
        "unknown or unrecorded" ~ "unknown"
      ),
      reproductiveCondition = dplyr::recode_values(
        .data$custom.status.full.grown.bird,
        "breeding bird" ~ "reproductive",
      ),
      occurrenceStatus = "present",
      organismID = .data$bird_id,
      eventID = as.character(.data$observation_id),
      parentEventID = paste(.data$bird_id, .data$bird_shorthand_clean, sep = "_"),
      eventType = "observation",
      eventDate = observation_datetime,
      samplingProtocol = "bird_ring",
      decimalLatitude = observation_lat,
      decimalLongitude = observation_lon,
      identificationVerificationStatus = "verified by expert",
      scientificName = bird_scientific_name,
      kingdom = "Animalia"
    )
}
