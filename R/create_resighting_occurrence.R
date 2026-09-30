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
    # Ringing event is the first observation that is either a capture or a
    # capture+release
    dplyr::filter(!.data$observation_id %in% ref_ids) |>
    dplyr::mutate(
      .keep = "none",
      basisOfRecord = "HumanObservation",
      occurrenceID = as.character(.data$observation_id),
      sex = NA_character_,
      lifeStage = dplyr::recode_values(
        .data$custom.status.full.grown.bird,
        "not applicable (chick)" ~ "juvenile"
      ),
      reproductiveCondition = dplyr::recode_values(
        .data$custom.status.full.grown.bird,
        "breeding bird" ~ "breeding",
      ),
      vitality = ifelse(.data$dead, "dead", "alive"),
      occurrenceStatus = "present",
      organismID = .data$bird_id,
      eventID = as.character(.data$observation_id),
      parentEventID = paste(
        .data$bird_id, .data$bird_shorthand_clean,
        sep = "_"
      ),
      eventType = dplyr::recode_values(
        .data$observation_type,
        "capture" ~ "capture",
        "release" ~ "release",
        "capture+release" ~ "capture+release",
        "dead" ~ "resighting",
        default = "resighting"
      ),
      eventDate = .data$observation_datetime,
      samplingProtocol = .data$eventType,
      eventRemarks = paste(
        "observation conditions:", .data$observation_condition
      ),
      decimalLatitude = .data$observation_lat,
      decimalLongitude = .data$observation_lon,
      geodeticDatum = "WGS84",
      identificationVerificationStatus =
        ifelse(
          .data$observation_status == "trusted",
          "verified by expert",
          NA_character_
        ),
      scientificNameID = .data$bird_aphia_lsid,
      scientificName = .data$bird_scientific_name,
      kingdom = "Animalia"
    )
}
