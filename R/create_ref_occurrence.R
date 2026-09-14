#' Create Darwin Core Occurrence from reference (ringing) data
#'
#' @param cleaned_data A data frame, as returned by `clean_data()`.
#' @returns A data frame with Darwin Core occurrences derived from ringing
#' events.
#' @family dwc functions
#' @noRd
create_ref_occurrence <- function(cleaned_data) {
  cleaned_data |>
    # Ringing event is the first observation that is either a capture or a
    # capture+release
    dplyr::filter(
      .data$observation_type %in% c("capture", "capture+release")
    ) |>
    dplyr::slice_head(n = 1, by = bird_id) |>
    dplyr::mutate(
      .keep = "none",
      basisOfRecord = "HumanObservation",
      occurrenceID = paste(
        .data$bird_id, .data$bird_shorthand_clean, "start",
        sep = "_" # Same as eventID
      ),
      sex = dplyr::recode_values(
        .data$bird_sex,
        "F" ~ "female",
        "M" ~ "male",
        "U" ~ "unknown"
      ),
      lifeStage = dplyr::recode_values(
        .data$bird_age_ringing,
        "pullus" ~ "juvenile",
        "unknown" ~ "unknown"
      ),
      bird_age_ringing = .data$bird_age_ringing, # Keep original value for emof
      reproductiveCondition = dplyr::recode_values(
        .data$custom.status.full.grown.bird,
        "breeding bird" ~ "reproductive",
      ),
      vitality = ifelse(.data$dead, "dead", "alive"),
      occurrenceStatus = "present",
      organismID = .data$bird_id,
      eventID = paste(
        .data$bird_id, .data$bird_shorthand_clean, "start",
        sep = "_" # Same as occurrenceID
      ),
      parentEventID = paste(
        .data$bird_id, .data$bird_shorthand_clean,
        sep = "_"
      ),
      eventType = "ringing",
      eventDate = .data$observation_datetime,
      samplingProtocol = "ringing",
      eventRemarks = paste(
        "observation condtions:", .data$observation_condition, "| ",
        "bird age:", .data$bird_age_ringing
      ),
      decimalLatitude = .data$observation_lat,
      decimalLongitude = .data$observation_lon,
      geodeticDatum = "WGS84",
      identificationVerificationStatus = "verified by expert",
      scientificNameID = .data$bird_species_euring,
      scientificName = .data$bird_scientific_name,
      kingdom = "Animalia",
      observation_id = .data$observation_id
    )
}
