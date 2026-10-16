#' Create Darwin Core Occurrence from ringing event data
#'
#' @param data A data frame, as returned by `clean_data()`.
#' @returns A data frame with Darwin Core occurrences derived from ringing
#'   events.
#' @family dwc functions
#' @noRd
create_ringing_occurrence <- function(data) {
  data |>
    # Ringing event is the first observation that is either a capture or a
    # capture+release
    dplyr::filter(
      .data$observation_type %in% c("capture", "capture+release")
    ) |>
    dplyr::slice_head(n = 1, by = "bird_id") |>
    dplyr::mutate(
      .keep = "none",
      # RECORD-LEVEL
      basisOfRecord = "HumanObservation",
      # OCCURRENCE
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
      reproductiveCondition = dplyr::recode_values(
        .data$`custom:status full-grown bird`,
        "breeding bird" ~ "breeding",
      ),
      vitality = dplyr::if_else(.data$dead, "dead", "alive"),
      occurrenceStatus = "present",
      # ORGANISM
      organismID = .data$bird_id,
      # EVENT
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
        "observation conditions:", .data$observation_condition
      ),
      # LOCATION
      decimalLatitude = .data$observation_lat,
      decimalLongitude = .data$observation_lon,
      geodeticDatum = "WGS84",
      # IDENTIFICATION
      identificationVerificationStatus = "verified by expert",
      # TAXON
      scientificNameID = .data$bird_aphia_lsid,
      scientificName = .data$bird_scientific_name,
      kingdom = "Animalia",
      # Columns needed for resightings and emof
      observation_id = .data$observation_id,
      bird_age_ringing = .data$bird_age_ringing
    )
}
