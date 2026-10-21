#' Create Darwin Core Occurrence from resighting data
#'
#' @param data A data frame, as returned by `clean_data()`.
#' @param ringing_id A vector of observation IDs that are already included in
#'   the ringing occurrences, as returned by `create_ringing_occurrence()`.
#' @returns A data frame with Darwin Core occurrences derived from resighting
#'   events.
#' @family dwc functions
#' @noRd
create_resighting_occurrence <- function(data, ringing_ids) {
  data |>
    # Filter out ringing observations
    dplyr::filter(!.data$observation_id %in% ringing_ids) |>
    dplyr::mutate(
      .keep = "none",
      # RECORD-LEVEL
      basisOfRecord = "HumanObservation",
      # OCCURRENCE
      occurrenceID = .data$observation_id,
      sex = dplyr::recode_values(
        .data$bird_sex,
        "F" ~ "female",
        "M" ~ "male",
        "U" ~ "unknown"
      ),
      lifeStage = dplyr::recode_values(
        .data$`custom:status full-grown bird`,
        "not applicable (chick)" ~ "juvenile",
        "unknown or unrecorded" ~ NA_character_,
        "breeding bird" ~ NA_character_,
        "not a breeding bird" ~ NA_character_,
        "in colony, unknown if breeding" ~ NA_character_,
        "in colony, not breeding" ~ NA_character_,
        NA ~ NA_character_,
        default = NA_character_
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
      eventID = .data$observation_id,
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
      # LOCATION
      decimalLatitude = .data$observation_lat,
      decimalLongitude = .data$observation_lon,
      geodeticDatum = "WGS84",
      # IDENTIFICATION
      identificationVerificationStatus = dplyr::if_else(
        .data$observation_status == "trusted",
        "verified by expert",
        NA_character_
      ),
      # TAXON
      scientificNameID = .data$bird_aphia_lsid,
      scientificName = .data$bird_scientific_name,
      kingdom = "Animalia"
    )
}
