#' Clean data
#'
#' @param data A data frame with observation data downloaded from [CR-Birding](
#'   https://submit.cr-birding.org/).
#' @returns The data frame with converted, added and reordered columns.
#' @noRd
clean_data <- function(data) {
  data <-
    data |>
    # Convert types
    dplyr::mutate(
      observation_date = lubridate::ymd(.data$observation_date),
      observation_time = dplyr::if_else(
        !is.na(.data$observation_time),
        as.character(.data$observation_time),
        "00:00:00"
      ),
      observation_lat = round(as.numeric(.data$observation_lat), digits = 6),
      observation_lon = round(as.numeric(.data$observation_lon), digits = 6),
      observation_is_capture = dplyr::if_else(
        observation_is_capture == "Y", TRUE, FALSE
      ),
      observation_is_certain = dplyr::if_else(
        observation_is_certain == "Y", TRUE, FALSE
      )
    ) |>
    # Add columns
    dplyr::mutate(
      observation_datetime = as.POSIXct(
        paste(.data$observation_date, .data$observation_time),
        tz = "UTC"
      ),
      dead = stringr::str_detect(.data$observation_condition, "dead"),
      revalidation_release = dplyr::if_else(
        .data$observation_condition == "released after revalidation",
        TRUE,
        FALSE
      ),
      bird_scientific_name = dplyr::recode_values(
        .data$bird_species,
        "Lesser Black-backed Gull" ~ "Larus fuscus", # 5910
        "European Herring Gull" ~ "Larus argentatus", # 5920
        "Yellow-legged Gull" ~ "Larus michahellis", # 5926
        "Gull" ~ "Larus", # 6009
        "large gull (hybrid)" ~ "Larus", # 6008
        "large gull (species unknown)" ~ "Larus",
        "Pied Avocet" ~ "Recurvirostra avosetta",
        "Great Cormorant" ~ "Phalacrocorax carbo"
      ),
      bird_shorthand_clean = stringr::str_remove(
        stringr::str_remove(.data$bird_shorthand, "^[A-Za-z]-"),
        stringr::fixed(".")
      )
    ) |>
    # Sort by bird_id, datetime and capture (important for mutate (.by) later)
    dplyr::arrange(
      .data$bird_id,
      .data$observation_datetime,
      dplyr::desc(.data$observation_is_capture)
    ) |>
    # Add row number per group
    dplyr::mutate(row_number = dplyr::row_number(), .by = "bird_id") |>
    # Add observation type, see https://github.com/inbo/bird-tracking/issues/236
    dplyr::mutate(
      observation_type = dplyr::case_when(
        # Capture before release
        .data$observation_is_capture & dplyr::lead(.data$revalidation_release)
        ~ "capture",
        # Release after capture
        .data$revalidation_release & dplyr::lag(.data$observation_is_capture)
        ~ "release",
        # Single release (and therefore capture)
        .data$revalidation_release
        ~ "capture+release",
        # Single capture (and therefore release)
        .data$observation_is_capture &
          (!dplyr::lead(.data$revalidation_release) |
           is.na(dplyr::lead(.data$revalidation_release))) &
          (!dplyr::lag(.data$observation_is_capture) |
           is.na(dplyr::lag(.data$observation_is_capture)))
        ~ "capture+release",
        # Dead
        dead & (!dplyr::lag(.data$dead) | is.na(dplyr::lag(.data$dead)))
        ~ "dead"
      ),
      .by = "bird_id"
    )

  # Get Aphia lsids for each scientific name
  names <- dplyr::pull(dplyr::distinct(data, bird_scientific_name))
  aphia_ids <-
    movepub::get_aphia_id(names) |>
    dplyr::select("name", "aphia_lsid") |>
    dplyr::rename(
      bird_scientific_name = "name",
      bird_aphia_lsid = "aphia_lsid"
    )

  # Join observations with aphia_id and reorder columns
  data |>
    dplyr::left_join(aphia_ids, by = c("bird_scientific_name")) |>
    dplyr::relocate(
      "bird_id",
      "bird_shorthand",
      "bird_shorthand_clean",
      "bird_reference",
      "bird_scientific_name",
      "bird_aphia_lsid",
      "bird_sex",
      "bird_age_ringing",
      "bird_species_euring",
      "observation_type",
      "observation_is_capture",
      "revalidation_release",
      "dead",
      "observation_id",
      "row_number",
      "observation_datetime",
      "observation_lat",
      "observation_lon",
      "observation_region_euring",
      "observation_condition",
      "inserted",
      "observation_status",
      "observation_status_reason"
    )
}
