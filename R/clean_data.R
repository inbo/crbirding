clean_data <- function(data) {
  cleaned_data <-
  data |>
    # Convert types
    dplyr::mutate(
      observation_lat = as.numeric(observation_lat),
      observation_lon = as.numeric(observation_lon),
      observation_date = lubridate::ymd(observation_date),
      observation_time = dplyr::if_else(is.na(observation_time), "00:00:00", observation_time),
      observation_is_capture = dplyr::if_else(observation_is_capture == "Y", TRUE, FALSE)
    ) |>
    # Add columns
    dplyr::mutate(
      observation_datetime = as.POSIXct(paste(observation_date, observation_time), tz = "UTC"),
      dead = stringr::str_detect(observation_condition, "dead"),
      revalidation_release = dplyr::if_else(observation_condition == "released after revalidation", TRUE, FALSE),
      bird_scientific_name = dplyr::recode_values(
        bird_species,
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
        stringr::str_remove(bird_shorthand, "^[A-Za-z]-"), stringr::fixed(".")
      )
    ) |>
    # Exclude unwanted data
    dplyr::filter(
      !is.na(bird_id) & observation_status != "impossible"
    ) |>
    # Sort by bird_id, datetime and capture (important for mutate(.by) later)
    dplyr::arrange(bird_id, observation_datetime, dplyr::desc(observation_is_capture)) |>
    # Add row number per group
    dplyr::mutate(row_number = dplyr::row_number(), .by = bird_id) |>
    # Add observation type, see https://github.com/inbo/bird-tracking/issues/236
    dplyr::mutate(
      observation_type = dplyr::case_when(
        # Capture before release
        observation_is_capture & dplyr::lead(revalidation_release)
        ~ "capture",
        # Release after capture
        revalidation_release & dplyr::lag(observation_is_capture)
        ~ "release",
        # Single release (and therefore capture)
        revalidation_release
        ~ "capture+release",
        # Single capture (and therefore release)
        observation_is_capture &
          (!dplyr::lead(revalidation_release) | is.na(dplyr::lead(revalidation_release))) &
          (!dplyr::lag(observation_is_capture) | is.na(dplyr::lag(observation_is_capture)))
        ~ "capture+release",
        # Dead
        dead & (!dplyr::lag(dead) | is.na(dplyr::lag(dead)))
        ~ "dead"
      ),
      .by = bird_id
    ) |>
    # Select and order columns
    dplyr::select(
      bird_id,
      bird_shorthand,
      bird_shorthand_clean,
      bird_reference,
      bird_scientific_name,
      bird_sex,
      bird_age_ringing,
      observation_type,
      observation_is_capture,
      revalidation_release,
      dead,
      observation_id,
      row_number,
      observation_datetime,
      observation_lat,
      observation_lon,
      observation_region_euring,
      observation_condition,
      custom.status.full.grown.bird,
      inserted,
      observation_status,
      observation_status_reason
    )
}
