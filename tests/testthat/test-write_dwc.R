test_that("write_dwc() writes CSV and meta.xml files to a directory and
           a list of data frames invisibly", {
  skip_if_offline()
  data <- pied_avocet
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(data, temp_dir))

  expect_contains(
    list.files(temp_dir),
    c("emof.csv", "meta.xml", "occurrence.csv")
  )
  expect_named(result, c("occurrence", "emof"))
  expect_s3_class(result$occurrence, "tbl")
  expect_s3_class(result$emof, "tbl")
  expect_invisible(suppressMessages(write_dwc(data, temp_dir)))
})

test_that("write_dwc() returns the expected Darwin Core terms as columns", {
  skip_if_offline()
  data <- pied_avocet
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(data, temp_dir))

  expect_identical(
    colnames(result$occurrence),
    c(
      "type",
      "license",
      "rightsHolder",
      "datasetID",
      "institutionCode",
      "collectionCode",
      "datasetName",
      "basisOfRecord",
      "occurrenceID",
      "sex",
      "lifeStage",
      "reproductiveCondition",
      "vitality",
      "occurrenceStatus",
      "organismID",
      "eventID",
      "parentEventID",
      "eventType",
      "eventDate",
      "samplingProtocol",
      "decimalLatitude",
      "decimalLongitude",
      "geodeticDatum",
      "identificationVerificationStatus",
      "scientificNameID",
      "scientificName",
      "kingdom"
    )
  )
  expect_identical(
    colnames(result$emof),
    c(
      "occurrenceID",
      "measurementType",
      "measurementTypeID",
      "measurementValue",
      "measurementValueID",
      "measurementUnit",
      "measurementUnitID"
    )
  )
})

test_that("write_dwc() returns the expected Darwin Core mapping for the example
           dataset", {
  skip_if_offline()
  data <- pied_avocet
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  suppressMessages(write_dwc(data, temp_dir))

  expect_snapshot_file(file.path(temp_dir, "occurrence.csv"))
  expect_snapshot_file(file.path(temp_dir, "emof.csv"))
  expect_snapshot_file(file.path(temp_dir, "meta.xml"))
})

test_that("write_dwc() returns file that comply with the info in meta.xml", {
  skip_if_offline()
  data <- pied_avocet
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  suppressMessages(write_dwc(data, temp_dir))

  # Use helper function to compare
  expect_meta_match(file.path(temp_dir, "occurrence.csv"))
  expect_meta_match(file.path(temp_dir, "emof.csv"))
})

test_that("write_dwc() supports custom dataset id, name, license, rights_holder", {
  skip_if_offline()
  data <- pied_avocet
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(
    data,
    temp_dir,
    dataset_id = "custom_dataset_id",
    dataset_name = "custom_dataset_name",
    license = "custom_license",
    rights_holder = "custom_rights_holder"
  ))

  dataset_id <- purrr::pluck(result, "occurrence", "datasetID", 1)
  dataset_name <- purrr::pluck(result, "occurrence", "datasetName", 1)
  license <- purrr::pluck(result, "occurrence", "license", 1)
  rights_holder <- purrr::pluck(result, "occurrence", "rightsHolder", 1)

  expect_identical(dataset_id, "custom_dataset_id")
  expect_identical(dataset_name, "custom_dataset_name")
  expect_identical(license, "custom_license")
  expect_identical(rights_holder, "custom_rights_holder")
})
