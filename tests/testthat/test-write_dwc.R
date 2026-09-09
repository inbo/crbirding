test_that("write_dwc() writes CSV and meta.xml files to a directory and
           a data frame invisibly", {
  skip_if_offline()
  data <- pied_avocet
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(data, temp_dir))

  expect_contains(
    list.files(temp_dir),
    c("meta.xml", "occurrence.csv")
  )
  expect_s3_class(result, "tbl")
  expect_invisible(suppressMessages(write_dwc(data, temp_dir)))
})

test_that("write_dwc() returns the expected Darwin Core terms as columns", {
  skip_if_offline()
  data <- pied_avocet
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(data, temp_dir))

  expect_identical(
    colnames(result),
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
      "occurrenceStatus",
      "organismID",
      "eventID",
      "parentEventID",
      "eventType",
      "eventDate",
      "samplingProtocol",
      "decimalLatitude",
      "decimalLongitude",
      "identificationVerificationStatus",
      "scientificName",
      "kingdom"
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
  # expect_snapshot_file(file.path(temp_dir, "emof.csv"))
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

  expect_identical(unique(result$datasetID), "custom_dataset_id")
  expect_identical(unique(result$datasetName), "custom_dataset_name")
  expect_identical(unique(result$license), "custom_license")
  expect_identical(unique(result$rightsHolder), "custom_rights_holder")
})
