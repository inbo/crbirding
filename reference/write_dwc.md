# Transform CR-Birding data to a Darwin Core Archive

Transforms observation data downloaded from
[CR-Birding](https://submit.cr-birding.org/) to a [Darwin Core
Archive](https://dwc.tdwg.org/text/).

## Usage

``` r
write_dwc(
  data,
  directory,
  dataset_id = NULL,
  dataset_name = unique(data$animal_project),
  license = NULL,
  rights_holder = NULL
)
```

## Arguments

- data:

  A data frame with observation data downloaded from
  [CR-Birding](https://submit.cr-birding.org/). Data are expected to be
  in English and from a single study.

- directory:

  Path to local directory to write files to.

- dataset_id:

  Identifier for the dataset.

- dataset_name:

  Title of the dataset. Defaults to `animal_project`.

- license:

  License of the dataset.

- rights_holder:

  Acronym of the organization owning or managing the rights over the
  data.

## Value

CSV and `meta.xml` files written to disk. And invisibly, a list of data
frames with the transformed data.

## Details

The resulting files can be uploaded to an
[IPT](https://www.gbif.org/ipt) for publication to GBIF and/or OBIS.

## Transformation details

This function transforms data to:

- An [Occurrence
  core](https://rs.gbif.org/core/dwc_occurrence_2025-07-10.xml).

- An [Extended Measurement Or Facts
  extension](https://rs.gbif.org/extension/obis/extended_measurement_or_fact_2023-08-28.xml)

- A `meta.xml` file.

Key features of the Darwin Core transformation:

- Deployments (animal+ring associations) are parent events, with ringing
  and resightings as child events. No information about the parent event
  is provided other than its ID, meaning that data can be expressed in
  an Occurrence core with one row per observation and `parentEventID`
  shared by all occurrences in a deployment.

- The ringing event often contains metadata about the animal (sex, bird
  age). Bird age is conservatively mapped to `lifeStage`, with pullus
  being mapped to juvenile. Other values are not mapped, as life stage
  is species-dependent. Sex and bird age are additionally provided in an
  Extended Measurement Or Facts extension, where values are mapped to a
  controlled vocabulary recommended by [OBIS](https://obis.org/).

- Observations without `bird_id` or with
  `observation_status = "impossible"` or `"unlikely"` are excluded.

- Parameters are used to set the following record-level terms:

  - `dwc:datasetID`: `dataset_id`.

  - `dwc:datasetName`: `dataset_name`.

  - `dcterms:license`: `license`.

  - `dcterms:rightsHolder`: `rights_holder`.

## Required data

The source data should have at least the following fields:

- `observation_id`

- `observation_date`

- `observation_lat`

- `obervation_lon`

- `bird_species`

- `bird_id`

- `bird_shorthand`

- `observation_is_capture`

- `observation_status`

## Examples

``` r
write_dwc(pied_avocet, directory = "my_directory")
#> 
#> ── Transforming data to Darwin Core ──
#> 
#> ── Writing files ──
#> 
#> • my_directory/occurrence.csv
#> • my_directory/meta.xml
#> • my_directory/emof.csv

# Clean up (don't do this if you want to keep your files)
unlink("my_directory", recursive = TRUE)
```
