#' Create a basic, valid LiPD object for testing
#'
#' @param dsn The dataSetName for the object
#' @param id The datasetId for the object
#' @return A list formatted as a valid LiPD object
create_test_lipd_object <- function(dsn = "TestDSN", id = paste0("TestID-", dsn)) {
  L <- list(
    "dataSetName" = dsn,
    "datasetId" = id,
    "archiveType" = "marine sediment",
    "lipdVersion" = 1.3,
    "datasetVersion" = "1.0.0",
    "createdBy" = "lipdR-test",
    "geo" = list(
      "longitude" = -105.0,
      "latitude" = 40.0,
      "siteName" = "Test Site"
    ),
    "pub" = list(
      list("author" = list(list(name = "Test Author, A.")))
    ),
    "changelog" = list(
      list(
        "version" = "1.0.0",
        "curator" = "test-user",
        "timestamp" = "2023-01-01T12:00:00Z",
        "notes" = "Initial creation for testing."
      )
    ),
    "paleoData" = list(
      list(
        "measurementTable" = list(
          list(
            "tableName" = "P1M1",
            "age" = list(
              "variableName" = "age",
              "units" = "yr BP",
              "TSid" = paste0("tsid-age-", dsn),
              "number" = 1,
              "values" = 1:5
            ),
            "temp" = list(
              "variableName" = "temp",
              "units" = "degC",
              "TSid" = paste0("tsid-temp-", dsn),
              "number" = 2,
              "values" = 20:24
            )
          )
        )
      )
    ),
    "chronData" = list(
      list(
        "measurementTable" = list(
          list(
            "tableName" = "C1M1",
            "depth" = list(
              "variableName" = "depth",
              "units" = "cm",
              "TSid" = paste0("tsid-depth-", dsn),
              "number" = 1,
              "values" = seq(10, 50, by = 10)
            ),
            "age" = list(
              "variableName" = "age",
              "units" = "yr BP",
              "TSid" = paste0("tsid-chronage-", dsn),
              "number" = 2,
              "values" = seq(100, 500, by = 100)
            )
          )
        )
      )
    )
  )
  return(new_lipd(L))
}



#' Create a mock neotoma2 site object for testing conversions
#' @return A neotoma2 site object
create_mock_neotoma_object <- function() {
  # This function requires neotoma2 and sf to be installed.
  # The tests that use it will be skipped if they are not.
  if (!requireNamespace("neotoma2", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
    return(NULL)
  }

  # 1. Create dummy sample data as a flat data frame
  samp_data <- data.frame(
    sample.id = c(1, 1, 2, 2), # Unique ID for each sample level
    depth = c(10, 10, 20, 20),
    age = c(1000, 1000, 2000, 2000),
    variable.name = c("Pollen", "Charcoal", "Pollen", "Charcoal"),
    value = c(100, 5, 120, 8),
    units = c("grains", "cm^2", "grains", "cm^2")
  )

  # 2. Create a list of data frames, one for each sample level.
  sample_data_list <- split(samp_data, f = samp_data$sample.id)

  # 3. Create individual 'sample' objects from the list of data frames.
  # This is the correct way to build the 'samples' object.
  sample_list <- purrr::map(sample_data_list, function(ss) {
    datum_df <- data.frame(
      variable.name = ss$variable.name,
      value = ss$value,
      units = ss$units
    )

    neotoma2::set_sample(
      sampleid = unique(ss$sample.id),
      depth = unique(ss$depth),
      ages = unique(ss$age),
      datum = datum_df
    )
  })

  # 4. Combine the list of 'sample' objects into a 'samples' object
  #samples <- neotoma2::set_samples(samples = sample_list)

  # 5. Create dummy chronology data
  chron_controls <- data.frame(
    depth = c(5, 25),
    thickness = c(1, 1),
    age = c(500, 2500),
    agelimityounger = c(450, 2450),
    agelimitolder = c(550, 2550),
    chroncontroltype = c("Radiocarbon", "Radiocarbon")
  )
  chronology <- neotoma2::set_chronology(
    chronologyid = 987,
    notes = "Test chronology",
    chroncontrols = chron_controls
  )

  # 6. Create publications
  pub <- neotoma2::set_publications(
    publicationid = 1,
    citation = "Test Scientist, A. (2024). A Fake Paper. Journal of Testing, 1(1), 1-10.",
    doi = "10.fake/doi"
  )
  publications <- neotoma2::set_publications(pub)

  # 7. Create the dataset, including the correctly built samples object
  dataset <- neotoma2::set_dataset(
    datasetid = 54321,
    datasettype = "pollen",
    notes = "Pollen data.",
    samples = sample_list,
    publications = publications
  )
  datasets <- neotoma2::set_datasets(dataset)

  # 8. Create the collection unit
  collunit <- neotoma2::set_collunit(
    collectionunitid = 111,
    handle = "TEST01",
    datasets = datasets,
    chronologies = neotoma2::set_chronologies(chronology)
  )
  collunits <- neotoma2::set_collunits(collunit)

  # 9. Finally, create the site
  site <- neotoma2::set_site(
    siteid = 123,
    sitename = "Neotoma Test Lake",
    geography = sf::st_as_sf(sf::st_sfc(sf::st_point(c(-100, 45)))),
    description = "A test site for conversions.",
    altitude = 1500,
    collunits = collunits
  )

  return(site)
}

