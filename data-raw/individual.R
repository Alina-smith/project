## code to prepare `individual` dataset goes here
## functions used explained ----
# here
  # here - select files without having to give the whole file path
# readr
  # read_csv - read in csv files
# usethis
  # create_project - makes an R project in the working directory
  # use_data_raw - creates a raw-data folder within the working directory
  # use_course - downloads and unzips files
#purrr
  # map - applies a function to each element of a list or atomic vector - always returns a list
  # list_rbind - combines elements from a list into a dataframe
#fs
  # path - construct a path to a file or directory

## Setup ----
library(dplyr)

# Make directory folders - only do once
usethis::create_project("~/Desktop/wood-survey") # create a project within the working directory
usethis::use_data_raw(name = "individual") # create a raw-data folder within the working directory with a script called individual
usethis::use_data(individual, overwrite = TRUE)
usethis::use_course("bit.ly/wood-survey-data", destdir = "data-raw") # download and unzip data and save in data-raw

# Read in function code from other script
source(here::here("R", "geolocate.R")) # execute the code from the file we made the function in

## Combine individual tables ----
# Create paths to inputs - reads in data and assign it to a variable name
raw_data_path <- here::here("data-raw", "wood-survey-data-master") # path to all raw data files
individual_paths <- fs::dir_ls(fs::path(raw_data_path, "individual")) # path to the individual data files within raw_data_path and makes a list of all of the files within individual

# read in all the individual tables in the individual_paths list into one table
individual <- purrr::map(
  individual_paths,
  ~ readr::read_csv(
    file = .x,
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
) %>%
  purrr::list_rbind() %>%
  readr::type_convert()

# save this as a csv file
individual %>%
  readr::write_csv(file = fs::path(raw_data_path, "vst_individuals.csv"))

# Combine NEON data tables ----
# read in additional tables
maptag <- readr::read_csv(
  fs::path(
    raw_data_path,
    "vst_mappingandtagging.csv"
  ),
  show_col_types = FALSE
) %>%
  select(-eventID)

perplot <- readr::read_csv(
  fs::path(
    raw_data_path,
    "vst_perplotperyear.csv"
  ),
  show_col_types = FALSE
) %>%
  select(-eventID)

# Left join tables to individual
individual %<>%
  left_join(maptag,
            by = "individualID",
            suffix = c("", "_map")
  ) %>%
  left_join(perplot,
            by = "plotID",
            suffix = c("", "_ppl")
  ) %>%
  assertr::assert(
    assertr::not_na, stemDistance, stemAzimuth, pointID,
    decimalLongitude, decimalLatitude, plotID
  )

# Geolocate individual ----
individual <- individual %>%
  mutate(
    stemLat = get_stem_location(
      decimalLongitude, decimalLatitude,
      stemAzimuth, stemDistance
    )$lat,
    stemLon = get_stem_location(
      decimalLongitude, decimalLatitude,
      stemAzimuth, stemDistance
    )$lon
  ) %>% 
  janitor::clean_names()

# Saving data
fs::dir_create("data") # create data directory

individual %>% 
  readr::write_csv(here::here("data", "individual.csv")) # save as csv

## Dataspice ----
# packages
library(dataspice)

# Create metadata file
create_spice() # creates the metadata files
edit_creators() # takes you to the creater metadata page to edit it
prep_attributes() # auto fills in all columns into atributes meta page

# add in extra detail in the attributes file
variables <- readr::read_csv(
  here::here(
    "data-raw", "wood-survey-data-master",
    "NEON_vst_variables.csv"
  ),
  col_types = readr::cols(.default = "c")
) %>%
  dplyr::mutate(fieldName = janitor::make_clean_names(fieldName)) %>% #make_clean_names changes the names to snake case
  select(fieldName, description, units)

attributes <- readr::read_csv(here::here("data", "metadata", "attributes.csv")) %>%
  select(-description, -unitText)

dplyr::left_join(attributes, variables, by = c("variableName" = "fieldName")) %>%
  dplyr::rename(unitText = "units") %>%
  readr::write_csv(here::here("data", "metadata", "attributes.csv"))

# creat json-ld file
write_spice()
build_site()

jsonlite::read_json(here::here("data", "metadata", "dataspice.json")) %>%
  listviewer::jsonedit()

build_site(out_path = "data/index.html")

## ggplot ----
library(ggplot2)





