#!/usr/bin/Rscript
source("main.R")
library(testthat)

# Test for read_expression_table function
test_that("read_expression_table returns tibble with subject_id column", {
  result <- read_expression_table('example_intensity_data.csv')
  if (!"subject_id" %in% names(result)) {
    fail("subject_id column is missing from the returned tibble. Make sure to include this column in your function output.")
  }
  if (!is.tibble(result)) {
    fail("The returned result is not a tibble. Ensure your function returns a tibble.")
  }
})

# Test for period_to_underscore function
test_that("period_to_underscore replaces period with underscore", {
  result <- period_to_underscore("foo.bar")
  if (result != "foo_bar") {
    fail("The function did not replace periods with underscores correctly. Check the string manipulation in your function.")
  }
})


# Test for rename_and_select function
test_that("rename_and_select renames and selects the correct columns", {
  # Assuming metadata is a tibble with required columns
  result <- rename_and_select(metadata)
  expected_names <- c("Sex", "Age", "TNM_Stage", "Tumor_Location", "geo_accession", "KRAS_Mutation", "Subtype", "Batch")
  if (!identical(names(result), expected_names)) {
    fail("The returned tibble does not have the expected column names. Ensure your function renames and selects columns correctly.")
  }
})
