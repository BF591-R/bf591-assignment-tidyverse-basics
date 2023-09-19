#!/usr/bin/Rscript
source("main.R")
library(testthat)

# Test for read_expression_table function
test_that("read_expression_table returns tibble with subject_id column", {
  result <- read_expression_table('example_intensity_data.csv')
  
  # Check for subject_id in column names
  expect_true("subject_id" %in% names(result), 
              info = "subject_id column is missing from the returned tibble. Make sure to include this column in your function output.")
  
  # Check if result is a tibble
  expect_true(is_tibble(result), 
              info = "The returned result is not a tibble. Ensure your function returns a tibble.")
})

# Test for period_to_underscore function
test_that("period_to_underscore replaces period with underscore", {
  result <- period_to_underscore("foo.bar.baz")
  
  # Check string manipulation
  expect_equal(result, "foo_bar_baz", 
               info = "The function did not replace periods with underscores correctly. Check the string manipulation in your function.")
})


# Test for rename_and_select function
test_that("rename_and_select renames and selects the correct columns", {
  #metadata is a tibble with required columns
  result <- rename_and_select(metadata)
  expected_names <- c("Sex", "Age", "TNM_Stage", "Tumor_Location", "geo_accession", "KRAS_Mutation", "Subtype", "Batch")
  if (!identical(names(result), expected_names)) {
    fail("The returned tibble does not have the expected column names. Ensure your function renames and selects columns correctly.")
  }
})



# Test for stage_as_factor function
test_that("stage_as_factor adds a Stage column with the correct format", {
  #data is a tibble with TNM_Stage column
  result <- stage_as_factor(data)
  
  if (!"Stage" %in% names(result)) {
    fail("Stage column is missing from the returned tibble. Ensure your function adds this column.")
  }
  
  if (!is.factor(result$Stage)) {
    fail("Stage column should be of type factor. Make sure you've set it as such.")
  }
  
  if (!all(grepl("^stage ", result$Stage))) {
    fail("Entries in the Stage column do not follow the 'stage x' format. Double-check the string manipulation in your function.")
  }
})

# Test for mean_age_by_sex function
test_that("mean_age_by_sex calculates correct mean age for given sex", {
  for (sex in c("M", "F")) {
    mean_age <- mean(data[data$Sex == sex, ]$Age, na.rm = TRUE)
    result <- mean_age_by_sex(data, sex)
    
    if (!identical(result, mean_age)) {
      fail(paste("The calculated mean age for", sex, "does not match the expected value. Check the aggregation logic in your function."))
    }
  }
})


# Test for age_by_stage function
test_that("age_by_stage calculates average age per stage correctly", {
  
  result <- tryCatch({
    age_by_stage(data)
  }, error = function(e) {
    stop("Error executing the age_by_stage function. Please check the function implementation.")
  })
  
  tryCatch({
    # Check that result has two columns
    expect_equal(ncol(result), 2)
  }, error = function(e) {
    stop("The result should have two columns.")
  })
  
  tryCatch({
    # Check that one column matches the unique values of 'Stage' from the data
    expect_true(any(identical(result[[1]], unique(data$Stage)) | 
                      identical(result[[2]], unique(data$Stage))))
  }, error = function(e) {
    stop("One column of the result should match the unique values of 'Stage' from the data.")
  })
  
  tryCatch({
    # Check that the other column contains only numeric values
    if (is.numeric(result[[1]])) {
      expect_true(all(is.numeric(result[[1]])))
    } else {
      expect_true(all(is.numeric(result[[2]])))
    }
  }, error = function(e) {
    stop("One of the columns in the result should contain only numeric values.")
  })
})



# Test for subtype_stage_cross_tab function
test_that("subtype_stage_cross_tab returns correct cross-tabulated table", {
  result <- subtype_stage_cross_tab(data)
  
  if (!"Stage" %in% rownames(result) || !"Subtype" %in% colnames(result)) {
    fail("Your result table should have 'Stage' as rows and 'Subtype' as columns.")
  }
  
  if (any(is.na(result))) {
    fail("Your table should not have any NA values. Fill missing pairs with zeros.")
  }
})


# Test for summarize_expression function
test_that("summarize_expression behaves as expected", {
  
  # Read expression matrix
  exprs <- read.csv("data/example_intensity_data.csv")
  
  
  result <- tryCatch({
    summarize_expression(exprs)
  }, error = function(e) {
    stop("Error executing the summarize_expression function. Please check the function implementation.")
  })
  
  tryCatch({
    # Check that result is a tibble
    expect_true(is_tibble(result))
  }, error = function(e) {
    stop("The returned result is not a tibble. Ensure your function returns a tibble.")
  })
  
  tryCatch({
    # Check that result has the expected columns
    expect_identical(names(result), c("main_exp", "variance", "probe"))
  }, error = function(e) {
    stop("The returned tibble does not have the expected column names.")
  })
  
  tryCatch({
    # Check number of rows
    expect_equal(nrow(result), ncol(exprs))
  }, error = function(e) {
    stop("Mismatch in the number of rows of the result compared to the number of columns in the expression matrix.")
  })
  
  tryCatch({
    # Check values in main_exp column
    expected_means <- colMeans(exprs)
    expect_identical(result$main_exp, expected_means)
  }, error = function(e) {
    stop("Mismatch between the expected means and the means in the main_exp column.")
  })
  
  tryCatch({
    # Check values in variance column
    expected_variances <- apply(exprs, 2, var)
    expect_identical(result$variance, expected_variances)
  }, error = function(e) {
    stop("Mismatch between the expected variances and the variances in the variance column.")
  })
  
  tryCatch({
    # Check values in probe column
    expect_identical(result$probe, colnames(exprs))
  }, error = function(e) {
    stop("Mismatch between the expected probe names and the probe names in the probe column.")
  })
})






