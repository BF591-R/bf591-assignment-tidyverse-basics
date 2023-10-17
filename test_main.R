#!/usr/bin/Rscript

source("main.R")
library(testthat)
library(tidyverse)

describe("read_expression_table()", {
  result <- read_expression_table('data/example_intensity_data_subset.csv')
  
  it("created a tibble, not dataframe", {
    expect_true(is_tibble(result))
  })
  it("created a new column named subject_id in the dataframe", {
    expect_true("subject_id" %in% names(result))
  })
  it("returns a tibble with 35 rows and 1001 columns", {
    expect_equal(c(nrow(result), ncol(result)), c(35, 1001))
  })
})

describe("load_metadata()", {
  metadata <- load_metadata('data/proj_metadata.csv')
  
  it("returns the metadata as a tibble", {
    expect_true(is_tibble(metadata))
  })
  it("returns a tibble with exactly 134 rows, and 75 columns", {
    expect_equal(c(nrow(metadata), ncol(metadata)), c(134, 75))
  })
})

describe("period_to_underscore()", {
  result <- period_to_underscore("foo.bar.baz")
  
  #evaluates in order to catch functions that only replace one instance
  it("does not return a string with only the first instance replaced", {
    expect_false(result == "foo_bar.baz")
  })
  it("returns a string with all periods replaced by underscores", {
    expect_equal(result, "foo_bar_baz")
  })
})

describe("rename_and_select()", {
  set.seed(42)
  fake_tibble <- tibble(Age_at_diagnosis = floor(runif(10, min=30, max=100)), 
                        SixSubtypesClassification = c(rep('C4', 5), rep('C3', 5)),
                        normalizationcombatbatch= c(rep('batch1', 3), rep('batch2', 7)), 
                        Sex = c(rep('M', 3), rep('F', 7)), 
                        TNM_Stage = floor(runif(10, min=1, max=4)), 
                        Tumor_Location = c(rep('distal', 2), rep('proximal', 8)), 
                        geo_accession = paste0("GSM1293", 0:9), 
                        KRAS_Mutation = c(rep('-', 4), rep('+', 6)), 
                        extra = rep(1, 10), 
                        extra2 = rep(2, 10),
                        extra3 = rep(3, 10))
  
  result <- rename_and_select(fake_tibble)
  expected_names <- c("Sex", "Age", "TNM_Stage", "Tumor_Location", "geo_accession", "KRAS_Mutation", "Subtype", "Batch")

  it("returns the same column names as specified", {
    expect_true(setequal(names(result), expected_names))
  })  
})

describe("stage_as_factor()", {
  set.seed(42)
  fake_tibble <- tibble(Age = floor(runif(10, min=30, max=100)), 
                        Subtype = c(rep('C4', 5), rep('C3', 5)),
                        Batch= c(rep('batch1', 3), rep('batch2', 7)), 
                        Sex = c(rep('M', 3), rep('F', 7)), 
                        TNM_Stage = floor(runif(10, min=1, max=4)), 
                        Tumor_Location = c(rep('distal', 2), rep('proximal', 8)), 
                        geo_accession = paste0("GSM1293", 0:9), 
                        KRAS_Mutation = c(rep('-', 4), rep('+', 6)))
  
  result <- stage_as_factor(fake_tibble)
  
  it("returns a new column called Stage", {
    expect_true("Stage" %in% names(result))
  })
  it("converted the values of Stage to factors", {
    expect_true(is.factor(result$Stage))
  })
})

describe("mean_age_by_sex()", {
  set.seed(42)
  data <- tibble(
    Sex = sample(c("M", "F"), 100, replace=TRUE),
    Age = sample(15:90, 100, replace=TRUE)
  )
  
  m_result <- mean_age_by_sex(data, "M") %>% pull()
  f_result <- mean_age_by_sex(data, "F") %>% pull()
  
  it("correctly returns the average age for M", {
    expect_true(dplyr::near(c(m_result), c(54.20455), tol=.1))
  })
  it("correctly returns the average age for F", {
    expect_true(dplyr::near(c(f_result), c(47.69643), tol=.1))
  })
})

describe("age_by_stage()", {
  test_data <- tibble(
    Stage = c(rep('stage 1', 4), rep('stage 2', 3), rep('stage 3', 2), rep('stage 4', 1)),
    Age = c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
  )
  calculated_result <- age_by_stage(test_data)
  test_result <- calculated_result %>% pull(mean_avg, Stage)
  
  stage_names <- c("stage 1", "stage 2", "stage 3", 'stage 4')
  stage_avgs <- c(22.5, 26, 28.5, 30)
  stage_means <- setNames(stage_avgs, stage_names)
  
  it("returns a tibble", {
    expect_true(is_tibble(calculated_result))
  })
  it("correctly returns the right columns", {
    expect_true(all(c("Stage", "mean_avg") %in% colnames(calculated_result)))
  })
  it("returns the correct values for average age", {
    expect_mapequal(test_result, stage_means)
  })
})

describe("subtype_stage_cross_tab()", {
  test_stage <- tibble(Stage = c(rep('stage 4', 2), rep('stage 3', 1), rep('stage 1', 4), rep('stage 2', 3)), 
                       Subtype = c(rep('C4', 3), rep('C3', 7)))
  
  # Create expected cross-tabulated result
  test_crosstab <- tribble(
    ~Stage, ~C3, ~C4,
    "stage 1", 4, 0,
    "stage 2", 3, 0,
    "stage 3", 0, 1,
    "stage 4", 0, 2
  )
  
  calculated_crosstab <- subtype_stage_cross_tab(test_stage) %>% ungroup()
  
  it("should not retun any NA values", {
    expect_false(any(is.na(calculated_crosstab)))
  })
  it("should return Stage as a column", {
    expect_true("Stage" %in% colnames(calculated_crosstab))
  })
  it("should match the expected results based on the test crosstab", {
    expect_equal(test_crosstab, calculated_crosstab)
  })
})

describe("summarize_expression()", {
  exprs <- tibble(
    subject_id = c("A", "B"),
    probe1 = c(1, 2),
    probe2 = c(2, 7),
    probe3 = c(3, 15)
  )
  result <- summarize_expression(exprs)
  values_mean <- c(1.5, 4.5, 9)
  names_mean <- paste0("probe", 1:3)
  test_means <- setNames(values_mean, names_mean)
  
  exprs_mean_values <- c(result$mean_exp)
  exprs_mean_names <- c(result$probe)
  exprs_means <- setNames(exprs_mean_values, exprs_mean_names)
    
  values_var <- c(0.5, 12.5, 72)
  names_var <- c('probe1', 'probe2', 'probe3')
  test_vars <- setNames(values_var, names_var)
    
  exprs_var_values <- c(result$variance)
  exprs_var_names <- c(result$probe)
  exprs_vars <- setNames(exprs_var_values, exprs_var_names)
    
  it("should return the proper variance values from the test tibble", {
    expect_mapequal(exprs_vars, test_vars)
  })
  it("should return the proper mean values from the test tibble", {
    expect_mapequal(exprs_means, test_means)
  })
})