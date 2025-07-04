get_lsmeans_example <- function() {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm_test_data
  )
  emmeans_res <- h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = "proportional"
  )
  list(
    vars = vars,
    fit = fit,
    emmeans_res = emmeans_res
  )
}

get_lsmeans_example_no_arm <- function() {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ AVISIT + us(AVISIT | USUBJID),
    data = mmrm_test_data,
    optimizer = "nlminb"
  )
  emmeans_res <- h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = "proportional"
  )
  list(
    vars = vars,
    fit = fit,
    emmeans_res = emmeans_res
  )
}

# h_get_emmeans_res ----

test_that("h_get_emmeans_res works as expected", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm_test_data
  )
  weights <- "proportional"
  result <- expect_silent(h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = weights
  ))
  expect_list(result)
  expect_class(result$object, "emmGrid")
  expect_class(result$grid, "data.frame")
  expect_named(result$grid, c("AVISIT", "ARMCD", "n"))
  expect_identical(nrow(result$grid), nrow(result$object@grid))

  datfull <- na.omit(fit$data)
  assert_true(identical(nrow(datfull), length(fit$tmb_data$y_vector)))
  ns <- datfull %>%
    dplyr::group_by(ARMCD, AVISIT) %>%
    dplyr::summarize(n_expected = dplyr::n())
  compare_grid <- result$grid %>% dplyr::full_join(ns, by = dplyr::join_by(AVISIT, ARMCD))
  expect_identical(compare_grid$n, compare_grid$n_expected)
})

test_that("h_get_emmeans_res works as expected without arm variable", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ AVISIT + us(AVISIT | USUBJID),
    data = mmrm_test_data,
    optimizer = "nlminb"
  )
  weights <- "proportional"
  result <- expect_silent(h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = weights
  ))
  expect_list(result)
  expect_class(result$object, "emmGrid")
  expect_class(result$grid, "data.frame")
  expect_named(result$grid, c("AVISIT", "n"))
  expect_identical(nrow(result$grid), nrow(result$object@grid))
})

# h_get_average_visit_specs ----

test_that("h_get_average_visit_specs works as expected", {
  example <- get_lsmeans_example()
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  result <- expect_silent(h_get_average_visit_specs(
    emmeans_res = example$emmeans_res,
    vars = example$vars,
    averages = averages,
    fit = example$fit
  ))
  expected <- list(
    coefs = list(
      "PBO.VIS1+3" = c(0.5, 0, 0.5, 0, 0, 0, 0, 0),
      "TRT.VIS1+3" = c(0, 0, 0, 0, 0.5, 0, 0.5, 0),
      "PBO.VIS2+4" = c(0, 0.5, 0, 0.5, 0, 0, 0, 0),
      "TRT.VIS2+4" = c(0, 0, 0, 0, 0, 0.5, 0, 0.5)
    ),
    grid = data.frame(
      ARMCD = c("PBO", "TRT", "PBO", "TRT"),
      AVISIT = c("VIS1+3", "VIS1+3", "VIS2+4", "VIS2+4"),
      n = c(94L, 85L, 91L, 88L)
    )
  )
  expect_identical(result, expected)
})

test_that("h_get_average_visit_specs gives error if not all visits are correct", {
  example <- get_lsmeans_example()
  averages <- list(
    "VIS4+5" = c("VIS4", "VIS5") # VIS5 is not included in data.
  )
  expect_error(
    h_get_average_visit_specs(
      emmeans_res = example$emmeans_res,
      vars = example$vars,
      averages = averages,
      fit = example$fit
    ),
    "Must be a subset of {'VIS1','VIS2','VIS3','VIS4'}",
    fixed = TRUE
  )
})

test_that("h_get_average_visit_specs works as expected without arm", {
  example <- get_lsmeans_example_no_arm()
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  result <- expect_silent(h_get_average_visit_specs(
    emmeans_res = example$emmeans_res,
    vars = example$vars,
    averages = averages,
    fit = example$fit
  ))
  expected <- list(
    coefs = list(
      "VIS1+3" = c(0.5, 0, 0.5, 0),
      "VIS2+4" = c(0, 0.5, 0, 0.5)
    ),
    grid = data.frame(
      AVISIT = c("VIS1+3", "VIS2+4"),
      n = c(179L, 179L)
    )
  )
  expect_identical(result, expected)
})

test_that("h_get_average_visit_specs uses number of patients with any of the averaged visits as n", {
  skip_on_ci()

  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ AVISIT + ar1(AVISIT | USUBJID),
    data = mmrm_test_data
  )
  emmeans_res <- h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = "proportional"
  )
  averages <- list(
    "V1+3" = c("VIS1", "VIS3"),
    "V2+4" = c("VIS2", "VIS4")
  )
  avg_specs <- h_get_average_visit_specs(
    emmeans_res = emmeans_res,
    vars = vars,
    averages = averages,
    fit = fit
  )
  result <- avg_specs$grid
  expected <- data.frame(
    AVISIT = c("V1+3", "V2+4"),
    n = c(179L, 179L)
  )
  expect_identical(result, expected)
})

# h_get_spec_visit_estimates ----

test_that("h_get_spec_visit_estimates works as expected", {
  example <- get_lsmeans_example()
  specs <- list(
    coefs = list(
      "PBO.VIS1+3" = c(0.5, 0, 0.5, 0, 0, 0, 0, 0),
      "TRT.VIS1+3" = c(0, 0, 0, 0, 0.5, 0, 0.5, 0)
    ),
    grid = data.frame(
      ARMCD = c("PBO", "TRT"),
      AVISIT = c("VIS1+3", "VIS1+3"),
      n = c(68L, 67L)
    )
  )
  result <- expect_silent(h_get_spec_visit_estimates(
    emmeans_res = example$emmeans_res,
    specs = specs,
    conf_level = 0.9
  ))
  expected <- data.frame(
    ARMCD = c("PBO", "TRT"),
    AVISIT = c("VIS1+3", "VIS1+3"),
    n = c(68L, 67L),
    estimate = c(37.86, 41.91),
    se = c(0.517, 0.541),
    df = c(171.1, 171.9),
    lower_cl = c(37, 41),
    upper_cl = c(38.7, 42.8)
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("h_get_spec_visit_estimates produces test results optionally", {
  example <- get_lsmeans_example()
  specs <- list(
    coefs = list(
      "PBO.VIS1+3" = c(0.5, 0, 0.5, 0, 0, 0, 0, 0),
      "TRT.VIS1+3" = c(0, 0, 0, 0, 0.5, 0, 0.5, 0)
    ),
    grid = data.frame(
      ARMCD = c("PBO", "TRT"),
      AVISIT = c("VIS1+3", "VIS1+3"),
      n = c(68L, 67L)
    )
  )
  result <- expect_silent(h_get_spec_visit_estimates(
    emmeans_res = example$emmeans_res,
    specs = specs,
    conf_level = 0.9,
    tests = TRUE
  ))
  expected <- data.frame(
    ARMCD = c("PBO", "TRT"),
    AVISIT = c("VIS1+3", "VIS1+3"),
    n = c(68L, 67L),
    estimate = c(37.86, 41.91),
    se = c(0.517, 0.541),
    df = c(171.1, 171.9),
    lower_cl = c(37, 41),
    upper_cl = c(38.7, 42.8),
    t_stat = c(73.2, 77.4),
    p_value = c(4.5e-131, 1.5e-135)
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("h_get_spec_visit_estimates works without arm", {
  example <- get_lsmeans_example_no_arm()
  specs <- list(
    coefs = list(
      "VIS1+3" = c(0.5, 0, 0.5, 0)
    ),
    grid = data.frame(
      AVISIT = "VIS1+3",
      n = 129L
    )
  )
  result <- expect_silent(h_get_spec_visit_estimates(
    emmeans_res = example$emmeans_res,
    specs = specs,
    conf_level = 0.9
  ))
  expect_snapshot_value(result, tolerance = 1e-3, style = "serialize")
})

# h_get_single_visit_estimates ----

test_that("h_get_single_visit_estimates works as expected", {
  example <- get_lsmeans_example()
  result <- expect_silent(h_get_single_visit_estimates(
    emmeans_res = example$emmeans_res,
    conf_level = 0.8
  ))
  expect_snapshot_value(result, tolerance = 1e-3, style = "serialize")
})

test_that("h_get_single_visit_estimates works without arm", {
  example <- get_lsmeans_example_no_arm()
  result <- expect_silent(h_get_single_visit_estimates(
    emmeans_res = example$emmeans_res,
    conf_level = 0.8
  ))
  expect_snapshot_value(result, tolerance = 1e-3, style = "serialize")
})

# h_get_relative_reduc_df ----

test_that("h_get_relative_reduc_df works as expected", {
  example <- get_lsmeans_example()
  estimates <- h_get_single_visit_estimates(
    emmeans_res = example$emmeans_res,
    conf_level = 0.8
  )
  result <- expect_silent(h_get_relative_reduc_df(
    estimates = estimates,
    vars = example$vars
  ))
  expect_snapshot_value(result, tolerance = 1e-3, style = "serialize")
})

# h_single_visit_contrast_specs ----

test_that("h_single_visit_contrast_specs works as expected", {
  example <- get_lsmeans_example()
  result <- expect_silent(h_single_visit_contrast_specs(
    emmeans_res = example$emmeans_res,
    vars = example$vars
  ))
  expected <- list(
    coefs = list(
      "TRT.VIS1" = c(-1, 0, 0, 0, 1, 0, 0, 0),
      "TRT.VIS2" = c(0, -1, 0, 0, 0, 1, 0, 0),
      "TRT.VIS3" = c(0, 0, -1, 0, 0, 0, 1, 0),
      "TRT.VIS4" = c(0, 0, 0, -1, 0, 0, 0, 1)
    ),
    grid = data.frame(
      ARMCD = rep("TRT", 4),
      AVISIT = c("VIS1", "VIS2", "VIS3", "VIS4")
    )
  )
  expect_identical(result, expected)
})

# h_average_visit_contrast_specs ----

test_that("h_average_visit_contrast_specs works as expected", {
  example <- get_lsmeans_example()
  single_specs <- h_single_visit_contrast_specs(
    emmeans_res = example$emmeans_res,
    vars = example$vars
  )
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  result <- expect_silent(h_average_visit_contrast_specs(
    specs = single_specs,
    averages = averages
  ))
  expected <- list(
    coefs = list(
      "TRT.VIS1+3" = c(-0.5, 0, -0.5, 0, 0.5, 0, 0.5, 0),
      "TRT.VIS2+4" = c(0, -0.5, 0, -0.5, 0, 0.5, 0, 0.5)
    ),
    grid = data.frame(
      ARMCD = c("TRT", "TRT"),
      AVISIT = c("VIS1+3", "VIS2+4")
    )
  )
  expect_identical(result, expected)
})

test_that("h_average_visit_contrast_specs works also for 3 arms and many visits", {
  single_specs <- list(
    coefs = list(
      `ARM B.Week 4` = c(
        -1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 4` = c(
        -1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ),
      `ARM B.Week 8` = c(
        0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 8` = c(
        0, -1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0
      ), `ARM B.Week 12` = c(
        0, 0, -1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 12` = c(
        0,
        0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0
      ), `ARM B.Week 16` = c(
        0, 0, 0, -1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 16` = c(
        0,
        0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0
      ), `ARM B.Week 20` = c(
        0, 0, 0, 0, -1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 20` = c(
        0,
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
        0, 0, 0
      ), `ARM B.Week 24` = c(
        0, 0, 0, 0, 0, -1, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 24` = c(
        0,
        0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 0, 0
      ), `ARM B.Week 28` = c(
        0, 0, 0, 0, 0, 0, -1, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 28` = c(
        0,
        0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0
      ), `ARM B.Week 32` = c(
        0, 0, 0, 0, 0, 0, 0, -1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 32` = c(
        0,
        0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0
      ), `ARM B.Week 36` = c(
        0, 0, 0, 0, 0, 0, 0, 0, -1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 36` = c(
        0,
        0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 0
      ), `ARM B.Week 40` = c(
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 40` = c(
        0,
        0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0
      ), `ARM B.Week 44` = c(
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 44` = c(
        0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
        0, 0, 0
      ), `ARM B.Week 48` = c(
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 48` = c(
        0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        1, 0, 0
      ), `ARM B.Week 52` = c(
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 52` = c(
        0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0
      ), `ARM B.Week 56` = c(
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), `ARM A.Week 56` = c(
        0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1
      )
    ), grid = data.frame(TRT01P = c(
      "ARM B", "ARM A",
      "ARM B", "ARM A", "ARM B", "ARM A", "ARM B", "ARM A", "ARM B",
      "ARM A", "ARM B", "ARM A", "ARM B", "ARM A", "ARM B", "ARM A",
      "ARM B", "ARM A", "ARM B", "ARM A", "ARM B", "ARM A", "ARM B",
      "ARM A", "ARM B", "ARM A", "ARM B", "ARM A"
    ), AVISIT = c(
      "Week 4",
      "Week 4", "Week 8", "Week 8", "Week 12", "Week 12", "Week 16",
      "Week 16", "Week 20", "Week 20", "Week 24", "Week 24", "Week 28",
      "Week 28", "Week 32", "Week 32", "Week 36", "Week 36", "Week 40",
      "Week 40", "Week 44", "Week 44", "Week 48", "Week 48", "Week 52",
      "Week 52", "Week 56", "Week 56"
    ))
  )
  averages <- list("average of Week 40 & 44" = c("Week 40", "Week 44"))
  result <- expect_silent(h_average_visit_contrast_specs(
    specs = single_specs,
    averages = averages
  ))
  expected <- list(
    coefs = list(
      "ARM A.average of Week 40 & 44" = c(
        0, 0, 0,
        0, 0, 0, 0, 0, 0, -0.5, -0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0, 0,
        0
      ),
      "ARM B.average of Week 40 & 44" = c(
        0, 0, 0, 0, 0, 0, 0,
        0, 0, -0.5, -0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      )
    ),
    grid = data.frame(
      TRT01P = c("ARM A", "ARM B"),
      AVISIT = c("average of Week 40 & 44", "average of Week 40 & 44")
    )
  )
  expect_identical(result, expected)
})

# get_mmrm_lsmeans ----

test_that("get_mmrm_lsmeans can calculate the LS mean results", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm_test_data
  )
  conf_level <- 0.95
  weights <- "proportional"
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    averages = averages,
    weights = weights
  ))
  expect_list(result)
  expect_data_frame(result$estimates)
  expect_data_frame(result$contrasts)
  expect_identical(attr(result, "weights"), weights)
  expect_identical(attr(result, "averages"), averages)
})

test_that("get_mmrm_lsmeans preserves combined arm levels.", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm_test_data
  )
  result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = 0.95,
    weights = "proportional"
  )
  expect_identical(
    levels(mmrm_test_data$ARMCD),
    levels(result$estimates$ARMCD)
  )
  expect_identical(
    levels(mmrm_test_data$ARMCD)[-1],
    levels(result$contrasts$ARMCD)
  )
})

test_that("get_mmrm_lsmeans works without arm", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ AVISIT + us(AVISIT | USUBJID),
    data = mmrm_test_data,
    optimizer = "nlminb"
  )
  conf_level <- 0.95
  weights <- "proportional"
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    averages = averages,
    weights = weights
  ))
  expect_list(result)
  expect_data_frame(result$estimates)
  expect_snapshot_value(result$estimates, tolerance = 1e-3, style = "serialize")
  expect_null(result$contrasts)
})
