## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
output <- data.frame(
  A1 = c("100 (20)", "50 (30)"),
  B1 = c("90 (35)", "40 (15)"),
  "..." = c("", "")
)
row.names(output) <- c("M", "F")
kable(output, align = "c")

## -----------------------------------------------------------------------------
library(tern.mmrm)
data(mmrm_test_data)
head(mmrm_test_data)

## ----message=FALSE, results="hide"--------------------------------------------
mmrm_results <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX", "FEV1_BL"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "proportional"
)

## -----------------------------------------------------------------------------
mmrm_results$lsmeans$contrasts

## ----eval=TRUE----------------------------------------------------------------
library(mmrm)
library(emmeans)

fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = mmrm_test_data
)
emmeans::emmeans(fit, pairwise ~ ARMCD | AVISIT, weights = "proportional")

## ----eval=TRUE----------------------------------------------------------------
mmrm_results$diagnostics

