% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subgroups.R
\name{extract_mmrm_subgroups}
\alias{extract_mmrm_subgroups}
\title{Extraction of MMRM Subgroup Results based on Population Model Definition}
\usage{
extract_mmrm_subgroups(
  fit,
  visit,
  subgroups = NULL,
  groups_lists = list(),
  treatment_arm = fit$treatment_levels[1L],
  label_all = "All Patients"
)
}
\arguments{
\item{fit}{(\code{tern_mmrm})\cr model fit on the total population.}

\item{visit}{(\code{string})\cr single visit or name of averages of visits (referring
to the averages specified when creating the \code{fit}).}

\item{subgroups}{(\code{character} or \code{NULL})\cr names of subgroup variables to use in the
forest plot, these need to be factors.}

\item{groups_lists}{(named \code{list} of \code{list})\cr optionally contains for each
\code{subgroups} variable a list, which specifies groups of factor levels, see
details.}

\item{treatment_arm}{(\code{string})\cr single treatment arm to compare with the reference
arm.}

\item{label_all}{(\code{string})\cr label for the total population analysis.}
}
\value{
A list with two elements:
\itemize{
\item \code{estimates}: \code{data.frame} with columns \code{arm}, \code{n}, \code{lsmean}, \code{subgroup},
\code{var}, \code{var_label}, \code{row_type}, containing the LS means results for
the overall population and the specified subgroups.
\item \code{contrasts}: \code{data.frame} with columns \code{n_tot}, \code{diff}, \code{lcl}, \code{ucl},
\code{pval}, \code{subgroup}, \code{var}, \code{var_label}, \code{row_type}. Note
that this has half the number of rows as \code{estimates}.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This prepares LS mean estimates and contrasts for a specific visit and treatment
arm relative to the reference arm, along with a list of subgroup variables and corresponding
(grouped) factor levels.
}
\details{
The \code{groups_lists} argument is handy when you don't want to have
subgroups identical to the original levels of the factor variable. This might
be the case when you want to merge levels into a single subgroup, define
overlapping subgroups or omit levels completely. Then you insert an element into
\code{groups_lists} with the name of the \code{subgroups} variable and containing
as a named list the subgroup definitions. See the example below.
}
\note{
If the original model \code{vars} include \code{covariates} which are used here in
\code{subgroups} then these are dropped from \code{covariates} before the corresponding
model is fitted.
}
\examples{
mmrm_results <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = "RACE",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "compound symmetry",
  weights_emmeans = "equal",
  averages_emmeans = list(
    "VIS1+2" = c("VIS1", "VIS2")
  )
)

extract_mmrm_subgroups(
  fit = mmrm_results,
  visit = "VIS3",
  subgroups = c("RACE", "SEX"),
  groups_lists = list(
    RACE = list(
      A = c("Asian", "White"),
      B = c("Black or African American", "White")
    )
  )
)
}
