% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulate_mmrm.R
\name{tabulate_mmrm}
\alias{tabulate_mmrm}
\alias{as.rtable.tern_mmrm}
\alias{h_mmrm_fixed}
\alias{h_mmrm_cov}
\alias{h_mmrm_diagnostic}
\alias{tidy.tern_mmrm}
\alias{s_mmrm_lsmeans}
\alias{a_mmrm_lsmeans}
\alias{s_mmrm_lsmeans_single}
\alias{a_mmrm_lsmeans_single}
\alias{summarize_lsmeans}
\title{Tabulation of \code{MMRM} Results}
\usage{
\method{as.rtable}{tern_mmrm}(x, type = c("fixed", "cov", "diagnostic"), ...)

h_mmrm_fixed(x, format = "xx.xxxx")

h_mmrm_cov(x, format = "xx.xxxx")

h_mmrm_diagnostic(x, format = "xx.xxxx")

\method{tidy}{tern_mmrm}(x, ...)

s_mmrm_lsmeans(df, .in_ref_col, show_relative = c("reduction", "increase"))

a_mmrm_lsmeans(df, .in_ref_col, show_relative = c("reduction", "increase"))

s_mmrm_lsmeans_single(df)

a_mmrm_lsmeans_single(df)

summarize_lsmeans(
  lyt,
  arms = TRUE,
  ...,
  table_names = "lsmeans_summary",
  .stats = NULL,
  .formats = NULL,
  .indent_mods = NULL,
  .labels = NULL
)
}
\arguments{
\item{x}{(\code{tern_mmrm})\cr the original result from \code{\link[=fit_mmrm]{fit_mmrm()}}.}

\item{type}{(\code{string})\cr type of table which should be returned.}

\item{...}{additional argument \code{format} for controlling the numeric format.}

\item{format}{(\code{string})\cr format for the numbers in the table.}

\item{df}{(\verb{data frame})\cr data set containing all analysis variables.}

\item{.in_ref_col}{(\code{logical})\cr \code{TRUE} when working with the reference level, \code{FALSE} otherwise.}

\item{show_relative}{should the "reduction" (\code{control - treatment}, default) or the "increase"
(\code{treatment - control}) be shown for the relative change from baseline?}

\item{lyt}{(\code{layout})\cr input layout where analyses will be added to.}

\item{arms}{(\code{flag})\cr  should treatment variable be considered when using
\code{summarize_lsmeans} layout generating function.}

\item{table_names}{(\code{character})\cr this can be customized in case that the same \code{vars} are analyzed multiple times,
to avoid warnings from \code{rtables}.}

\item{.stats}{(\code{character})\cr statistics to select for the table.}

\item{.formats}{(named \code{character} or \code{list})\cr formats for the statistics.}

\item{.indent_mods}{(named \code{integer})\cr indent modifiers for the labels.}

\item{.labels}{(named \code{character})\cr labels for the statistics (without indent).}
}
\value{
\code{\link[=as.rtable.tern_mmrm]{as.rtable.tern_mmrm()}} returns the fixed effects, covariance estimate or
diagnostic statistics tables.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

These functions can be used to produce tables from a fitted \code{MMRM} produced with
\code{\link[=fit_mmrm]{fit_mmrm()}}.
}
\section{Functions}{
\itemize{
\item \code{as.rtable(tern_mmrm)}: Produce simple \code{MMRM} tables via the generic \code{\link[=as.rtable]{as.rtable()}}.

\item \code{h_mmrm_fixed()}: Helper function to produce fixed effects table.

\item \code{h_mmrm_cov()}: Helper function to produce a covariance matrix table.

\item \code{h_mmrm_diagnostic()}: Helper function to produce a diagnostic statistics table.

\item \code{tidy(tern_mmrm)}: Helper method (for \code{\link[broom:reexports]{broom::tidy()}}) to prepare a \code{data.frame} from an
\code{tern_mmrm} object containing the least-squares means and contrasts.

\item \code{s_mmrm_lsmeans()}: Statistics function which is extracting estimates from a tidied least-squares means
data frame.

\item \code{a_mmrm_lsmeans()}: Formatted Analysis function which can be further customized by calling
\code{\link[rtables:make_afun]{rtables::make_afun()}} on it. It is used as \code{afun} in \code{\link[rtables:analyze]{rtables::analyze()}}.

\item \code{s_mmrm_lsmeans_single()}: Statistics function which is extracting estimates from a tidied least-squares means
data frame when \code{ARM} is not considered in the model.

\item \code{a_mmrm_lsmeans_single()}: Formatted Analysis function (when \code{ARM} is not considered in the model)
which can be further customized by calling
\code{\link[rtables:make_afun]{rtables::make_afun()}} on it. It is used as \code{afun} in \code{\link[rtables:analyze]{rtables::analyze()}}.

\item \code{summarize_lsmeans()}: Analyze function for tabulating least-squares means estimates from tidied \code{mmrm} results.

}}
\examples{
result <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "equal"
)
as.rtable(result, type = "cov", format = "xx.x")

result_no_arm <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "equal"
)
as.rtable(result_no_arm, type = "cov", format = "xx.x")
df <- broom::tidy(result)
df_no_arm <- broom::tidy(result_no_arm)
s_mmrm_lsmeans(df[8, ], .in_ref_col = FALSE)
s_mmrm_lsmeans_single(df_no_arm[4, ])
library(dplyr)

dat_adsl <- mmrm_test_data \%>\%
  select(USUBJID, ARMCD) \%>\%
  unique()
basic_table() \%>\%
  split_cols_by("ARMCD", ref_group = result$ref_level) \%>\%
  add_colcounts() \%>\%
  split_rows_by("AVISIT") \%>\%
  summarize_lsmeans(
    .stats = c("n", "adj_mean_se", "adj_mean_ci", "diff_mean_se", "diff_mean_ci"),
    .labels = c(adj_mean_se = "Adj. LS Mean (Std. Error)"),
    .formats = c(adj_mean_se = sprintf_format("\%.1f (\%.2f)"))
  ) \%>\%
  build_table(
    df = broom::tidy(result),
    alt_counts_df = dat_adsl
  )

basic_table() \%>\%
  split_rows_by("AVISIT") \%>\%
  summarize_lsmeans(arms = FALSE) \%>\%
  build_table(
    df = broom::tidy(result_no_arm),
    alt_counts_df = dat_adsl
  )
}
