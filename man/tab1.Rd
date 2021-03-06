% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab1.R
\name{tab1}
\alias{tab1}
\title{Table 1}
\usage{
tab1(
  contvars,
  catvars,
  byvar,
  dat,
  col = TRUE,
  spread = "range",
  pval = TRUE,
  fisher = TRUE
)
}
\arguments{
\item{contvars}{is a list of the continuous variables you want in the rows
e.g. list('Age'). Can be NULL.}

\item{catvars}{is a list of the categorical variables you want in the rows
e.g. list('Gender','Race'). Can be NULL.}

\item{byvar}{is the categorical variable you want to tabulate by across the
columns (needs to be in quotes). Can be NULL.}

\item{dat}{is the dataset to use for analysis}

\item{col}{takes the value TRUE or FALSE indicating whether you want column
percent (TRUE, default) or row percent (FALSE)}

\item{spread}{takes the value "range" or "iqr" indicating whether you want
(min, max) or (Q1, Q3) in summaries of continuous variables. Defaults to
"range".}

\item{pval}{takes the value TRUE or FALSE indicating whether p-values should
be included. Defaults to TRUE. If TRUE, \code{\link[stats]{kruskal.test}} p-values are
produced for continuous variables and either \code{\link[stats]{fisher.test}} or
\code{\link[stats]{chisq.test}} p-values are produced for categorical variables.}

\item{fisher}{takes the value TRUE or FALSE. If TRUE, \code{\link[stats]{fisher.test}}
p-values are produced. If FALSE, \code{\link[stats]{chisq.test}} p-values are produced.}
}
\value{
Returns a dataframe. If there are warnings or errors from
\code{\link[stats]{kruskal.test}}, \code{\link[stats]{fisher.test}}, or \code{\link[stats]{chisq.test}} then NA is
returned in place of the p-value.
}
\description{
\code{tab1} takes lists of continuous and/or categorical variables and
returns Median (spread) for continuous variables and N (\%) for categorical
variables. Produces a table with both an overall column and columns by
another variable.
}
\author{
Emily C Zabor \email{zabore@mskcc.org}
}
