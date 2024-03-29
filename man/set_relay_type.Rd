% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transfer.R
\name{set_relay_type}
\alias{set_relay_type}
\title{Set a baton's relay type}
\usage{
set_relay_type(
  baton,
  threshold = getOption("relay_type", default = "COMPETITION"),
  suppressWarnings = TRUE,
  autoassign = TRUE,
  envir = .GlobalEnv,
  ...
)
}
\arguments{
\item{baton}{R object of S3 class, created by \code{\link{create_baton}}.}

\item{threshold}{character value for the type of the baton (e.g. 'CANCELLED', 'PRACTICE', or 'COMPETITION').}

\item{suppressWarnings}{boolean value to determine if warning messages upon YAML write are ignored.}

\item{autoassign}{boolean value to determine if the passed baton is also refreshed automatically. Recommended as TRUE to avoid having to do manual assignment to provided baton.}

\item{envir}{Environment where baton exists, default set to .GlobalEnv, only needed when autoassign is TRUE. If deploying on RStudio Connect, may require using \code{knitr::knit_global()}.}

\item{...}{Additional parameters passed to \code{assign}}
}
\value{
S3 class object.
}
\description{
\code{set_relay_type} is a helper function to set the 'relay_type' of the baton; this flag can be accessed by the user to help control how batons are accessed.
The updated metadata will occur both in the source YAML as well as the R object for paired consistency. There are three types of relay: 'CANCELLED', 'PRACTICE',
or 'COMPETITION'. The default is 'COMPETITION'. This can be adjusted globally by \code{options(relay_type = "COMPETITION")}
}
\examples{
\dontrun{
my_baton <- create_baton()

set_relay_type(my_baton, 'CANCELLED')
read_metadata(my_baton)$relay_type

set_referee(my_baton, 'COMPETITION')
read_metadata(my_baton)$relay_type
}
}
