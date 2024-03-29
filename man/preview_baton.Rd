% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructor.R
\name{preview_baton}
\alias{preview_baton}
\title{Preview a baton from a YAML file}
\usage{
preview_baton(loc)
}
\arguments{
\item{loc}{Location of YAML file that was saved from a \emph{baton}.}
}
\value{
S3 class object.
}
\description{
\code{preview_baton} accesses the baton YAML file for previewing.
}
\details{
A previewed version of a baton is restricted and is mainly used for read-only operations such as
\code{\link{summary}} and \code{\link{plot}}.
}
\examples{
\dontrun{
preview_baton(loc = '/path/to/relay/baton.yml')
}
}
