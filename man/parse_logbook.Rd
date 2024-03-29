% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parse_logbook}
\alias{parse_logbook}
\title{Parse the baton's logbook for details of interest}
\usage{
parse_logbook(
  baton_logbook,
  target = c("PASS", "PASS_NUMBER", "DATE", "TIME", "DATETIME", "MESSAGE_TYPE",
    "MESSAGE")
)
}
\arguments{
\item{baton_logbook}{A baton or a vector containing a baton's logbook details.}

\item{target}{Character vector for parsing target in logbook (one of: 'PASS', 'PASS_NUMBER', 'DATETIME', 'DATE' 'TIME', , 'MESSAGE_TYPE', 'MESSAGE').}
}
\description{
Will parse based on various REGEX capturing groups:
}
\details{
\itemize{
  \item{"Group 1"}{entire content}
  \item{"Group 2"}{pass number}
  \item{"Group 3"}{DateTime}
  \item{"Group 4"}{Date}
  \item{"Group 5"}{Time}
  \item{"Group 6"}{message type}
  \item{"Group 7"}{message content}
}
}
