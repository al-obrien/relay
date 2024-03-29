% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transfer.R
\name{write_logbook}
\alias{write_logbook}
\title{Write to baton's logbook}
\usage{
write_logbook(
  baton,
  msg,
  msg_type = "MESSAGE",
  trunc_long = TRUE,
  suppressWarnings = TRUE,
  autoassign = TRUE,
  envir = .GlobalEnv,
  ...
)
}
\arguments{
\item{baton}{R object of S3 class, created by \code{\link{create_baton}}.}

\item{msg}{Location of YAML file that was saved from a \emph{baton}.}

\item{msg_type}{boolean value which will attempt only to reset the \code{pass_complete} status without loading the \emph{baton}.}

\item{trunc_long}{boolean value to determine if messages should be truncated at 256 characters.}

\item{suppressWarnings}{boolean value to determine if warning messages upon YAML write are ignored.}

\item{autoassign}{boolean value to determine if the passed baton is also refreshed automatically. Recommended as TRUE to ensure logging doesn't have to read and write each time an entry is made.}

\item{envir}{Environment where baton exists, default set to .GlobalEnv, only needed when autoassign is TRUE. If deploying on RStudio connect, may require \code{knitr::knit_global()}.}

\item{...}{Additional parameters passed to \code{assign}}
}
\value{
S3 class object.
}
\description{
Write log information to a baton's logbook without affecting the metadata or contents. This can be considered the microscopic tracking whereas
the metadata and contents are macroscopic. It can be helpful when an error occurs between different passes and you want these details within the baton log.
The baton must be loaded into the R environment in order to perform logging. If you do not use the \code{autoassign} parameter and forget to assign the baton
to itself, one risks losing previous logs.
}
\details{
The baton logbook provides easy access to linear streams of logging. Although possible, logging support for parallel processing
has a few risks. Future version of relay may attempt to extend or simplify the parallel logging support. There is a possibility of read/write
multiple access issues as well. There are two main options when using \code{parallel::parLapply}. First, when making the
SOCKET cluster, ensure that the outputs are being logged to a temporary file:
\preformatted{
cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = 'tmpoutloc.txt')
}
When exiting the process, ensure that the temporary log is then captured as an entry in the baton's log using \code{write_logbook}.
Second, if you want to use {{relay}} more directly, ensure that the cluster has relay and the baton accessible on every node using:
\preformatted{
parallel::clusterEvalQ(cl, library(relay));
parallel::clusterExport(cl, varlist = list("batonname"), envir = environment())
}
If the parallel operations are within a function, ensure \code{envir = environment())} instead of \code{.GlobalEnv}. The \code{parallel::parLapply()} function
should contain code similar to the following to work:
\preformatted{
batonname$logbook <- relay::read_logbook(loc = batonname$metadata$location);
relay::write_logbook(batonname, 'LOGGING MESSAGE', msg_type = 'MESSAGE', autoassign = FALSE);
}
Which ensures the latest logbook is available before writing and to update the YAML. When the parallel steps are finished the baton in the global environment
will need to be refreshed as well to avoid the prior entries being lost. This is easily done within a function as well using \code{\link{on.exit}}:
\preformatted{
# Ensure name of baton provided as a parameter caught early in the function
btn_name <- deparse(substitute(batonname));

# Ensure operations run on function exit...
on.exit({
 batonname$logbook <- relay::read_logbook(loc = batonname$metadata$location);

 relay::write_logbook(batonname, 'EXIT MESSAGE',
                      msg_type = 'MESSAGE', autoassign = TRUE, envir = environment())

 # Push out of function
 assign(btn_name, batonname, envir = knitr::knit_global())
})
}
}
\seealso{
\code{\link{read_logbook}}
}
