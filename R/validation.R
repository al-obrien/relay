#' Validate baton
#'
#' \code{validate_baton} is an internal function to \{relay\}. Determines if the \emph{baton} has the correct metadata compliant to being a true baton.
#'
#' Basic validation checks used in constructing batons
validate_baton <- function(baton) {
  if(!inherits(baton, 'baton')) stop('Class is not of type "baton"')

  if(!setequal(c('id', 'referee', 'relay_type', 'relay_start', 'relay_finish', 'all_grabs', 'all_passes', 'pass_complete', 'passes_completed', 'location', 'dropped'), names(baton$metadata))) {
    stop('Metadata is missing from baton; ensure a baton is passed to this function.')
  }
}
