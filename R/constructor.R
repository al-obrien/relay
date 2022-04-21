#' Create a baton
#'
#' \code{create_baton} is a constructor of an S3 class used for tracking workflows.
#'
#' By default, the content of the \emph{baton} will be empty, only the metadata will be populated. The S3 object created will have an associated
#' YAML file made, in a temporary location if no set location provided to \code{loc}. There is also the ability to automatically assign the S3 object
#' to the R environment in case that is easier to remember.
#'
#' The metadata of a bundle includes the following:
#' \enumerate{
#'    \item id: unique ID of baton based upon time stamp and random numbers.
#'    \item relay_start: time stamp when baton first created (matched ID).
#'    \item relay_finish: time the baton was last passed (will not be populated if in middle of pass).
#'    \item all_grabs: time stamps for all grabs or intercepts that occurred (excludes initial creation time).
#'    \item all_passes: time stamps for all completed passes.
#'    \item pass_complete: whether or not the baton completed its latest pass.
#'    \item passes_completed: the number of successful passes completed.
#'    \item location: where the baton YAML file has been saved.
#'    \item dropped: boolean value of whether the baton has been dropped, signalling and end to the relay.
#' }
#'
#' The logbook operates with \code{\link{write_logbook}} and \code{\link{read_logbook}}, which help track the baton without having to write contents or update the metadata.
#'
#' @note Although some generics (\code{\link{summary.baton}}) are used in \{relay\}, most functions just check for the class and do not proceed unless
#' it is a \emph{baton}. This may be rewritten to use only generics or R6 classes but right now this hybrid approach was used for simplicity.
#'
#' @param content A list of custom content that the baton should carry.
#' @param loc The location of the associated YAML file (defaults to a temporary location).
#' @param auto_assign Boolean value to determine if name assignment is to be automatic).
#' @param envir Environment where \code{auto_assign} should write to.
#' @param bundled Boolean value to determine if the baton should be created within a bundle; default is set to \code{FALSE}.
#' @param bundle_params List of parameters to pass to \code{\link{create_bundle}}.
#'
#' @return S3 class object.
#' @export
#' @examples
#' \dontrun{
#' my_baton <- create_baton(loc = file.path('path', 'to', 'save', 'yaml'))
#' }
create_baton <- function(content = list(), loc = NULL, auto_assign = FALSE, envir = .GlobalEnv, bundled = FALSE, bundle_params = list()) {

  if(!is.list(content)) stop('`content` parameter must be a list.')

  # Metadata
  start <- Sys.time()
  id <- paste0(c(format(start, '%Y%m%d%H%M%S'), '-',
                 sample(c(LETTERS, letters), replace = T, size = 4),
                 sample(c(0:9), 4, replace = T)),
               collapse = '')

  # _baton file location
  loc <- if(is.null(loc)) tempdir() else normalizePath(loc, mustWork = TRUE);
  loc <- file.path(loc, paste0('_baton-', id, '.yml'))

  baton <- structure(list(metadata = list(id = id,
                                          relay_start = format(start, '%Y-%m-%d %H-%M-%S'),
                                          relay_finish = NA,
                                          all_grabs = NULL,
                                          all_passes = NULL,
                                          pass_complete = FALSE,
                                          passes_completed = 0,
                                          location = loc,
                                          dropped = FALSE),
                          logbook = NULL,
                          content = content),
                     class = "baton")

  # Perform validation
  validate_baton(baton)

  # Create YAML for tracking
  convert_baton2yml(baton = baton, write = TRUE)
  message('Baton created... associated file created at: ', baton$metadata$loc)

  if(bundled) {
    if(!all(is.list(bundle_params), names(bundle_params) %in% c('dir', 'tree', 'tag', 'mode', '...'))) stop('Invalid bundle parameter provided. Give as a list.')
    bundle_path <- do.call(create_bundle, args = c(baton = list(baton), bundle_params))
    baton <- relocate_baton(baton, loc = bundle_path, silent = TRUE)
  }

  if(auto_assign) {
    assign(x = paste0('_baton-', baton$metadata$id), value = baton, envir = envir)
  } else {
    return(baton)
  }
}

#' Create a bundle
#'
#' \code{create_bundle} is a helper function to create a set of skeleton folders under a main bundle directory.
#'
#' Creating a bundle can be done at the same time as \code{\link{create_baton}}. However, it can be used without a baton as well
#' to easily make a set of empty folders to populate later. When using as part of a \{relay\}, the primary purpose is to store information that
#' passes checks from batons.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
#' @param dir character value for loaction to create the bundle on the file system.
#' @param tree character vector for sub directories; default has no sub-directories set.
#' @param tag character value for prefixed label to the bundled folder.
#' @param mode passed to \code{\link{Sys.chmod}}.
#' @param ... passed to \code{\link{dir.create}}.
#'
#' @return Character vector for the location of created bundle.
#' @export
#' @examples
#' \dontrun{
#' create_bundle(my_baton, dir = '/path/to/relay/raw_bundle',
#'               tree = c('raw', 'processed', 'metadata', 'output', '/raw/sub-raw'))
#' }
create_bundle <- function(baton, dir, tree, tag = '_bundle-', mode, ...){

  validate_baton(baton)

  dir <- normalizePath(dir)
  baton_id <- baton$metadata$id
  bundle_path <- file.path(dir, paste0(tag, baton$metadata$id))

  # Create the folder skeleton
  dir.create(bundle_path, ...)
  purrr::walk(tree, ~dir.create(file.path(bundle_path, .)))

  if(!missing(mode)){
    purrr::walk(tree, ~Sys.chmod(file.path(bundle_path, .), mode = mode, use_umask = FALSE));
    Sys.chmod(bundle_path, mode = mode, use_umask = FALSE);
  }

  return(bundle_path)

}
