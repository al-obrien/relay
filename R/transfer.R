#' Pass a baton
#'
#' Baton pass operation (write to contents, update metadata, append to YAML)
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}
#' @param content a list of custom content that the baton should carry. Populates the YAML.
#' @param relocate character vector defining the file path to move the YAML file upon completion of the pass.
#'
#' @return S3 class object.
#' @export
#' @examples
#' \dontrun{
#' # ----------------------------------------- #
#' # The following code provides an example of how
#' # to create a baton, populate its content, and
#' # complete the pass.
#' # ----------------------------------------- #
#'
#' # Define location where raw data lives (to be moved to bundle for snapshot)
#' raw_loc <- '/path/to/relay/raw'
#'
#' # Create baton, within a bundle
#' baton_raw <- create_baton(bundled = TRUE,
#'                           bundle_params = list(dir = '/path/to/relay/raw_bundle',
#'                           tree = c('raw', 'processed', 'metadata', 'output')))
#'
#' # Location of bundle
#' bundle_raw_loc <- dirname(baton_raw$metadata$location)
#'
#'
#' # Data expected to be used in bundle
#' data_manifest <- c("cdom_data_raw.Rds", "cases_imm.sas7bdat", "covid_comorbidity_lookup.sas7bdat",
#'
#' # Move data to bundle
#' copy_files(raw_loc, data_manifest, file.path(bundle_raw_loc, 'raw'))
#'
#' # Define content of baton to save to YAML in addition to metadata (nested lists)
#' existence <- map_lgl(data_manifest, ~file.exists(paste0(bundle_raw_loc, '/raw/', .))
#' creation_times <- map_chr(data_manifest, ~as.character(file.info(paste0(bundle_raw_loc, '/raw/', .))$mtime))
#' baton_content <- list(datasets = list(names = data_manifest,
#'                                       existence = existence,
#'                                       creation_times = creation_times))
#'
#' # Pass baton for end of process (next step in the relay in another script can see if this baton was passed and, if so, the content it has)
#' baton_raw <- relay::pass_baton(baton_raw, content = baton_content)
#' }
pass_baton <- function(baton, content = list(), relocate) {

  # Check if pass complete
  if(baton$metadata$pass_complete) stop('Baton pass completed, iterate on top of this relay by starting with `grab_baton()` again.')
  if(!is.list(content)) stop('`content` parameter must be a list.')
  if(length(locate_batons(suppress_messages = TRUE)>1)) warning('Multiple baton objects in environment, be careful of the order they are passed.\n')

  # Validate
  validate_baton(baton)

  # Update metadata
  finish <- Sys.time()
  baton$metadata$relay_finish <- format(finish, '%Y-%m-%d %H-%M-%S')
  baton$metadata$pass_complete <- TRUE
  baton$metadata$passes_completed <- baton$metadata$passes_completed + 1
  baton$metadata$all_passes <- c(baton$metadata$all_passes, baton$metadata$relay_finish)

  # If want to relocate, change that metadata here, save old to remove if wanted...
  if(!missing(relocate)) {
    baton <- relocate_baton(baton, loc = relocate)
  }

  # Update contents
  baton$content <- utils::modifyList(baton$content, content)

  # Update YAML
  convert_baton2yml(baton, write = TRUE)

  return(baton)
}

#' Grab a baton
#'
#' Attempt to start a new pass by grabbing a \emph{baton} object.
#'
#' By default, \code{grab_baton} will use the parameter for a \emph{baton} that exists in the environment.
#' If a location of a YAML file is provided via \code{loc}, then an attempt is made to load a \emph{baton} from disk.
#' The \emph{baton} will only load if it had been successfully passed; if so, the \emph{baton} metadata will be updated in
#' preparation for a new pass to be performed later in the workflow. The original YAML file is overwritten to update two
#' metedata parameters: \code{pass_complete} and \code{relay_finish}.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}
#' @param loc Location of YAML file that was saved from a \emph{baton}.
#'
#' @return S3 class object.
#' @export
#' @examples
#' \dontrun{
#' # Make a baton
#' baton_raw <- create_baton(bundled = TRUE,
#'                           bundle_params = list(dir = '/path/to/relay/raw_bundle',
#'                           tree = c('raw', 'processed', 'metadata', 'output')))
#'
#' # Pass an empty baton for illustrative purposes
#' baton_raw <- relay::pass_baton(baton_raw)
#'
#' # Grab the baton (if later in the script)
#' baton_raw <- relay::grab_baton(baton_raw)
#'
#' # Grab the baton (if in an entirely separate script/project)
#' baton_raw <- relay::grab_baton(loc = '/path/to/original/baton')
#' }
grab_baton <- function(baton, loc = NULL) {

  if(!is.null(loc)) {
    message('Attempting to load baton from YAML...')
    baton <- convert_yml2baton(loc)
  }

  if(length(locate_batons(suppress_messages = TRUE)>1)) warning('Multiple baton objects in environment, be careful of the order they are passed.\n')

  # Validate
  validate_baton(baton)

  # Check if pass complete
  if(baton$metadata$dropped) stop('This baton has been dropped and cannot be grabbed. It is locked from further relays.')
  if(!baton$metadata$pass_complete) stop('Baton pass incomplete, cannot grab. Try running `pass_baton()`\n')

  # Update metadata
  baton$metadata$relay_finish <- NA
  baton$metadata$pass_complete <- FALSE

  # Update YAML
  convert_baton2yml(baton, write = TRUE)

  return(baton)

}

#' Drop a baton
#'
#' Drop a baton to prevent it from being passed/grabbed.
#'
#' Performed to lock the \emph{baton} from any further use of \code{\link{pass_baton}} or \code{\link{grab_baton}}. If performed in
#' error, can reset by manually editing the YAML file, assuming one has permissions to tinker with the files created!
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}
#'
#' @return S3 class object.
#' @export
drop_baton <- function(baton) {

  # Validate
  validate_baton(baton)

  # Update metadata
  baton$metadata$dropped <- TRUE

  # Update YAML
  convert_baton2yml(baton, write = TRUE)

  return(baton)
}

#' Relocate a baton
#'
#' Internal function used to relocate a baton to a new location.
#'
#' @return S3 class object.
relocate_baton <- function(baton, loc, silent = FALSE) {

  # Validate
  #validate_baton(baton)

  if(!silent) warning('Relocating the baton, prior will be removed to relocated path')

  loc <- file.path(normalizePath(loc, mustWork = TRUE), paste0('_baton-', baton$metadata$id, '.yml'))

  # Could use convert_baton2yml, but easier for error catching if doing this method...
  file.copy(baton$metadata$location, loc)
  message('Baton YAML moved to ', loc)

  if(file.exists(loc)) file.remove(baton$metadata$location) else warning('Did not remove original file as new locaiton was not copied to.')

  baton$metadata$location <- loc

  # Update YAML with new location
  convert_baton2yml(baton, write = TRUE)

  return(baton)
}

#' Intercept a baton
#'
#' Functionally similar to \code{\link{grab_baton}} but able to obtain \emph{baton} before the pass operation is complete.
#'
#' Normally, \code{\link{grab_baton}} is the preferred method to load a \emph{baton}. However, there are circumstances where a pass
#' needs to be \emph{intercepted} mid-pass so that a process can reset and continue. It is recommended to use \code{intercept_baton} in
#' limited situations. To avoid loading a baton twice, \code{intercept_baton} will attempt to delete any \emph{baton} in the global environment
#' that shares the same ID; this is a measure to ensure a baton is only being acted on linearly. A warning is also provided to remind users that
#' misuse of this function could lead to unexpected results in the YAML file if a pass that was expected to be completed was intercepted and acted
#' upon differently.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}
#' @param loc Location of YAML file that was saved from a \emph{baton}.
#' @param reset_only boolean value which will attempt only to reset the \code{pass_complete} status without loading the \emph{baton}.
#' @param env R environment to locate and delete duplicate batons during an interception; defaults to \code{.GlobalEnv}.
#'
#' @return S3 class object.
#' @export
#' @examples
#' \dontrun{
#' # Make a baton
#' baton_raw <- create_baton(bundled = TRUE,
#'                           bundle_params = list(dir = '/path/to/relay/raw_bundle',
#'                           tree = c('raw', 'processed', 'metadata', 'output')))
#'
#' # Pass an empty baton for illustrative purposes
#' baton_raw <- relay::pass_baton(baton_raw)
#'
#' # Grab the baton (if later in the script)
#' baton_raw <- relay::intercept_baton(baton_raw)
#'
#' # Grab the baton (if in an entirely separate script/project)
#' baton_raw <- relay::intercept_baton(loc = '/path/to/original/baton')
#' }
intercept_baton <- function(baton, loc = NULL, reset_only = FALSE, env = .GlobalEnv) {

  if(!is.null(loc)) {
    message('Attempting to load baton from YAML...')
    baton <- convert_yml2baton(loc)
  }

  if(length(locate_batons(suppress_messages = TRUE)>1)) warning('Multiple baton objects in environment, be careful of the order they are passed.\n')

  # Validate
  validate_baton(baton)

  # Check if pass complete
  if(baton$metadata$dropped) stop('This baton has been dropped and cannot be grabbed. It is locked from further relays.')
  if(!baton$metadata$pass_complete) {
    warning('Baton pass was incomplete, but has been intercepted.\n')
  } else { warning('Baton pass was complete; preferable to use `grab_baton()`')}

  # Update metadata, YAML and return
  if(reset_only){
    baton$metadata$pass_complete <- TRUE
    convert_baton2yml(baton, write = TRUE)
    message('Associated YAML file had pass status reset')
  } else {
    baton$metadata$relay_finish <- NA
    baton$metadata$pass_complete <- FALSE
    convert_baton2yml(baton, write = TRUE)

    # Attempt cleanup
    tryCatch({
      locate_existing <- suppressMessages(locate_batons(loc = env, suppress_messages = TRUE))
      existing_names <- purrr::map_chr(locate_existing, ~get(.)$metadata$id)
      to_rm <- locate_existing[baton$metadata$id %in% existing_names]
      warning('Attempting to remove the following baton(s): ', paste(to_rm, collapse = ' '))
      rm(list = to_rm, envir = env)
    },
    error = function(err) {
      message('An error was received when trying to clear existing batons with the same id as the one intercepted.')
      warning(err)
    })

    return(baton)
  }
}
