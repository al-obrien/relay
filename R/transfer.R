#' Pass a baton
#'
#' Baton pass operation (write to contents, update metadata, append to YAML)
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
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
  if(length(locate_batons(suppress_messages = TRUE))>1) warning('Multiple baton objects in environment, be careful of the order they are passed.\n')

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

  if(length(locate_batons(suppress_messages = TRUE))>1) warning('Multiple baton objects in environment, be careful of the order they are passed.\n')

  # Validate
  validate_baton(baton)

  # Check if pass complete
  if(baton$metadata$dropped) stop('This baton has been dropped and cannot be grabbed. It is locked from further relays.')
  if(!baton$metadata$pass_complete) stop('Baton pass incomplete, cannot grab. Try running `pass_baton()`\n')

  # Update metadata
  baton$metadata$relay_finish <- NA
  baton$metadata$pass_complete <- FALSE
  baton$metadata$all_grabs <- c(baton$metadata$all_grabs, format(Sys.time(), '%Y-%m-%d %H-%M-%S'))

  # Check if relocation needed
  if(!is.null(loc) && normalizePath(baton$metadata$location, mustWork = FALSE) != normalizePath(loc, mustWork = FALSE)) {
    warning('Updating baton YAML location information based upon load location provided.')
    baton$metadata$location <- loc
    #baton <- relocate_baton(baton, loc = loc)
  }

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

  # Was initially making work with grab_baton, but was unnecessary...
  # cond <- grepl(x = loc, '\\.[yY][mM][lL]$')
  # if(!cond) {
    loc <- file.path(normalizePath(loc, mustWork = TRUE), paste0('_baton-', baton$metadata$id, '.yml'))
  # } else if (cond) {
  #   loc <- loc
  # }

  # Could use convert_baton2yml, but easier for error catching if doing this method...
  file.copy(baton$metadata$location, loc)
  message('Baton YAML moved to ', loc)

  if(file.exists(loc)) file.remove(baton$metadata$location) else warning('Did not remove original file as new location was not copied to.')

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
    baton$metadata$all_grabs <- c(baton$metadata$all_grabs, format(Sys.time(), '%Y-%m-%d %H-%M-%S'))
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

#' Write to baton's logbook
#'
#' Write log information to a baton's logbook without affecting the metadata or contents. This can be considered the microscopic tracking whereas
#' the metadata and contents are macroscopic. It can be helpful when an error occurs between different passes and you want these details within the baton log.
#' The baton must be loaded into the R environment in order to perform logging. If you do not use the \code{autoassign} parameter and forget to assign the baton
#' to itself, one risks losing previous logs.
#'
#' The baton logbook provides easy access to linear streams of logging. Although possible, logging support for parallel processing
#' has a few risks. Future version of relay may attempt to extend or simplify the parallel logging support. There is a possibility of read/write
#' multiple access issues as well. There are two main options when using \code{parallel::parLapply}. First, when making the
#' SOCKET cluster, ensure that the outputs are being logged to a temporary file:
#' \preformatted{
#' cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = 'tmpoutloc.txt')
#' }
#' When exiting the process, ensure that the temporary log is then captured as an entry in the baton's log using \code{write_logbook}.
#' Second, if you want to use {{relay}} more directly, ensure that the cluster has relay and the baton accessible on every node using:
#' \preformatted{
#' parallel::clusterEvalQ(cl, library(relay));
#' parallel::clusterExport(cl, varlist = list("batonname"), envir = environment())
#' }
#' If the parallel operations are within a function, ensure \code{envir = environment())} instead of \code{.GlobalEnv}. The \code{parallel::parLapply()} function
#' should contain code similar to the following to work:
#' \preformatted{
#' batonname$logbook <- relay::read_logbook(loc = batonname$metadata$location);
#' relay::write_logbook(batonname, 'LOGGING MESSAGE', msg_type = 'MESSAGE', autoassign = FALSE);
#' }
#' Which ensures the latest logbook is available before writing and to update the YAML. When the parallel steps are finished the baton in the global environment
#' will need to be refreshed as well to avoid the prior entries being lost. This is easily done within a function as well using \code{\link{on.exit}}:
#' \preformatted{
#' # Ensure name of baton provided as a parameter caught early in the function
#' btn_name <- deparse(substitute(batonname));
#'
#' # Ensure operations run on function exit...
#' on.exit({
#'  batonname$logbook <- relay::read_logbook(loc = batonname$metadata$location);
#'
#'  relay::write_logbook(batonname, 'EXIT MESSAGE',
#'                       msg_type = 'MESSAGE', autoassign = TRUE, envir = environment())
#'
#'  # Push out of function
#'  assign(btn_name, batonname, envir = knitr::knit_global())
#' })
#' }
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
#' @param msg Location of YAML file that was saved from a \emph{baton}.
#' @param msg_type boolean value which will attempt only to reset the \code{pass_complete} status without loading the \emph{baton}.
#' @param trunc_long boolean value to determine if messages should be truncated at 256 characters.
#' @param suppressWarnings boolean value to determine if warning messages upon YAML write are ignored.
#' @param autoassign boolean value to determine if the passed baton is also refreshed automatically. Recommended as TRUE to ensure logging doesn't have to read and write each time an entry is made.
#' @param envir Environment where baton exists, default set to .GlobalEnv, only needed when autoassign is TRUE. If deploying on RStudio connect, may require \code{knitr::knit_global()}.
#' @param ... Additional parameters passed to \code{assign}
#' @seealso \code{\link{read_logbook}}
#' @return S3 class object.
#' @export
write_logbook <- function(baton, msg, msg_type = 'MESSAGE', trunc_long = TRUE, suppressWarnings = TRUE, autoassign = TRUE, envir = .GlobalEnv,  ...) {

  if(baton$metadata$pass_complete) stop('Baton pass complete, cannot write to logbook unless the relay is in process. Try running `grab_baton()`\n')

  btn_name <- deparse(substitute(baton))

  msg_types <- list('TRACE' = 1, 'DEBUG' = 2, 'MESSAGE' = 3, 'WARNING' = 4, 'ERROR' = 5)
  msg_type <- toupper(msg_type)
  msg_type <- match.arg(msg_type, choices = names(msg_types), several.ok = FALSE)

  # Exit early if not right msg threshold...
  thresh_n <- match(baton$metadata$referee, names(msg_types))
  msg_n <- match(msg_type, names(msg_types))
  if (msg_n < thresh_n) {
    if(autoassign){
      return(invisible(NULL))
    } else {
      return(invisible(baton))
    }
  }

  if(trunc_long) {
    if(nchar(msg) > 256) {
      msg <- paste0(strtrim(msg, 256), '...')
    }
  }

  log_msg <- paste0('Pass [', baton$metadata$passes_completed, '] ', Sys.time(), ' [', msg_type,'] ', msg)

  # Add to log
  baton$logbook <- c(baton$logbook, log_msg)

  # Update YAML
  if(suppressWarnings) {
    suppressWarnings(convert_baton2yml(baton, write = TRUE))
  } else {convert_baton2yml(baton, write = TRUE)}


  # Return value to env
  if(autoassign){
    assign(btn_name, baton, envir = envir, ...)
  } else {
    invisible(baton)
  }
}

#' Read a baton's logbook
#'
#' Read log information from a baton's logbook without affecting the metadata or contents.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}
#' @param loc Location of YAML file that was saved from a \emph{baton}.
#' @param as.list Boolean value, determine if returns as vector or a list.
#' @seealso \code{\link{write_logbook}}
#' @return S3 class object.
#' @export
read_logbook <- function(baton, loc = NULL, as.list = FALSE) {

  if(!is.null(loc)) {
    message('Attempting to load baton from YAML...')
    baton <- convert_yml2baton(loc)
  }

  if(as.list) {
    return(as.list(baton$logbook))
  } else baton$logbook

}


#' Set a baton's referee
#'
#' \code{set_referee} is a helper function to set the 'referee' of the baton; this controls what threshold of content is written to the logbook. The
#' updated metadata will occur both in the source YAML as well as the R object for paired consistency.
#'
#' The default threshold is set to \code{'TRACE'} and can be overriden directly or by setting the global options via \code{options(relay_referee = 'TRACE')}.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
#' @param threshold character value for the minimum threshold for logging to occur (e.g. 'TRACE', 'DEBUG', 'MESSAGE', 'WARNING', 'ERROR').
#' @param suppressWarnings boolean value to determine if warning messages upon YAML write are ignored.
#' @param autoassign boolean value to determine if the passed baton is also refreshed automatically. Recommended as TRUE to avoid having to do manual assignment to provided baton.
#' @param envir Environment where baton exists, default set to .GlobalEnv, only needed when autoassign is TRUE. If deploying on RStudio Connect, may require using \code{knitr::knit_global()}.
#' @param ... Additional parameters passed to \code{assign}
#'
#' @return S3 class object.
#' @export
#' @examples
#' \dontrun{
#' my_baton <- create_baton()
#' set_referee(my_baton, 'MESSAGE')
#' write_logbook(my_baton, 'A super important message')
#' set_referee(my_baton, 'ERROR')
#' write_logbook(my_baton, 'This message ignored b/c the referee cares not')
#' read_logbook(my_baton)
#' }
set_referee <- function(baton, threshold = getOption('relay_referee', default = 'TRACE'), suppressWarnings = TRUE, autoassign = TRUE, envir = .GlobalEnv, ...) {

  if(baton$metadata$pass_complete) stop('Baton pass complete, cannot set referee unless the relay is in process. Try running `grab_baton()`\n')

  btn_name <- deparse(substitute(baton))

  msg_types <- list('TRACE' = 1, 'DEBUG' = 2, 'MESSAGE' = 3, 'WARNING' = 4, 'ERROR' = 5)
  threshold <- toupper(threshold)
  threshold <- match.arg(threshold, choices = names(msg_types), several.ok = FALSE)

  # Update metadata in source YAML
  baton$metadata$referee <- threshold

  # Attempt to update metadata in source YAML
  tryCatch({
    tmp_baton <- convert_yml2baton(baton$metadata$location)
    tmp_baton$metadata$referee <- threshold
    convert_baton2yml(tmp_baton, write = TRUE)
  },
  error = function(err) {
    stop('Could not overwrite `referee` for baton YAML file.')
  })

  # Return value to env
  if(autoassign){
    assign(btn_name, baton, envir = envir, ...) #TODO use locate_batons? or mget loop?
  } else {
    invisible(baton)
  }

}

#' Set a baton's relay type
#'
#' \code{set_relay_type} is a helper function to set the 'relay_type' of the baton; this flag can be accessed by the user to help control how batons are accessed.
#' The updated metadata will occur both in the source YAML as well as the R object for paired consistency. There are three types of relay: 'CANCELLED', 'PRACTICE',
#' or 'COMPETITION'. The default is 'COMPETITION'. This can be adjusted globally by \code{options(relay_type = "COMPETITION")}
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
#' @param threshold character value for the type of the baton (e.g. 'CANCELLED', 'PRACTICE', or 'COMPETITION').
#' @param suppressWarnings boolean value to determine if warning messages upon YAML write are ignored.
#' @param autoassign boolean value to determine if the passed baton is also refreshed automatically. Recommended as TRUE to avoid having to do manual assignment to provided baton.
#' @param envir Environment where baton exists, default set to .GlobalEnv, only needed when autoassign is TRUE. If deploying on RStudio Connect, may require using \code{knitr::knit_global()}.
#' @param ... Additional parameters passed to \code{assign}
#'
#' @return S3 class object.
#' @export
#' @examples
#' \dontrun{
#' my_baton <- create_baton()
#'
#' set_relay_type(my_baton, 'CANCELLED')
#' read_metadata(my_baton)$relay_type
#'
#' set_referee(my_baton, 'COMPETITION')
#' read_metadata(my_baton)$relay_type
#' }
set_relay_type <- function(baton, threshold = getOption('relay_type', default = 'COMPETITION'), suppressWarnings = TRUE, autoassign = TRUE, envir = .GlobalEnv, ...) {

  # Use baton in env or load from YAML
  if(inherits(baton, 'baton')) {
    validate_baton(baton)
  } else if(file.exists(baton)) {
    message('Attempting to load baton from YAML...')
    baton <- convert_yml2baton(baton)
    validate_baton(baton)
  }

  btn_name <- deparse(substitute(baton))

  relay_types <- list('CANCELLED' = 1, 'PRACTICE' = 2, 'COMPETITION' = 3)
  threshold <- toupper(threshold)
  threshold <- match.arg(threshold, choices = names(relay_types), several.ok = FALSE)


  # Update in-memory baton
  baton$metadata$relay_type <- threshold

  # Attempt to update metadata in source YAML
  tryCatch({
    tmp_baton <- convert_yml2baton(baton$metadata$location)
    tmp_baton$metadata$relay_type <- threshold
    convert_baton2yml(tmp_baton, write = TRUE)
  },
  error = function(err) {
    stop('Could not overwrite `relay_type` for baton YAML file.')
  })

  # Return value to env
  if(autoassign){
    assign(btn_name, baton, envir = envir, ...) #TODO use locate_batons? or mget loop?
  } else {
    invisible(baton)
  }
}
