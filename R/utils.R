#TODO add ability to autoclean global env.

#' Cleanup batons
#'
#' Clear batons from a directory (YAML files) and/or R environment.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
#' @param loc file path to clear YAML files related to baton tracking.
#' @param envir environment to clear \emph{baton} objects from the active R session; default set to \code{.GlobalEnv}.
#' @param ... parameters passed to \code{\link{list.files}}
#'
#' @examples
#' \dontrun{
#' clear_batons(loc = '/tmp', recursive = TRUE)
#' }
#' @export
clear_batons <- function(baton, loc = NULL, envir = .GlobalEnv, ...) {

  if(!is.null(loc)) {
    rm_list <- list.files(loc, pattern = '^_baton-.*\\.yml', ...)
    warning('\nRemoving the following files: \n ', paste(rm_list, collapse = '\n '))
    invisible(file.remove(file.path(loc, rm_list)))
  } else {
    warning('\nRemoving the following files: \n ', paste(baton$metadata$location, collapse = '\n '))
    invisible(file.remove(baton$metadata$location))
    invisible(rm(list = deparse(substitute(baton)), envir = envir))
  }
}

#' Locate batons
#'
#' Search for batons in R environment or directory.
#'
#' @param loc location to look for \emph{batons}; default set to \code{.GlobalEnv}, file paths can also be used.
#' @param suppress_messages boolean value to determine if messages listing discovered \emph{batons} are suppressed.
#' @param ... parameters passed to \code{\link{list.files}}.
#'
#' @return character vector describing location of batons in file system or in R session by name.
#'
#' @export
#' @examples
#' \dontrun{
#' # Using folder path
#' locate_batons('path/to/baton/directory')
#'
#' # Check R environment
#' locate_batons()
#' }
locate_batons <- function(loc = .GlobalEnv, suppress_messages = FALSE, ...){

  # If environment
  if(is.environment(loc)) {

    if(!suppress_messages) {message('Environment variable detected, searching that location in R.')}
    l_objs <- ls(envir = loc)
    batons <- l_objs[vapply(l_objs, function(x) {inherits(get(x), 'baton') }, logical(1), USE.NAMES = F)]
    if(!suppress_messages) {
      if(length(batons) > 0) {
        writeLines(strwrap(paste0('The following batons are in the selected environment: ', paste0(batons, collapse = ', '))))
      } else {
        writeLines(strwrap('The following batons are in the selected environment: None'))
      }
    }

    invisible(batons)

  } else if(!dir.exists(normalizePath(loc))) {

    stop("Provided directory doesn't exist.")

  } else {

    if(!suppress_messages) {message('File path detected, searching that location on drive.')}
    batons <- list.files(loc, pattern = '^_baton-.*\\.yml', ...)
    if(!suppress_messages) writeLines(strwrap(paste0('The following batons are in the selected directory: ', paste0(batons, collapse = ', '))))
    invisible(batons)

  }
}

#TODO better support for nested content
#' Summarize batons
#'
#' Generic function to summarize and print information on \emph{baton} S3 class.
#'
#' @param object R object of S3 class, created by \code{\link{create_baton}}.
#' @param ... non-functional, left for future development.
#'
#' @export
summary.baton <- function(object, include_contents = TRUE, ...){
  validate_baton(object)
  writeLines(strwrap("Metadata of baton...", prefix = "\n"))
  writeLines(strwrap(unlist(lapply(names(object$metadata), function(x) paste0(' *', x , ': ', object$metadata[[x]])))))
  if(include_contents){
    writeLines(strwrap("Contents of baton...", prefix = "\n"))
    if(length(object$content) == 0) {
      writeLines(strwrap('  *No contents.'))
    } else {writeLines(strwrap(unlist(lapply(names(object$content), function(x) paste0(' *', x , ': ', object$content[[x]])))))}
  }
  invisible(object)
}


#' List all files under directory
#'
#' Helper function to quickly list all the files in a directory, and its sub-directories.
#'
#' This function may look very similar to \code{\link{list.files}} but the main difference is it will ensure
#' nested files have just the file names and not directories.
#'
#' @param path character vector defining the top directory to search downwards.
#' @param recursive passed to \code{\link{list.files}}; default set to \code{TRUE}.
#' @param basename_only boolean value, determines if only the base file names are output (i.e. no file path components).
#' @param ... parameters passed to \code{\link{list.files}}.
#'
#' @export
list_files_recursive <- function(path, recursive = TRUE, basename_only = TRUE, ...) {
  path <- normalizePath(path)

  if(!basename_only) {
    list.files(path = path, recursive = recursive, ...)
  } else {
    basename(list.files(path = path, recursive = recursive, ...))
  }
}

#' Quickly copy files based upon pattern alone
#'
#' Use regular-expressions via \code{\link{grepl}} to quickly copy files to a new location.
#'
#' Operates as a helper function that wraps around \code{\link{grepl}}, \code{\link{list.files}}, and \code{\link{file.copy}}.
#' For another helper function in \{relay\} that allows file copying, see \code{\link{copy_files}}.
#'
#' @param from,to character vectors, defining file paths.
#' @param pattern passed to \code{\link{grepl}}.
#' @param recursive passed to \code{\link{grepl}}.
#' @param grepl_params additional parameters passed to \code{\link{grepl}}.
#' @param ... additional parameters passed to \code{\link{file.copy}}.
#'
#' @export
quick_copy <- function(from, to, pattern, recursive = FALSE, grepl_params = list(), ...) {
  from <- normalizePath(from)

  file_list <- list.files(path = from, recursive = recursive, full.name = TRUE)

  subset_list <- do.call(grepl, args = c(list(x = file_list), list(pattern = pattern), grepl_params))
  subset_list <- file_list[subset_list]

  invisible(file.copy(subset_list,
            to = file.path(to, basename(subset_list)), ...))
}

#' Copy directory and all sub-folders
#'
#' Bundles into a tar, then unpacks in a new location. Works with nested folders/files. May need to use \code{full.names} to
#' ensure all subfiles are located and moved.
#'
#' @param from,to character vectors, defining file paths.
#' @param tar parameter passed to \code{\link[utils]{tar}}; may require assigning to \code{'tar'} manually if \code{Sys.getenv('tar')} has no value set.
#' @param extra_flags parameter passed to \code{\link[utils]{tar}}; defaults to \code{'-C'} for the \code{bundle_dir}.
#' @param ... additional parameters passed to \code{\link[utils]{tar}}
#'
#' @export
copy_dir <- function(from, to, tar, extra_flags, ...) {

  from <- normalizePath(from)
  to <- normalizePath(to, mustWork = FALSE)
  temp_file <- tempfile()

  # Stop if directory already exists or if the original location does not
  #TODO add overwrite parameter...
  stopifnot(dir.exists(from), !dir.exists(to))

  if(missing(extra_flags)) extra_flags <- paste('-C ', from)

  # Bundle in temp location with all files
  if(missing(tar)) tar <- Sys.getenv('tar')
  utils::tar(tarfile = temp_file,
      files = list.files(from, recursive = TRUE, ...),
      tar = tar,
      extra_flags = extra_flags)

  # Unload in new location
  if(missing(tar)) tar <- Sys.getenv('TAR')
  utils::untar(temp_file, exdir = to, tar = tar)

  # Attempt to delete temp file
  invisible(file.remove(temp_file))
}

#' Copy files to new location
#'
#' Helper function wrapped around \code{\link{file.copy}}.
#'
#' Basic functionality is making it simple to collapse \code{\link{file.copy}} with \code{\link{file.path}} and
#' ensure the \code{copy.date} is kept to \code{TRUE}.
#'
#' @param path character vector defining directory with files of interest.
#' @param file_names character vector of file names within the directory.
#' @param to character vector defining the destination directory for the files listed.
#' @param ... additional parameters passed to \code{\link{file.copy}}
#'
#' @export
copy_files <- function(path, file_names, to, ...) {
  path <- normalizePath(path)
  destination <- normalizePath(to)

  invisible(file.copy(file.path(path, file_names),
                      destination,
                      copy.date = TRUE,
                      ...))
}


#' Load a large list of mixed files from a folder
#'
#' Quickly load a large mixed set of files into R environment.
#'
#' This is only useful if all the files can be loaded with similar parameters. If certain columns or rows needs to be dropped
#' during loading, then it may be best to load those separately. The use-case with the \{relay\} package is to provide a way to
#' quickly take data files from a bundle into the environment. Loaded data will be assigned to the global environment by default with their
#' names all in lower-case.
#'
#' Files with the following extensions can all be loaded at the same time:
#' \enumerate{
#'     \item sas7bdat via \{haven\}
#'     \item xlsx via \{readxl\}
#'     \item csv via \{utils\} or \{readr\}
#'     \item rds via \{base\}
#' }
#'
#' Please note: when loading more troublesome/complex data, it may be easier to use the appropriate load functions directly.
#'
#' @param path character vector defining the directory containing files to load.
#' @param file_list character vector of files within directory to load.
#' @param env R environment to assign data to; default set to \code{.GlobalEnv}.
#' @param haven_arg,readxl_arg,csv_arg,fst_arg,rds_arg additional parameters passed to load functions depending on file extension.
#' @param readr boolean value to determine if \code{\link[readr]{read_csv}} is used in preference to \code{\link[utils]{read.csv}}.
#'
#' @return data-sets assigned to designated R environment.
#' @export
batch_load <- function(path, file_list, env = .GlobalEnv, haven_arg = list(), readxl_arg = list(), csv_arg = list(), fst_arg = list(), rds_arg = list(), readr = TRUE) {

  valid_args <- c('sas7bdat', 'xlsx', 'csv', 'rds', 'fst')

  extension <- gsub(tolower(file_list), replacement = '\\1', pattern = '.*\\.(.*$)')

  if(all(!extension %in% valid_args)) stop('Not all extensions in file list are valid, must be one of: ', paste0(valid_args, collapse = ' '))

  temp_f <- function(x, y, env) {

    temp_path <- file.path(path, x)

    switch(
      y,

      'sas7bdat' = {
        assign(value = do.call(haven::read_sas, c(temp_path, haven_arg)),
               x = sub(tolower(x), pattern = paste0('.', y ,'$'), replacement = ''),
               envir = env)

      },

      'xlsx' = {
        assign(value = do.call(readxl::read_xlsx, c(temp_path, readxl_arg)),
               x = sub(tolower(x), pattern = paste0('.', y ,'$'), replacement = ''),
               envir = env)
      },
      'csv' = {
        if(readr) {
          assign(value = do.call(readr::read_csv, c(temp_path, csv_arg)),
                 x = sub(tolower(x), pattern = paste0('.', y ,'$'), replacement = ''),
                 envir = env)
        } else {
          assign(value = do.call(utils::read.csv, c(temp_path, csv_arg)),
                 x = sub(tolower(x), pattern = paste0('.', y ,'$'), replacement = ''),
                 envir = env)
        }
      },
      'fst' = {
        assign(value = do.call(fst::read_fst, c(temp_path, fst_arg)),
               x = sub(tolower(x), pattern = paste0('.', y ,'$'), replacement = ''),
               envir = env)
      },
      'rds' = {
        assign(value = do.call(readRDS, c(temp_path, rds_arg)),
               x = sub(tolower(x), pattern = paste0('.', y ,'$'), replacement = ''),
               envir = env)
      }
    )
  }

  purrr::walk2(file_list, extension, ~temp_f(.x, .y, env = env))

}


#' Compress entire bundle to tar
#'
#' Helper function wrapped around \code{\link[utils]{tar}} for easy compression of \emph{bundles}.
#'
#' In the context of \{relay\}, \code{compress_bundle()} simplifies the process of compressing the bundle (see \code{\link{create_bundle}}) into a single object
#' so it can be stored more efficiently or transferred across networks. When the \emph{baton} object is provided, it will by default use the baton's unique-id within the bundle
#' and place the tar file within the parent directory. This behavior can be overridden by manually providing the file path to \code{bundle_dir} parameter and the
#' write location parameter \code{dir_to}. It is recommended to match the \code{file_extension} to the type of compression; for example, if \code{compression = 'gzip'}, the extension
#' should be \code{'.tar.gz'}.
#'
#' @param bundle_dir location of directory to compress; \emph{baton} can be provided to automate the process.
#' @param dir_to location to write the tar file; if \emph{baton} provided this will default to the parent directory.
#' @param file_extension character value; default set to \code{'.tar'}.
#' @param tar parameter passed to \code{\link[utils]{tar}}; may require assigning to \code{'tar'} manually if \code{Sys.getenv('tar')} has no value set.
#' @param extra_flags parameter passed to \code{\link[utils]{tar}}; defaults to \code{'-C'} for the \code{bundle_dir}.
#' @param ... additional parameters passed to \code{\link[utils]{tar}}
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a bundled baton
#' my_baton <- create_baton(bundled = TRUE,
#'                          bundle_params = list(dir = file.path('path', 'to', 'save', 'yaml'),
#'                                               tree = c('subdir1', 'subdir2', 'subdir3')))
#'
#' # Compress based upon baton
#' compress_bundle(my_baton, tar = 'tar', file_extension = '.tar.gz', compression = 'gzip')
#'
#' # Compress based upon bundle directory
#' compress_bundle('path/to/location/to/tar', tar = 'tar', file_extension = '.tar.gz', compression = 'gzip')
#'
#' # Uncompress: use normal method:
#' untar("path/to/nameoftarfile.tar.gz", exdir ="/unzip/location", tar = 'TAR')
#' }
compress_bundle <- function(bundle_dir, dir_to, file_extension = '.tar', tar, extra_flags, ...) {

  if(!missing(dir_to) && !dir.exists(dir_to)) stop('Write directory does not exist.')

  # If baton provided
  if(inherits(bundle_dir, 'baton')){

    validate_baton(bundle_dir)
    id <- bundle_dir$metadata$id
    dirname <- dirname(bundle_dir$metadata$location)

    # If no location to provided, write one level up
    if(missing(dir_to)) dir_to <- dirname(dirname)

    if(grepl(pattern = id, x = basename(dirname))) {
      from <- dirname
      message('Will compress directory based upon baton location: ', dirname)
    } else stop('Bundle directory name does not match unique core id (', id, ') of provided baton.')

    # If dir provided
  } else if (dir.exists(bundle_dir)){
    from <- bundle_dir
    message('Will compress directory provided manually: ', bundle_dir)
  } else stop('Invalid value for bundle_dir provided.')

  # Check values for tar parameters
  if(missing(extra_flags)) extra_flags <- paste('-C ', from)
  if(missing(tar)) tar <- Sys.getenv('tar')

  # Compress...
  utils::tar(tarfile = file.path(dir_to, paste0(basename(from), file_extension)),
             files = list.files(from, recursive = TRUE),
             tar = tar,
             extra_flags = extra_flags,
             ...)
}

#' Read metadata of baton
#'
#' Read the metadata content of a baton, either in the environment, or from a file source. This has some functional overlaps with \code{summary} but
#' is specific to the metadata section. This function will not alter the metadata content, and is a read-only operation. If you want to read log information
#' try \code{\link{read_logbook}}.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
#' @param loc file path to clear YAML files related to baton tracking.
#' @param subset a integer or character vector of metadata elements to return (defaults to ALL elements). Elements include: id, relay_start, relay_finish, all_passes, pass_complete, passes_completed, location, and dropped.
#' @export
#' @examples
#' \dontrun{
#'
#' batons_loc <- locate_batons('/location/where/bundlesorbatons/live', recursive = TRUE, full.name = TRUE)
#'
#' # Grab metadata of interest to determine which to load
#' baton_meta_pass <- purrr::map_lgl(batons_loc, ~unlist(read_metadata(loc = ., subset = c('pass_complete'))))
#' baton_meta_finish <- purrr::map_chr(batons_loc, ~unlist(read_metadata(loc = ., subset = c('relay_finish'))))
#'
#' # Equivalent in base R
#' baton_meta_pass <- vapply(batons_loc, function(x) unlist(read_metadata(loc = x, subset = c('pass_complete'))), logical(1), USE.NAMES = F)
#' baton_meta_finish <- vapply(batons_loc, function(x) unlist(read_metadata(loc = x, subset = c('relay_finish'))), character(1), USE.NAMES = F)
#'
#' # Determine which baton to keep!
#' select_baton <- batons_loc[which(baton_meta_finish == max(baton_meta_finish) & baton_meta_pass)]
#'
#' }
read_metadata <- function(baton, loc = NULL, subset = NULL) {

  if(!is.null(loc)) {
    baton <- convert_yml2baton(loc)
  }

  # Validate
  validate_baton(baton)

  # Return list or subset thereof
  if(!is.null(subset)) {
    subset <- tolower(subset)
    return(baton$metadata[subset])
  } else {
    return(baton$metadata)
  }
}

#' Append baton content
#'
#' Append content of baton instead of replacing the list content via \code{modifyList()}. Typically this can be done right at the
#' time when passing the baton, sent to the `content` parameter. Using this function should help ease the process of replacing items
#' nested within the list, though it is possible to do this manually too. It is recommended to always review the new content after
#' appending prior to passing the baton to ensure the process occurred as expected. If vector lengths or depths are not exact, there
#' could be unexpected entries.
#'
#' @param baton R object of S3 class, created by \code{\link{create_baton}}.
#' @param content_name Character vector in order of nested list component of existing baton content.
#' @param new_content Named list for new content to append to existing baton content, without replacing.
#' @param all_content Boolean, to decide if all content is returned or just the sections modified.
#'
#' @return List of the new content that should be appended when passing the baton
#'
#' @examples
#' \dontrun{
#' # Assuming there is an existing baton with content under 'data'...
#'
#' # Create some content to put under data, and already exists to append...
#' baton_content <- list('time_created' = Sys.time()))
#'
#' # Pass baton and do appending at same time
#' baton_example <- pass_baton(baton_example, content = append_content(baton_example, 'data', baton_content))
#'
#' # If need to append as well as add new content before passing, combine the append_content to another list
#' new_content <- append(append_content(baton_example, 'data', baton_content), list('new_content' = 'This is new content'))
#' baton_example <- pass_baton(baton_example, content = new_content)
#' }
#'
#' @export
append_content <- function(baton, content_name = NULL, new_content, all_content = TRUE){
  #TODO add more checks for new_content aligning to plucked... better way to align the content to append and where to slice down to...
  #TODO add functionality to replace entire content at once, not a subset list (i.e. what should it do with NULL default?)

  validate_baton(baton)
  if(!is.list(new_content)) stop('Parameter `new_content` must be a named list.')
  if(any(is.null(names(new_content)))) stop('Parameter `new_content` must be a named list.')
  if(any(is.null(names(baton$content)))) stop('Baton content must be a named list.') # This should never trigger...

  # Operations to append to existing list as by content name subset
  existing_content <- list(baton$content)
  content_subset <- as.list(content_name)
  plucked_content <- do.call(purrr::pluck, append(existing_content, content_subset)) # do.call necessary?

  # If still a list, then can append new_content as a list still...
  if(inherits(plucked_content, 'list')) {
    appended_content <- purrr::map2(plucked_content, new_content, append)
  } else {
    appended_content <- append(plucked_content, unlist(new_content, use.names = FALSE))
  }

  # Replace appended content at source
  new_content <- purrr::assign_in(baton$content, content_subset, appended_content)

  # Return full content list
  if(all_content) {
    return(new_content)
  } else {
    return(do.call(purrr::pluck, append(list(new_content), content_subset)))
  }
}

#' Parse the baton's logbook for details of interest
#'
#' Will parse based on various REGEX capturing groups:
#'
#' \itemize{
#'   \item{"Group 1"}{entire content}
#'   \item{"Group 2"}{pass number}
#'   \item{"Group 3"}{DateTime}
#'   \item{"Group 4"}{Date}
#'   \item{"Group 5"}{Time}
#'   \item{"Group 6"}{message type}
#'   \item{"Group 7"}{message content}
#' }
#'
#' @param baton_logbook A baton or a vector containing a baton's logbook details.
#' @param target Character vector for parsing target in logbook (one of: 'PASS', 'PASS_NUMBER', 'DATETIME', 'DATE' 'TIME', , 'MESSAGE_TYPE', 'MESSAGE').
#' @export
parse_logbook <- function(baton_logbook, target = c('PASS', 'PASS_NUMBER', 'DATE', 'TIME', 'DATETIME', 'MESSAGE_TYPE', 'MESSAGE')) {

  # If baton based instead of logbook vector, parse it...
  if(inherits(baton_logbook, 'baton')) {
    logbook_content <- relay::read_logbook(test_baton, as.list = FALSE)
  } else {
    logbook_content <- baton_logbook
  }

  # Determine which to return
  target <- match.arg(target,
                      choices = c('PASS', 'PASS_NUMBER', 'DATE', 'TIME', 'DATETIME', 'MESSAGE_TYPE', 'MESSAGE'),
                      several.ok = TRUE)

  pattern_search <- '^((Pass\\s\\[\\d\\])\\s(([\\d]{4}-[\\d]{2}-[\\d]{2})\\s([\\d]{2}:[\\d]{2}:[\\d]{2}))\\s(\\[[\\w]*\\]))\\s(.*)$'

  # Predefine vector to return
  return_list <- vector(mode = 'list', length = length(target))
  return_list <- setNames(return_list, target)

  logbook_contents <- read_logbook(test_baton, as.list = FALSE)

  # Apply content to each part of list based on extract method
  for(i in target){
    temp <- switch(i,
                   'PASS' = {gsub(pattern_search, '\\2', logbook_content, perl = TRUE)},
                   'PASS_NUMBER' = {gsub(pattern = '^Pass\\s\\[(\\d)\\]$', '\\1', x = gsub(pattern_search, '\\2', logbook_content, perl = TRUE))},
                   'DATETIME' = {gsub(pattern_search, '\\3', logbook_content, perl = TRUE)},
                   'DATE' = {gsub(pattern_search, '\\4', logbook_content, perl = TRUE)},
                   'TIME' = {gsub(pattern_search, '\\5', logbook_content, perl = TRUE)},
                   'MESSAGE_TYPE' = {gsub(pattern_search, '\\6', logbook_content, perl = TRUE)},
                   'MESSAGE' = {gsub(pattern_search, '\\7', logbook_content, perl = TRUE)}
                   )
    return_list[[i]] <- temp
  }

  return(return_list)

}


#' Plot baton metadata
#'
#' Will parse the baton metadata and logbook to provide graphical aides in determining when
#' pass, grab, and logging operations occured.
#'
#' @param baton S3 object of class 'baton'.
#' @param relative_time Boolean, should time be relative to start or absolute (in seconds).
#' @param separate Boolean, determine if pass numbers should be on separate lines.
#' @param include_logs Boolean, include or exclude log times.
#' @param point_offset Numeric value, proportion for how far to offset points from lines.
#' @param x_label_length Numeric value, for how many x-axis ticks to draw.
#' @param ... Additional parameters to plotting features (not in use yet...)
#'
#' @examples
#' \dontrun{
#' library(relay)
#'
#' test_baton <- create_baton()
#' write_logbook(test_baton, 'Test message 1')
#' test_baton <- pass_baton(test_baton)
#' write_logbook(test_baton, 'Test message 2')
#' test_baton <- grab_baton(test_baton)
#'
#' plot(test_baton)
#' plot(test_baton, relative_time =  TRUE, separate = TRUE, include_logs = TRUE)
#' }
plot.baton <- function(baton,
                       relative_time = TRUE,
                       separate = FALSE,
                       include_logs = FALSE,
                       x_label_length = 3,
                       point_offset = 0.02,
                       ...) {

  # Date formats for passes
  format_meta <- '%Y-%m-%d %H-%M-%S'
  format_logs <- '%Y-%m-%d %H:%M:%S'

  # Times of passes, create dataset
  pass_vector <- 0:(read_metadata(baton, subset = 'passes_completed')[[1]] - 1)
  min_time <- as.POSIXct(read_metadata(baton, subset = 'relay_start')[[1]], format = format_meta)
  max_time <- as.POSIXct(read_metadata(baton, subset = 'relay_finish')[[1]], format = format_meta)
  pass_times <- as.POSIXct(read_metadata(baton, subset = 'all_passes')[[1]], format = format_meta)
  grab_times <- c(min_time, as.POSIXct(read_metadata(baton, subset = 'all_grabs')[[1]], format = format_meta))

  if(relative_time) {
    max_time <- max_time - min_time
    pass_times <- pass_times - min_time
    grab_times <- grab_times - min_time
    min_time_abs <- min_time
    min_time <- min_time - min_time
  }

  pass_data <- data.frame(x0 = grab_times, x1 = pass_times, y0 = pass_vector)

  if(!separate) {

    # Initialize (plot all on 1 Y axis)
    plot.new()
    plot.window(xlim = c(min_time, max_time), ylim = range(0.5, 1.5))

    # Axes and labels
    x_labs <- seq(min_time, max_time, length.out = x_label_length)
    title(ylab = 'Pass Summary', main = 'Baton Timelapse Summary')

    if(relative_time) {
      axis(1, x_labs, las = 1, cex.axis = .75, font = 1)
      title(xlab = 'Time (seconds)')
    } else {
      axis(1, x_labs, labels = format(x_labs, "%b %d '%y \n(%H:%M)"), las = 1, cex.axis = .75, font = 1)
      title(xlab = 'Date (Time)')
    }

    # Add plots
    segments(x0 = pass_data$x0, x1 = pass_data$x1,
             y0 = 1, y1 = 1,
             col = 'grey50', lty = 1)
    points(pass_data$x0, rep(1, length(pass_vector)) * (1 + point_offset), pch = 25, col= 'green', bg = 'green')
    points(pass_data$x1, rep(1, length(pass_vector)) * (1 - point_offset), pch = 24, col= 'red', bg = 'red')

  } else {

    # Initialize
    plot.new()
    plot.window(xlim = c(min_time, max_time), ylim = range(pass_vector))

    # Axes and labels
    x_labs <- seq(min_time, max_time, length.out = x_label_length)
    axis(2, at = pass_data$y0,las = 2)
    title(ylab = 'Pass Number', main = 'Baton Timelapse Summary')

    if(relative_time) {
      axis(1, x_labs, las = 1, cex.axis = .75, font = 1)
      title(xlab = 'Time (seconds)')
    } else {
      axis(1, x_labs, labels = format(x_labs, "%b %d '%y \n(%H:%M)"), las = 1, cex.axis = .75, font = 1)
      title(xlab = 'Date (Time)')
    }

    segments(x0 = pass_data$x0,
             x1 = pass_data$x1,
             y0 = pass_data$y0,
             y1= pass_data$y0,
             col = 'grey50', lty = 1)
    points(pass_data$x0, pass_data$y0 * (1 + point_offset), pch = 25, col= 'green', bg = 'green')
    points(pass_data$x1, pass_data$y0 * (1 - point_offset), pch = 24, col= 'red', bg = 'red')

  }

  # --------------------------- #
  # Log additions
  # --------------------------- #

  if(include_logs) {

    # Date formats for logs
    format_logs <- '%Y-%m-%d %H:%M:%S'

    # Define helper for log colors
    log_cols <- function(vec){
      ifelse(vec == '[TRACE]', 'aquamarine',
             ifelse(vec == '[DEBUG]', 'maroon',
                    ifelse(vec == '[MESSAGE]', 'orange2',
                           ifelse(vec == '[WARNING]', 'yellow1',
                                  ifelse(vec == '[ERROR]', 'red2', NA_character_)
                           )
                    )
             )
      )
    }

    if(relative_time) {
      # Times of logs, create dataset
      log_data <- data.frame(x = as.POSIXct(parse_logbook(test_baton, 'DATETIME')[[1]], format = format_logs) - min_time_abs,
                             y = as.integer(parse_logbook(test_baton, 'PASS_NUMBER')[[1]]),
                             msg_type = parse_logbook(test_baton, 'MESSAGE_TYPE')[[1]])

    } else {
      # Times of logs, create dataset
      log_data <- data.frame(x = as.POSIXct(parse_logbook(test_baton, 'DATETIME')[[1]], format = format_logs),
                             y = as.integer(parse_logbook(test_baton, 'PASS_NUMBER')[[1]]),
                             msg_type = parse_logbook(test_baton, 'MESSAGE_TYPE')[[1]])
    }

    # Add log times
    points(log_data$x, log_data$y, pch = '|', col = log_cols(log_data$msg_type), cex = 1.25)

    # Add legend
    legend(x = "topleft",
           legend = c('Start/Grab', 'End/Pass', 'Log: TRACE', 'Log: DEBUG', 'Log: MESSAGE',
                      'Log: WARNING', 'Log: ERROR'),
           col = c('green', 'red', 'aquamarine', 'maroon', 'orange2', 'yellow1', 'red2'),
           pch = c(25, 24, rep(124, 5)),
           pt.bg = c('green', 'red', rep(NA,5)),
           bty ='n')

  } else {

    # Add legend
    legend(x = "topleft",
           legend = c('Start/Grab', 'End/Pass'),
           col = c('green', 'red'),
           pch = c(25,24),
           pt.bg = c('green', 'red'),
           bty ='n')
  }
}
