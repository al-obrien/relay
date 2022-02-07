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
    batons <- l_objs[sapply(l_objs, function(x) inherits(get(x), 'baton'))]
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
