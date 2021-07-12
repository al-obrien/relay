#' Convert from YAML to a baton
#'
#' Internal function.
convert_yml2baton <- function(x) {
  if(file.exists(x)) x <- yaml::read_yaml(x) else x <- yaml::read_yaml(text = x)

  if(is.null(x[['metadata']]) || is.null(x[['metadata']][['id']])) stop('Metadata missing from baton, likely due to corrupted or non-baton generated YAML file')

  class(x) <- "baton"
  x
}

#' Convert baton to YAML
#'
#' Internal function.
convert_baton2yml <- function(baton, write = TRUE, ...) {

  # Error check
  validate_baton(baton)

  # Check for errors (e.g. duplicates in yaml)
  tryCatch(expr = {
    invisible(yaml::yaml.load(yaml::as.yaml(baton)))

    if(write) {
      if(file.exists(baton$metadata$location)) warning('Overwritting existing baton file...')
      yaml::write_yaml(baton, baton$metadata$location, ...)
    } else {
      yaml::as.yaml(baton)
    }
  },
  error=function(err) {
    stop('Error encountered when converting to YAML: ', err)
  })
}
