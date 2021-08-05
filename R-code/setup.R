# README
#
# Run me first!

#' Get a path within the Google Shared Drive
#' 
#' @param ... character, zero or more path components like directories or filenames
#' @param root character, the root working directory, assumed to be the same for all,
#'   but use this to override as needed
#' @param expand logical, if TRUE pass the output to \code{expand.path} to replace
#'   a tilde.
#' @return path description
#' @examples
#' \dontrun{
#' # get the root path
#' get_path()
#' [1] "~/Google Drive File Stream/Shared drives/ohw-obis-mpa"
#' 
#' # get the root path, but don't expand tildes
#' get_path(expand = TRUE)
#' [1] "/Users/ben/Google Drive File Stream/Shared drives/ohw-obis-mpa"
#' # get the path to this file
#' get_path("Code", "R", "setup.R")
#' [1] "~/Google Drive File Stream/Shared drives/ohw-obis-mpa/Code/R/setup.R"
#' }
get_path <- function(..., root = "~/Google Drive File Stream/Shared drives/ohw-obis-mpa",
                     expand = FALSE){
  path <- file.path(root, ...)
  if (expand) path <- path.expand(path)
  return(path)
}


#' Retrieve a protected planet token stored in a file.
#'
#' Accepted practice is to store the token in a hidden file in your home directory
#' and write a function (in whatever language) to read it for you.  Generally, you
#' don't want to hardwire the token into your code.
#'
#' @param filename character, the name of the file where the token is stored
#' @return character token assigned by Protected Planet
protected_planet_token <- function(filename = "~/.protected_planet_api"){
  stopifnot(file.exists(filename[1]))
  readLines(filename)[1]
}
