## Personal setup for Rene to deal with google drive path


get_path <- function(..., 
                     root = "G:/Shared drives/ohw-obis-mpa",
                     expand = FALSE){
  path <- file.path(root, ...)
  if (expand) path <- path.expand(path)
  return(path)
}