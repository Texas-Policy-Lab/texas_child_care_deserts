#' @title Create data folder structure
#' @param root string. The root directory
#' @param pth list. A list of paths to create in the root directory
create_folder_str <- function(root, pths) {

  lapply(pths, function(p, root) {
    
    pth <- file.path(root, p)
    
    if(!dir.exists(pth)) {
      dir.create(pth, recursive = TRUE)
    }
  }, root = root)
}
