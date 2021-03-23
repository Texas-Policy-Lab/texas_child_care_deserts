#' @title Create data folder structure
#' @param root string. The root directory
#' @param pth list. A list of paths to create in the root directory
create_folder_str <- function(pths) {

  lapply(pths, function(pth) {

    if(!dir.exists(pth)) {
      dir.create(pth, recursive = TRUE)
    }
  })
}
