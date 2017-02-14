if(file.exists("ProjectLibrary")){
  .user_lib <- Sys.getenv("R_LIBS_USER")
  if(!.user_lib == "") .user_lib <- normalizePath(.user_lib)
  .currentlibPaths <- .libPaths()
  .libPaths(c("ProjectLibrary",.currentlibPaths[!.currentlibPaths %in% .user_lib]))
  message(paste("TidyProject package library set to: ",normalizePath("ProjectLibrary")))
  if(file.exists(file.path("ProjectLibrary","TidyProject"))) {
    library("TidyProject")
    message("TidyProject loaded")
  } else {
    message("TidyProject package is not installed in project library")
    message("Recommended: install into project library (e.g. install.packages(\"TidyProject\"))")
  }
}
