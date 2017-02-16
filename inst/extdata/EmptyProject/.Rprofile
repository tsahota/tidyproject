if(file.exists("ProjectLibrary")){
  .libPaths("ProjectLibrary")
  message(paste("User library replaced with TidyProject package library: ",normalizePath("ProjectLibrary")))
  if(file.exists(file.path("ProjectLibrary","TidyProject"))) {
    library("TidyProject")
    message("TidyProject loaded")
  }
}
