if(file.exists("ProjectLibrary")){
  .libPaths(c("ProjectLibrary",.Library.site,.Library))
  message(paste("TidyProject package library set to: ",normalizePath("ProjectLibrary")))
  if(file.exists(file.path("ProjectLibrary","TidyProject"))) {
    library("TidyProject")
    message("TidyProject loaded")
  } else {
    message("TidyProject package is not installed in project library")
    message("Recommended: install into project library (e.g. install.packages(\"TidyProject\"))")
  }
}
