if(file.exists("ProjectLibrary")){
  .libPaths("ProjectLibrary")
  message(paste("User library replaced with TidyProject package library: ",normalizePath("ProjectLibrary")))
  if(file.exists(file.path("ProjectLibrary","TidyProject"))) {
    library("TidyProject")
    message("TidyProject loaded")
  } else {
    message("TidyProject package is not installed in project library")
    message("Recommended: install into project library (e.g. install.packages(\"TidyProject\"))")
  }
}
