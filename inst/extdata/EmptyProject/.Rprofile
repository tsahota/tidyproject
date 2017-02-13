if(file.exists("ProjectLibrary")){
  .libPaths(c(file.path("ProjectLibrary"),.libPaths()))
  message(paste("TidyProject package library set to: ",normalizePath("ProjectLibrary")))
  if(file.exists(file.path("ProjectLibrary","TidyProject"))) {
    library("TidyProject")
    message("TidyProject loaded")
  } else {
    message("TidyProject package is not installed in project library")
    message("It is stongly recommended to (re)install into project library e.g.")
    message("   install.packages(\"TidyProject\") OR ")
    message("   install.packages(\"path/to/TidyProject/source\",repo=NULL)")
  }
}
