if(file.exists("ProjectLibrary")){
  if(length(.Library.site) == 0){
    .libPaths(c("ProjectLibrary",.libPaths()))
  } else .libPaths("ProjectLibrary")
  message(paste("TidyProject package library set up: ",normalizePath("ProjectLibrary")))
  if(file.exists(file.path("ProjectLibrary","TidyProject"))) {
    library("TidyProject")
    message("TidyProject loaded")
  }
}
if(file.exists(file.path("~",".Rprofile"))) source(file.path("~",".Rprofile"))
