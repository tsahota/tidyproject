if(file.exists(file.path("~",".Rprofile"))) source(file.path("~",".Rprofile"))
if(file.exists("ProjectLibrary")){
  if(length(.Library.site) == 0){
    .libPaths(c("ProjectLibrary",.libPaths()))
  } else .libPaths("ProjectLibrary")
  message(paste("tidyproject package library set up: ",normalizePath("ProjectLibrary")))
  if(file.exists(file.path("ProjectLibrary","tidyproject"))) {
    library("tidyproject")
    message("tidyproject loaded")
  }
}
