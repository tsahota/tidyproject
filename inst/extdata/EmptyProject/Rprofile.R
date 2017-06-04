### Begin tidyproject config

if (file.exists(file.path("~", ".Rprofile"))) source(file.path("~", ".Rprofile"))
.remove_user_lib <- FALSE
if (file.exists("ProjectLibrary")) {
  if (length(.Library.site) == 0 | !.remove_user_lib) {
    .libPaths(c("ProjectLibrary", .libPaths()))
  } else .libPaths("ProjectLibrary")
  message("Project Library configured")
  if (file.exists(file.path("ProjectLibrary", "tidyproject"))) {
    library("tidyproject")
  }
}

### End tidyproject config
