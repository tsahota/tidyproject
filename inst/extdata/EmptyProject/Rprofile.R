### Begin tidyproject config

.remove_user_lib <- FALSE  ## if changing this, restart R for change to take effect

if (file.exists(file.path("~", ".Rprofile"))) source(file.path("~", ".Rprofile"))
if (file.exists("ProjectLibrary")) {
  if (length(.Library.site) == 0 | !.remove_user_lib) {
    .libPaths(c("ProjectLibrary", .libPaths()))
  } else .libPaths("ProjectLibrary")
  message("Project Library configured")
}

### End tidyproject config
