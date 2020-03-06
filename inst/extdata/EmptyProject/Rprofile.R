#####################################
### Begin tidyproject config

if (file.exists(file.path("~", ".Rprofile"))) source(file.path("~", ".Rprofile"))

R_version <- paste0(R.version$major, ".", tools::file_path_sans_ext(R.version$minor))
proj_lib <- file.path("ProjectLibrary", R_version)
dir.create(proj_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(proj_lib,.libPaths()))
rm(proj_lib, R_version)

## set default library config strategy
suppressMessages({
  tidyproject::toggle_libs("project-user")
})

### End tidyproject config
#####################################

## Code written below here will execute each time R starts in this project
