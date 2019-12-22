#####################################
### Begin tidyproject config

if (file.exists(file.path("~", ".Rprofile"))) source(file.path("~", ".Rprofile"))

.libPaths(c(normalizePath("ProjectLibrary"),.libPaths()))

## set default library config strategy
suppressMessages({
  tidyproject::toggle_libs("project-user")
})

### End tidyproject config
#####################################

## Code written below here will execute each time R starts in this project
