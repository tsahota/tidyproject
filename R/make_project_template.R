make_project_template <- function(path, ...) {
  
  make_project(proj_name = path, ...)
  
  unlink(file.path(path, "OpenProject.Rproj"))
  
}