.onLoad <- function(libname, pkgname) {
  set_project_opts()
  load_localpackage(fail_silently = TRUE)
}
