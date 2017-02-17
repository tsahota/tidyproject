library(TidyProject)

context("creating project")

proj_name <- "test_tidyproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE)
}

test_that("Project is created",{

  cleanup(proj_name)

  expect_false(file.exists(proj_name))

  make_project(proj_name,project_library = FALSE)

  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(proj_name))
  expect_false(file.exists(file.path(proj_name,"ProjectLibrary")))

  cleanup(proj_name)

  make_project(proj_name)

  expect_true(file.exists(proj_name))
  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(file.path(proj_name,"ProjectLibrary")))

  cleanup(proj_name)
})

test_that("global variables look OK",{

  cleanup(proj_name) ; make_project(proj_name)

  expect_true(!is.null(getOption("scripts.dir")))
  expect_true(!is.null(getOption("models.dir")))

  cleanup(proj_name)

})

test_that(".Rprofile works",{

  cleanup(proj_name) ; make_project(proj_name)

  setwd(proj_name)

  expect_true(file.exists(".Rprofile"))
  .libPathsOld <- .libPaths()
  source(".Rprofile")

  expect_true(normalizePath(.libPaths()[1])==normalizePath("ProjectLibrary"))
  expect_false(normalizePath(Sys.getenv("R_LIBS_USER")) %in% .libPaths())

  cleanup(proj_name)
})

