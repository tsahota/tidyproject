library(TidyProject)

context("creating project")

test_that("Project is created",{
  proj_name <- "~/test"
  cleanup <- function(){
    if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE)
    base_proj_name <- paste0(proj_name,".git")
    if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE)
  }
  cleanup()

  expect_false(file.exists(proj_name))
  expect_error(make_project(basename(proj_name)))

  make_project(proj_name)

  expect_true(file.exists(proj_name))
  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(file.path(proj_name,"ProjectLibrary")))

  cleanup()
  make_project(proj_name,project_library = FALSE)

  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(proj_name))
  expect_false(file.exists(file.path(proj_name,"ProjectLibrary")))

})
