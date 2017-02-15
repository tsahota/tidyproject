library(TidyProject)

context("creating project")

proj_name <- "~/test"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE)
}

test_that("Project is created",{

  cleanup(proj_name)

  expect_false(file.exists(proj_name))
  expect_error(make_project(basename(proj_name)))

  make_project(proj_name)

  expect_true(file.exists(proj_name))
  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(file.path(proj_name,"ProjectLibrary")))

  cleanup(proj_name)
  make_project(proj_name,project_library = FALSE)

  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(proj_name))
  expect_false(file.exists(file.path(proj_name,"ProjectLibrary")))

  cleanup(proj_name)
})

test_that("Project is created",{
  cleanup(proj_name)
  make_project(proj_name)
  currentwd <- getwd() ; on.exit(setwd(currentwd))
  setwd(proj_name)
  expect_true(file.exists("ProjectLibrary"))
})
