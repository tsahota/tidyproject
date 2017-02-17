context("basic project functionality")

proj_name <- "test_tidyproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE)
}

test_that("Project has basic functionality",{

  cleanup(proj_name) ; make_project(proj_name)

  setwd(proj_name)

  expect_true(file.exists("ProjectLibrary"))

  new_script("test.R",silent = TRUE)
  expect_true(file.exists(file.path(getOption("scripts.dir"),"test.R")))

})

test_that("R session stamp",{

  cleanup(proj_name) ; make_project(proj_name)

  setwd(proj_name)
  Renvironment_info()
  expect_true(file.exists(file.path(getOption("scripts.dir"),"RenvironmentInfo.R")))
  source(file.path(getOption("scripts.dir"),"RenvironmentInfo.R"))

})
