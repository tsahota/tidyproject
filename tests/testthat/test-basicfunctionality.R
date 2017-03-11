context("basic project functionality")

proj_name <- "test_tidyproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE)
}

test_that("Project has basic functionality",{

  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)

  expect_true(file.exists("ProjectLibrary"))
#browser()
  new_script("test.R",open_file = FALSE)
  expect_true(file.exists(file.path(getOption("scripts.dir"),"test.R")))

  Renvironment_info()
  res <- check_session(check_rstudio=FALSE)
  expect_true(is.data.frame(res))

})

test_that("R session stamp",{

  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)

  write(c("## Description: abc",
          "## Depends on: ",
          "library(TidyProject)",
          "require(TidyProject)",
          "TidyProject::make_project"),file=file.path(getOption("scripts.dir"),"test.R"))

  Renvironment_info()
  expect_true(file.exists("Renvironment_info.txt"))

})
