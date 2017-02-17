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

  make_project(proj_name,project_library = FALSE)

  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(proj_name))
  expect_false(file.exists(file.path(proj_name,"ProjectLibrary")))

  cleanup(proj_name)

  make_project(proj_name)

  expect_true(file.exists(proj_name))
  expect_true(is_tidy_project(proj_name))
  expect_true(file.exists(file.path(proj_name,"ProjectLibrary")))

  #cleanup(proj_name)
})

test_that("global variables look OK",{

  expect_true(!is.null(getOption("scripts.dir")))
  expect_true(!is.null(getOption("models.dir")))

})

test_that(".Rprofile works",{

  currentwd <- getwd() ; on.exit(setwd(currentwd))
  setwd(proj_name)

  expect_true(file.exists(".Rprofile"))
  .libPathsOld <- .libPaths()
  source(".Rprofile")

  expect_true(normalizePath(.libPaths()[1])==normalizePath("ProjectLibrary"))
  expect_false(normalizePath(Sys.getenv("R_LIBS_USER")) %in% .libPaths())

})

test_that("Project has basic functionality",{
  currentwd <- getwd() ; on.exit(setwd(currentwd))
  setwd(proj_name)

  expect_true(file.exists("ProjectLibrary"))

  new_script("test.R",silent = TRUE)
  expect_true(file.exists(file.path(getOption("scripts.dir"),"test.R")))

})

test_that("Code library",{
  currentwd <- getwd() ; on.exit(setwd(currentwd))
  setwd(proj_name)

  dir.create("code_lib_test")

  replace_code_library(NULL)
  code_library_path_old <- getOption("code_library_path")
  expect_true(is.null(code_library_path_old))

  expect_message(code_library())
  x <- code_library()
  expect_true(is.data.frame(x) & nrow(x)==0)

  attach_code_library("code_lib_test")
  expect_true(code_library_path()=="code_lib_test")
  expect_true(normalizePath("code_lib_test") %in% normalizePath(getOption("code_library_path")))

  write(c("## Description: abc",
          "## Depends on: "),file=file.path("code_lib_test","test2.R"))

  write(c("## Description: def",
          "## Depends on: test2.R"),file=file.path("code_lib_test","test3.R"))

  write(c("## Description: hij",
          "## Depends on: test2.R, test3.R"),file=file.path("code_lib_test","test4.R"))

  write(c("## Description: klm",
          "## Depends on: "),file=file.path("code_lib_test","test5.R"))

  expect_true(file.exists(file.path("code_lib_test","test2.R")))

  clib <- code_library(silent=TRUE)
  expect_true("data.frame" %in% class(clib))
  expect_true(nrow(clib)==4)

  clib <- code_library(fields = "Depends on",silent=TRUE)
  expect_true("data.frame" %in% class(clib))
  expect_true(nrow(clib)==4)

  copy_script("test2.R")
  expect_true(file.exists(file.path(getOption("scripts.dir"),"test2.R")))

  file_contents <- readLines(file.path(getOption("scripts.dir"),"test2.R"))
  expect_true(length(file_contents)>2)

  replace_code_library(NULL)
  expect_true(is.null(getOption("code_library_path")))
})

test_that("R session stamp",{

  currentwd <- getwd() ; on.exit(setwd(currentwd))
  setwd(proj_name)
  Renvironment_info()
  expect_true(file.exists(file.path(getOption("scripts.dir"),"RenvironmentInfo.R")))
  source(file.path(getOption("scripts.dir"),"RenvironmentInfo.R"))

})
