context("Code Library")

proj_name <- "test_tidyproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE)
}

test_that("Code library",{

  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)

  dir.create("code_lib_test")

  replace_code_library(NULL)
  code_library_path_old <- getOption("code_library_path")
  on.exit(replace_code_library(code_library_path_old))
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

  clib <- code_library(viewer=FALSE,silent=TRUE)
  expect_true("character" %in% class(clib))

  clib <- code_library(viewer=FALSE,fields = "Depends on",silent=TRUE)
  expect_true("character" %in% class(clib))

  clib <- code_library(viewer=FALSE,silent=TRUE,return_info = TRUE)
  expect_true("data.frame" %in% class(clib))

  copy_script("test2.R")
  expect_true(file.exists(file.path(getOption("scripts.dir"),"test2.R")))

  file_contents <- readLines(file.path(getOption("scripts.dir"),"test2.R"))
  expect_true(length(file_contents)>2)

  info <- info_scripts(code_library(viewer = FALSE))
  expect_true("data.frame" %in% class(info))

  matched.file <- search_scripts(code_library(viewer = FALSE),"hi")
  expect_true(normalizePath(matched.file)==normalizePath(file.path("code_lib_test","test4.R")))

  matched.file <- search_scripts(code_library(viewer = FALSE),"nomatch")
  expect_true(length(matched.file)==0)

  preview("test4.R")

  replace_code_library(NULL)
  expect_true(is.null(getOption("code_library_path")))


})

