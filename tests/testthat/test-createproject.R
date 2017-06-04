library(tidyproject)

context("creating project")

proj_name <- "test_tidyproject"

cleanup <- function(proj_name) {
    if (file.exists(proj_name)) 
        unlink(proj_name, recursive = TRUE, force = TRUE)
    base_proj_name <- paste0(proj_name, ".git")
    if (file.exists(base_proj_name)) 
        unlink(base_proj_name, recursive = TRUE, force = TRUE)
}

test_that("Project is created", {

    currentwd <- getwd()
    cleanup(proj_name)
    on.exit({
        setwd(currentwd)
        cleanup(proj_name)
    })
    
    #expect_false(file.exists(proj_name))
    
    make_project(proj_name, remove_user_lib = TRUE)
    
    expect_true(is_tidyproject(proj_name))
    expect_true(file.exists(proj_name))
    
    config_lines <- readLines(file.path(proj_name, ".Rprofile"))
    expect_true(any(grepl(".remove_user_lib <- TRUE",config_lines)))
    
    cleanup(proj_name)
    make_project(proj_name)
    
    expect_true(file.exists(proj_name))
    expect_true(is_tidyproject(proj_name))
    expect_true(file.exists(file.path(proj_name, "ProjectLibrary")))
    
    unlink(file.path(proj_name,".Rprofile"))
    make_project(proj_name)  ## merges directories

})

test_that("make bare repository", {
    
    currentwd <- getwd()
    make_project(proj_name)
    on.exit({
        setwd(currentwd)
        cleanup(proj_name)
    })
    
    make_local_bare(proj_name)
    expect_true(file.exists(paste0(proj_name, ".git")))
    
})

test_that("global variables look OK", {
    
    currentwd <- getwd()
    make_project(proj_name)
    on.exit({
        setwd(currentwd)
        cleanup(proj_name)
    })
    
    expect_true(!is.null(getOption("scripts.dir")))
    expect_true(!is.null(getOption("models.dir")))
    
    set_project_opts()
})

test_that(".Rprofile works", {
    
    # testthat::skip_on_travis() testthat::skip_on_cran()
    
    currentwd <- getwd()
    make_project(proj_name)
    on.exit({
        setwd(currentwd)
        cleanup(proj_name)
    })
    
    setwd(proj_name)
    
    .libPathsOld <- .libPaths()
    on.exit(.libPaths(.libPathsOld), add = TRUE)
    
    source(".Rprofile")
    
    expect_true(normalizePath("ProjectLibrary", winslash = "/") %in% .libPaths())
    
})

