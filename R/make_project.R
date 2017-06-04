copy_empty_project <- function(proj_name,remove_user_lib){
  .Rprofile_name <- normalizePath(file.path(proj_name, ".Rprofile"),winslash = "/",mustWork = FALSE)
  Rprofile.R_name <- normalizePath(file.path(proj_name, "Rprofile.R"),winslash = "/",mustWork = FALSE)
  file.copy(file.path(system.file("extdata/EmptyProject", package = "tidyproject"), 
                      "."), proj_name, recursive = TRUE, overwrite = FALSE)
  if(!file.exists(.Rprofile_name)){
    result <- file.rename(Rprofile.R_name,.Rprofile_name)
    if(!result) stop("unable to create project config file")
  } else {
    existing_lines <- readLines(.Rprofile_name)
    if(any(grepl("ProjectLibrary",existing_lines)))
      stop("Existing ProjectLibrary setup lines found in ",
           .Rprofile_name,"\n Remove and then try again",call. = FALSE)
    new_lines <- readLines(Rprofile.R_name)
    cat(paste0("\n",new_lines),file = .Rprofile_name,append = TRUE)
  }
  config_lines <- readLines(.Rprofile_name)
  config_lines <- gsub("^(\\.remove_user_lib <- )\\S*$",
                       paste0("\\1",remove_user_lib),
                       config_lines)
  write(config_lines,file=.Rprofile_name)
}


#' Create new_project
#'
#' Creates directory structure.  User install tidyproject again in
#'
#' @param proj_name character string of full path to new_project
#' @param remove_user_lib logical (default=FALSE) if TRUE will attempt to remove 
#'   user R package library from .libPaths()
#'
#' @export
make_project <- function(proj_name, remove_user_lib = FALSE) {
    ## must be full path.  User function: create new_project
    new_proj <- !file.exists(proj_name)
    if (new_proj) {
        tryCatch({
            message("Directory doesn't exist. Creating...")
            dir.create(proj_name)
            copy_empty_project(proj_name=proj_name,remove_user_lib=remove_user_lib)
            if (!TRUE %in% file.info(proj_name)$isdir) 
                stop(paste(proj_name, "not created"))
        }, error = function(e) {
            message("Aborting. Reversing changes...")
            unlink(proj_name, recursive = TRUE, force = TRUE)
            stop(e)
        })
    } else {
        message("Directory exists. Merging...")
        ## find common files that wont be overwritten.
        all_templates <- dir(system.file("extdata/EmptyProject", package = "tidyproject"), 
            include.dirs = TRUE, all.files = TRUE, recursive = TRUE)
        all_existing <- dir(proj_name, include.dirs = TRUE, all.files = TRUE, recursive = TRUE)
        
        merge_conf <- intersect(all_templates, all_existing)
        message("\n---Merge conflict on files/folders (will not replace)---:\n")
        message(paste(merge_conf, collapse = "\n"))
        message("")
        copy_empty_project(proj_name=proj_name,remove_user_lib=remove_user_lib)
    }
    if (getOption("git.exists")) {
        currentwd <- getwd()
        on.exit(setwd(currentwd))
        setwd(proj_name)
        bare_proj_name <- gsub(basename(proj_name), paste0(basename(proj_name), ".git"), 
            proj_name)
        tryCatch({
            r <- git2r::init(".")
            if (!file.exists(".gitignore")) {
                s <- unique(c(".Rproj.user", ".Rhistory", ".RData", getOption("git.ignore.files")))
                write(s, ".gitignore")
            }
            paths <- unlist(git2r::status(r))
            if (length(git2r::reflog(r)) == 0) {
                git2r::add(r, paths)
                git2r::config(r, user.name = Sys.info()["user"], user.email = getOption("user.email"))
                git2r::commit(r, "initialise_repository")
            }
        }, error = function(e) {
            setwd(currentwd)
            if (new_proj) {
                message("Aborting. Reversing changes...")
                unlink(proj_name, recursive = TRUE, force = TRUE)
                unlink(bare_proj_name, recursive = TRUE, force = TRUE)
            }
            stop(e)
        })
    }
    message(paste("tidyproject directory ready:", proj_name))
    message("----------------------------------------------------")
    message("")
    message("INSTRUCTIONS:")
    message(paste("1. Open Rstudio project to start working: ", proj_name))
    message(paste("2. (optional) Install tidyproject package in project library"))

}

#' create local bare repository
#' @param proj_name character vector indicating path to tidyproject
#' @export
make_local_bare <- function(proj_name = getwd()) {
    currentwd <- getwd()
    on.exit(setwd(currentwd))
    setwd(proj_name)
    status <- git2r::status()
    if (length(status$untracked) > 0) 
        stop("untracked files detected. Create bare repositories manually.")
    if (length(status$unstaged) > 0) 
        stop("commit changes before continuing")
    proj_name_full <- getwd()
    bare_proj_name_full <- paste0(proj_name_full, ".git")
    git2r::clone(proj_name_full, bare_proj_name_full, bare = TRUE)
    setwd("../")
    res <- unlink(proj_name_full, recursive = TRUE, force = TRUE)
    git2r::clone(bare_proj_name_full, proj_name_full)
}
