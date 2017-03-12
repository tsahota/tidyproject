#' Create new_project
#'
#' Creates directory structure.  User install tidyproject again in
#'
#' @param proj_name character string of full path to new_project
#' @param project_library TRUE (default) or FALSE indicating whether or not to create a project library
#'
#' @export
make_project <- function(proj_name,project_library=TRUE){ ## must be full path.
  ## User function: create new_project
  new_proj <- !file.exists(proj_name)
  if(new_proj){
    tryCatch({
      message("Directory doesn't exist. Creating...")
      dir.create(proj_name)
      file.copy(file.path(system.file("extdata/EmptyProject",package="tidyproject"),"."),proj_name,recursive=TRUE)
      result <- file.rename(file.path(proj_name,"Rprofile.R"),file.path(proj_name,".Rprofile"))
      if(!result) stop("something wrong with .Rprofile creation") else
        unlink(file.path(proj_name,"Rprofile.R"))
      if(!TRUE %in% file.info(proj_name)$isdir) stop(paste(proj_name,"not created"))
      if(!project_library) unlink(file.path(proj_name,"ProjectLibrary"),recursive = TRUE)
    },
    error=function(e){
      message("Aborting. Reversing changes...")
      unlink(proj_name,recursive = TRUE)
      stop(e)
    })
  } else {
    message("Directory exists. Merging...")
    ## find common files that wont be overwritten.
    all_templates <- dir(system.file("extdata/EmptyProject",package="tidyproject"),include.dirs = TRUE,all.files = TRUE,recursive = TRUE)
    all_existing <- dir(proj_name,include.dirs = TRUE,all.files = TRUE,recursive = TRUE)
    
    merge_conf <- intersect(all_templates,all_existing)
    message("\n---Merge conflict on files/folders (will not replace)---:\n")
    message(paste(merge_conf,collapse = "\n"))
    message("")
    
    file.copy(file.path(system.file("extdata/EmptyProject",package="tidyproject"),"."),proj_name,recursive=TRUE,overwrite = FALSE)
    ## if the file is there don't overwrite.
    if(file.exists(file.path(proj_name,".Rprofile")))
      message(".Rprofile already present, creating Rprofile.R instead") else {
        result <- file.rename(file.path(proj_name,"Rprofile.R"),file.path(proj_name,".Rprofile"))
        if(!result) stop("something wrong with .Rprofile creation") else
          unlink(file.path(proj_name,"Rprofile.R"))
      }
    if(!project_library) {
      contents <- dir(file.path(proj_name,"ProjectLibrary"),include.dirs = TRUE,all.files = TRUE)
      contents <- contents[!contents %in% c(".","..")]
      contents <- contents[!grepl("Readme\\.txt$",contents)]
      if(length(contents)>0) stop("ProjectLibrary not empty. Will not delete. Rerun with project_library=TRUE")
      unlink(file.path(proj_name,"ProjectLibrary"),recursive = TRUE)
    }
  }
  if(getOption("git.exists")){
    currentwd <- getwd() ; on.exit(setwd(currentwd))
    setwd(proj_name)
    bare_proj_name <- gsub(basename(proj_name),paste0(basename(proj_name),".git"),proj_name)
    tryCatch({
      r <- git2r::init(".")
      if(!file.exists(".gitignore")){
        s <- unique(c(".Rproj.user",".Rhistory",".RData",getOption("git.ignore.files")))
        write(s,".gitignore")
      }
      paths <- unlist(git2r::status(r))
      if(length(git2r::reflog(r))==0){
        git2r::add(r, paths)
        git2r::config(r, user.name=Sys.info()["user"], user.email=getOption("user.email"))
        git2r::commit(r, "initialise_repository")
      }
    },
    error=function(e){
      setwd(currentwd)
      if(new_proj){
        message("Aborting. Reversing changes...")
        unlink(proj_name,recursive = TRUE)
        unlink(bare_proj_name,recursive = TRUE)
      }
      stop(e)
    })
  }
  message(paste("tidyproject directory ready:",proj_name))
  message("----------------------------------------------------")
  message("")
  message("INSTRUCTIONS:")
  message(paste("1. Open Rstudio project to start working: ",proj_name))
  if(project_library){
    message(paste("2. (optional) Install tidyproject package in project library"))
  }
}

#' create local bare repository
#' @param proj_name character vector indicating path to tidyproject
#' @export
make_local_bare <- function(proj_name=getwd()){
  currentwd <- getwd() ; on.exit(setwd(currentwd))
  setwd(proj_name)
  status <- git2r::status()
  if(length(status$untracked)>0) stop("untracked files detected. Create bare repositories manually.")
  if(length(status$unstaged)>0) stop("commit changes before continuing")
  proj_name_full <- getwd()
  bare_proj_name_full <- paste0(proj_name_full,".git")
  git2r::clone(proj_name_full,bare_proj_name_full,bare = TRUE)
  setwd("../")
  print("in make_local_bare")
  print(dir())
  res <- unlink(proj_name_full,recursive = TRUE)
  print(paste("res=",res))
  print(dir())
  git2r::clone(bare_proj_name_full,proj_name_full)
}