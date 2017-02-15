#' Project and code management
#'
#' TidyProject is designed to manage tidy project directories and
#' facilitate reproducibility and reuse of code
#'
#' @section Strategy:
#'
#' \itemize{
#'  \item Code management
#'  \itemize{
#'   \item{Project structure: Defines standardised directory structure for NONMEM projects}
#'   \item{Code: Automates and enforces standards on control comments
#'   \itemize{
#'    \item Beginning of code: Description, author, date,...
#'    \item $THETA/$OMEGA/$SIGMA: Parameter names, units, transformations)}
#'   }
#'   \item{Long term reproducibility: Local (project level) installation of packages}
#'   \item{Code sharing/reuse: editable model library + search functionality}
#'  }
#' }
#'
#' Compatible with git - optional
#' #'
#' @section Options:
#' \code{scripts.dir} = names of the "scripts" directory in the project
#'
#' \code{models.dir} = names of the "models" directory in the project
#'
#' \code{git.exists} = \code{TRUE} if git is installed (only works on unix currently)
#'
#'
#' @examples
#' \dontrun{
#' make_project("~/AZDXXXX/PKAE1")         ## creates new_project
#' code_library()                          ## display summary of code_library
#' copy_script("nm.log.R")                 ## copy_script from code_library to project
#' preview("nm.log.R")                     ## preview file in code_library
#'
#' new_script("test.R")                    ## Creates empty test.R
#' copy_script("output.data.R")            ## copies from code_library
#' copy_script("../pathto/script.R")       ## copies from other location
#' }
#' @docType package
#' @name TidyProject

NULL

## Names:
##  tidyproject (change to this)
##  TidyProject

## Description
## Three components 1) ProjectTemplate 2) packrat 3) code_library 4) script creations stuff
##  Project template but simpler directory structure.
##  Packrat but option for local install vs global install.
##  Definition of an external code_library
##   Search and copying functionality
##  new_code_file, copy_code_file

## It can refer to a code.library.
## make a search path for this.
## You can store this anywhere and build it up.

## The code library is external - this is easier to understand.  And not that bad
##  The code will not directly use anything that's not in the library.

## Make a code_library_search_path: this will search all the library code.

## NMproject comes with it's own library.  This is installed in the package
##  Can be modified by the user via a configuration file.
##  Important that NMproject and other packages are installed locally.

#' Set project options
set_project_opts <- function(){
  ## Internal function: will set all global variables
  ## put as much AZ specific code in here.
  #if(is.null(getOption("code_library.loc"))) options(code_library.loc= system.file("extdata/CodeLibrary",package = "TidyProject"))
  if(is.null(getOption("code_library_paths"))) options(code_library_paths=c(""))
  if(is.null(getOption("scripts.dir"))) options(scripts.dir="Scripts")
  if(is.null(getOption("models.dir"))) options(models.dir="Models")
  if(is.null(getOption("git.exists"))) options(git.exists=requireNamespace("git2r", quietly = TRUE))
  if(is.null(getOption("git.ignore.files"))) options(git.ignore.files=c(""))
  if(getOption("git.exists") & is.null(getOption("user.email"))) options(user.email="user@example.org")
}

#' Validate TidyProject session
#' @param fail_on_error logical (default=FALSE) indicating whether function should give error on a test fail
validate_session <- function(fail_on_error=FALSE){
  result <- TRUE
  msg <- function(...) if(fail_on_error) stop(...) else message(...)

  # if(!.rs.getProjectDirectory()==getwd()){
  #   message("Working directory is not Rstudio project directory")
  #   result <- FALSE
  # }

  if(!file.exists(getOption("scripts.dir"))){
    message("Directory getOption(\"scripts.dir\") not found")
    result <- FALSE
  }

  if(!file.exists(getOption("models.dir"))){
    message("Directory getOption(\"models.dir\") not found")
    result <- FALSE
  }
  if(!result) msg("TidyProject session validation failed.")
  return(invisible(result))
}

#' Test if directory is a TidyProject
#'
#' @param proj_path character vector indicating path to TidyProject
is_tidy_project <- function(proj_path = getwd()){
  file.exists(file.path(proj_path,getOption("scripts.dir"))) &
    file.exists(file.path(proj_path,getOption("models.dir")))
}

#' Setup files
#' @param file.name character indicating name of file to set up
setup_file <- function(file.name){
  ## Internal function: routine procedures after creating a file.
  Sys.chmod(file.name,mode = "744")  ## 744= read-write-executable for user, read only for others
  if(getOption("git.exists")) {
    git2r::add(git2r::repository("."),file.name)
    #system_cmd(paste("git add",file.name))    ## add to git repository. Need git installed
    message(paste(file.name,"added to git"))
  } else message(paste(file.name,"created"))
}

#' Create new_project
#'
#' Creates directory structure.  User install TidyProject again in
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
      file.copy(file.path(system.file("extdata/EmptyProject",package="TidyProject"),"."),proj_name,recursive=TRUE)
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
    all_templates <- dir(system.file("extdata/EmptyProject",package="TidyProject"),include.dirs = TRUE,all.files = TRUE,recursive = TRUE)
    all_existing <- dir(proj_name,include.dirs = TRUE,all.files = TRUE,recursive = TRUE)

    merge_conf <- intersect(all_templates,all_existing)
    message("\n---Merge conflict on files/folders (will not replace)---:\n")
    message(paste(merge_conf,collapse = "\n"))
    message("")

    file.copy(file.path(system.file("extdata/EmptyProject",package="TidyProject"),"."),proj_name,recursive=TRUE,overwrite = FALSE)
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
      if(length(git2r::reflog(r,verbose = FALSE))==0){
        git2r::add(r, paths)
        git2r::config(r, user.name=Sys.info()["user"], user.email=getOption("user.email"))
        git2r::commit(r, "initialise_repository")
      }
    },
    error=function(e){
      setwd(currentwd)
      message("Aborting. Reversing changes...")
      unlink(proj_name,recursive = TRUE)
      unlink(bare_proj_name,recursive = TRUE)
      stop(e)
    })
  }
  message(paste("TidyProject directory ready:",proj_name))
  message("----------------------------------------------------")
  message("")
  message("INSTRUCTIONS:")
  message(paste("1. Open Rstudio project to start working: ",proj_name))
  if(project_library){
    message(paste("2. (recommended) Install TidyProject package in project library"))
  }
}

#' create local bare repository
#' @param proj_name character vector indicating path to TidyProject
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
  unlink(proj_name,recursive = TRUE)
  git2r::clone(bare_proj_name_full,proj_name_full)
}

#' Test if full path
#'
#' @param x string giving file/path name
#' @return TRUE only when path starts with ~, /, \\ or X: (i.e. when x is a full path), FALSE otherwise
#' @examples
#' \dontrun{
#' is_full_path("file.text.ext")
#' is_full_path("/path/to/file.text.ext")
#' }

is_full_path <- function(x) grepl("^(~|/|\\\\|([a-zA-Z]:))",x,perl=TRUE)

#' Create new R script
#' @param name character indicating name of script to create
#' @param overwrite logical indicating whether to overwrite existing file (default=FALSE)
#' @export
new_script <- function(name,overwrite=FALSE){ ## create black script with comment fields. Add new_script to git
  if(name!=basename(name)) stop("name must not be a path")
  to.path <- file.path(getOption("scripts.dir"),name)  ## destination path
  if(file.exists(to.path) & !overwrite) stop(paste(to.path, "already exists. Rerun with overwrite = TRUE"))
  s <- c(paste0("## ","Author: ",Sys.info()["user"]),
         paste0("## ","First created: ",Sys.Date()),
         paste0("## ","Description: "),
         paste0("## ","Depends on: "),
         paste0("## ","Keywords: "),
         "","########################################",
         "## load packages and source functions here","",
         paste0("library(TidyProject)"),
         "","########################################",
         "## main script here","")
  writeLines(s,to.path)
  setup_file(to.path)
  file.edit(to.path) ## open file
}

