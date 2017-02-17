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
      if(new_proj){
        message("Aborting. Reversing changes...")
        unlink(proj_name,recursive = TRUE)
        unlink(bare_proj_name,recursive = TRUE)
      }
      stop(e)
    })
  }
  message(paste("TidyProject directory ready:",proj_name))
  message("----------------------------------------------------")
  message("")
  message("INSTRUCTIONS:")
  message(paste("1. Open Rstudio project to start working: ",proj_name))
  if(project_library){
    message(paste("2. (optional) Install TidyProject package in project library"))
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
new_script <- function(name,overwrite=FALSE,silent=FALSE){ ## create black script with comment fields. Add new_script to git
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
  if(!silent) file.edit(to.path)
}

## how to have multiple paths on the code_library
## Can add them in order to the search path

#' Create a recursive dependency_tree of file names
#'
#' Used by copy_script. Creates an an ordered vector of script dependencies.
dependency_tree <- function(from, ## a file name (full path or a script in current directory)
                            already.got=NULL){
  ## This is a difficult to understand function - it's recursive
  if(!file.exists(from)) stop("can't find \"from\" file")
  suppressWarnings(s0 <- readLines(from))
  depends.on <- s0[grepl("^[#; ]*Depends on: (.*)$",s0)]
  depends.on <- gsub("^[#; ]*Depends on: (.*)$","\\1",depends.on)
  if(length(depends.on)==0) return()
  depends.on <- strsplit(depends.on,",")[[1]]
  depends.on <- gsub("^\\s(.*)$","\\1",depends.on)
  if(!identical(depends.on,basename(depends.on))) stop("Can't read dependencies as full paths")
  if(length(intersect(depends.on,c(basename(from),already.got)))>0) stop("Circular dependency detected")
  ## recursively call function
  depends.on <- c(unlist(sapply(depends.on,function(i)dependency_tree(file.path(dirname(from),i),depends.on))),
                  depends.on)
  names(depends.on) <- NULL
  depends.on
}

#' copy_script
#' @export
copy_script <- function(from,to,dependencies=TRUE,stamp.copy=TRUE,overwrite=FALSE,comment_char="#"){
  ## User function: copies script from one location (e.g. code_library) to project scripts directory
  onlyfrom <- missing(to)
  if(missing(to)) to <- basename(from)
  if(to!=basename(to)) stop("name must not be a path")
  to.path <- file.path(getOption("scripts.dir"),to)  ## destination path
  if(file.exists(to.path) & !overwrite) stop(paste(to.path, "already exists. Rerun with overwrite = TRUE"))

  if(onlyfrom){
    search_path <- getOption("code_library_path")
  } else {
    search_path <- c(getOption("scripts.dir"),
                     getOption("code_library_path"))
  }

  from <- locate_file(from,search_path = search_path)

  ## assume dependencies are in the same directory: dirname(from)
  ## dependencies should not be from current directory
  if(dependencies){
    depends.on <- dependency_tree(from)
    if(length(depends.on)>0) message("Copying dependencies...")
    for(i in depends.on) {
      if(file.exists(file.path(getOption("scripts.dir"),i))) message(paste("Dependency",file.path(getOption("scripts.dir"),i),"already exists. Will not overwrite")) else
        copy_script(file.path(dirname(from),i),dependencies=FALSE)
    }
  }
  suppressWarnings(s0 <- readLines(from))
  ## modify text at top of "from"
  if(dirname(from)==".") from.path <- file.path(getwd(),from) else from.path <- from
  if(stamp.copy) s <- c(paste0(comment_char,comment_char," Copied from ",normalizePath(from.path),"\n##  (",Sys.time(),") by ",Sys.info()["user"]),s0) else
    s <- s0
  writeLines(s,to.path)
  setup_file(to.path)
}

#' List scripts
#'
#' @param folder string describing folder to search recursively in
#' @param extn character (can be regex) giving extension to limit search to
#' @param recursive by default TRUE
#' @examples
#' \dontrun{
#' ls_scripts("~/AZD6094/PK_liver4/") %>%
#'   info_scripts("Description") %>%
#'   filter(grepl("mod",DESCRIPTION))
#' }
#' @export

ls_scripts <- function(folder=".",extn="r|R|mod",recursive=TRUE){
  file.name <- paste0("\\.(",extn,")$")
  dir0 <- dir(folder,recursive=recursive)
  file.path(gsub("^(.*[^/])/*$","\\1",folder),dir0[grepl(file.name,dir0)])
}

#' List information about scripts
#'
#' @param files vector string of file names/paths
#' @param fields vector string of field tags to display
#' @param viewer logical indicating if Rstudio viewer should be used (default = TRUE)
#' @param silent run in quiet mode (default=FALSE)
#' @examples
#' \dontrun{
#' ls_scripts("~/AZD6094/PK_liver4/") %>%
#'   info_scripts("Description") %>%
#'   filter(grepl("mod",DESCRIPTION))
#' }
#' @export
info_scripts <- function(files,fields=c("Description","Keywords"),viewer=FALSE,silent=FALSE){
  res <- plyr::ldply(files,function(file.name){ ## per file
    suppressWarnings({
      s <- readLines(file.name)
      ## make data.frame
      ## e.g. Description Keywords
      #           XXXX      YYYY
      field.vals <- as.data.frame(lapply(fields,function(field){
        field <- gsub(paste0("^.*",field,": (.*)$"),"\\1",s[grepl(paste0("^.*",field,": "),s)])
        if(length(field)==0) field <- NA
        field <- field[1]  ## in case multiple, take only first
        as.character(field)
      }))
      names(field.vals) <- fields
    })
    field.vals
  })
  d <- cbind(data.frame(FOLDER=short_path(dirname(files)),NAME=basename(files)),res)
  if(!silent){
    if(viewer) View(d,"available files") else print(d)
  }
  invisible(cbind(data.frame(FOLDER=dirname(files),NAME=basename(files)),res))
}

#' Search for string in files
#'
#' @param files vector string of file names/paths
#' @param text string (can be regex) to search for
#' @export

search_scripts <- function(files,text){
  res <- unlist(sapply(files,function(file.name){
    suppressWarnings(s <- readLines(file.name))
    if(suppressWarnings(length(grep(text,s))==0)) return(NULL) else return(file.name)
  }))
  names(res) <- NULL
  res
}

#' Show Code Library
#'
#' @param extn vector string of extensions to include
#' @param fields character vector of fields to extract
#' @param viewer logical indicating if viewer should be used to display results (default=FALSE)
#' @param silent logical indicating if results should be return silently (default=FALSE)
#' @export
code_library <- function(extn="r|R",fields = "Description",viewer=FALSE,silent=FALSE) {
  if(is.null(getOption("code_library_path"))) {
    message("No directories attached. Attach with attach_code_library(\"path/to/dir/of/scripts\")")
    return(data.frame())
  }
  files <- ls_scripts(extn,folder=getOption("code_library_path"),
                      recursive=TRUE)
  info_scripts(files,fields = fields,viewer=viewer,silent=silent)
}


#' Preview code_library file
#' @param name character indicating script in code_library to preview
#' @export
preview <- function(name) {  ## preview files in code_library
  d <- code_library(viewer=FALSE,silent=TRUE)
  if(is.numeric(name)) path <- file.path(d$FOLDER[name],d$NAME[name]) else {
    if(!name %in% d$NAME) stop("file not found in code_library")
    pos <- match(name,d$NAME)
    path <- file.path(d$FOLDER[pos],d$NAME[pos])
  }
  file.show(path)
}


#' Locate file from search path
#'
#' Finds first file in search_path that exists
#' @param x string for file name
#' @param search_path vector of strings giving search path
#' @return Path of located file.  Returns error if file not found.
#' @examples
#' \dontrun{
#' locate_file("script.R",c(".","Scripts")) ## looks in current working directory, then Scripts folder
#' }
locate_file <- function(x,search_path=c(".")){
  ## internal function: locate_file from an ordered vector of directories
  x0 <- x
  if(file.exists(x0)) return(x0)
  for(dir in search_path){
    x <- normalizePath(file.path(dir,basename(x0)))
    if(file.exists(x)) return(x)
  }
  stop(paste(x0,"file not found"))
}


#' Display code library search path
#'
#' @export

code_library_path <- function() getOption("code_library_path")

#' Attach code library
#'
#' Attaches a path(s) to to the code library search path
#'
#' @param path character vector with paths to attach to
#' @export

attach_code_library <- function(path){
  options("code_library_path"=unique(c(path,getOption("code_library_path"))))
}

#' Replaces code library
#'
#' Replace code library search path with path(s)
#'
#' @param path character vector with paths to attach to
#' @export

replace_code_library <- function(path){
  options("code_library_path"=unique(path))
}


#' Make R session record
#'
#' Create a record of the R version and package versions used in a particular NMproject
#'
#' @export
Renvironment_info <- function(){

  scripts.dir <- getOption("scripts.dir")
  scripts <- ls_scripts(scripts.dir)

  text <- lapply(scripts,readLines)
  text <- unlist(text)

  lib_statements1 <- text[grep("\\blibrary\\([^)]+\\)",text)]
  lib_statements1 <- gsub(".*(library\\([^)]+\\)).*","\\1",lib_statements1)
  ## remove reasons why lib_statements1 may be different (e.g. spaces)
  lib_statements1 <- gsub("\\s","",lib_statements1)
  lib_statements1 <- gsub("\\\\","",lib_statements1) ## no statement should have \\
  lib_statements1 <- unique(lib_statements1)

  lib_statements2 <- text[grep("\\brequire\\([^)]+\\)",text)]
  lib_statements2 <- gsub(".*(require\\([^)]+\\)).*","\\1",lib_statements2)
  ## remove reasons why lib_statements2 may be different (e.g. spaces)
  lib_statements2 <- gsub("\\s","",lib_statements2)
  lib_statements2 <- gsub("\\\\","",lib_statements2) ## no statement should have \\
  lib_statements2 <- unique(lib_statements2)

  lib_statements3 <- text[grep("\\w+\\s*::",text)]
  lib_statements3 <- gsub(".*[ (+-](\\w+)\\s*::.*","\\1",lib_statements3)
  lib_statements3 <- gsub("\\s","",lib_statements3)
  lib_statements3 <- unique(lib_statements3)
  if(length(lib_statements3)>0) lib_statements3 <- paste0("library(",lib_statements3,")")

  lib_statements <- c(lib_statements1,lib_statements2,lib_statements3)

  script <- c(
    "## This script will generate a sessionInfo for project",
    "",
    "## Load all packages ",
    lib_statements,
    "",
    "txt <- c(paste0(\"## Created at \",Sys.time(),\" by \",Sys.info()[\"user\"],\"\\n\"))",
    "txt <- c(txt,capture.output(sessionInfo()))",
    "writeLines(txt, \"RenvironmentInfo.txt\")"
  )

  write(script,file=file.path(scripts.dir,"RenvironmentInfo.R"))
  suppressMessages(setup_file(file.path(scripts.dir,"RenvironmentInfo.R")))
  message(paste0("Script produced:           ",file.path(getOption("scripts.dir"),"RenvironmentInfo.R")))
  source(file.path(getOption("scripts.dir"),"RenvironmentInfo.R"))
  message(paste0("Environment info produced: RenvironmentInfo.txt"))

}


#' shorten path name
short_path <- function(x){
  ans <- strsplit(x,.Platform$file.sep)[[1]]
  if(length(ans)>5) ans.short <- c(ans[1:3],"..",ans[(length(ans)-1):length(ans)]) else ans.short <- ans
  do.call(file.path,as.list(ans.short))
}

#' rate my code
#' @export

rate_my_code <- function(){
  stop("under construction")
}
