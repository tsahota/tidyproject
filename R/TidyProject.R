#' Project and code management
#'
#' TidyProject is designed to manage directories and aid in readability and reproducibility
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
#' new_project("~/AZDXXXX/PKAE1")          ## creates new_project
#' code_library()                          ## display summary of code_library
#' copy_script("nm.log.R")                 ## copy_script from code_library to project
#' preview("nm.log.R")                     ## preview file in code_library
#'
#' copy_control("ADVAN2.mod","run1.mod")   ## copy template from code_library
#' copy_control("run1.mod","run2.mod")     ## create run2.mod from run1.mod
#' new_script("test.R")                    ## Creates empty test.R
#' copy_script("output.data.R")            ## copies from code_library
#' copy_script("../pathto/script.R")       ## copies from other location
#' }
#' @docType package
#' @name TidyProject

NULL

## Names:
##  TidyProject
##  TidyCode
##  TidyLittleProject
##  StructuredProject

## Description
## Three components 1) ProjectTemplate 2) packrat 3) code_library 4) script creations stuff
##  Project template but simpler directory structure.
##  Packrat but option for local install vs global install.
##  Definition of an external code_library
##   Search and copying functionality
##  new_code_file, copy_code_file
##

## It can refer to a code.library.
## make a search path for this.
## You can store this anywhere and build it up.

## The code library is external - this is easier to understand.  And not that bad
##  The code will not directly use anything that's not in the library.

## Make a code_library_search_path: this will search all the library code.

## NMproject comes with it's own library.  This is installed in the package
##  Can be modified by the user via a configuration file.
##  Important that NMproject and other packages are installed locally.

## PROBLEM: what if NMproject is no longer compatible with TidyProject?
##  Can we install TidyProject locally too?
##  Needs to be a project info object/settings - can look into ProjectTemplate

## How to install a local package?
##  install.packages(,lib.loc="ProjectLibrary")
##  library(...,lib="ProjectLibrary")

## Downsides:
##  a bit annoying to have to do.  Could I make a wrapper script?

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
}

validate_session <- function(){
  result <- TRUE
  message("working directory is Rstudio project directory...",appendLF = FALSE)
  if(.rs.getProjectDirectory()!=getwd()) message("TRUE") else
    message("FALSE") ; result <- FALSE
  message("working directory is Rstudio project directory...",appendLF = FALSE)
  if(file.exists(getOption("scripts.dir"))) message("TRUE") else
    message("FALSE") ; result <- FALSE
  return(result)
}

install_project_packages <- function(...,lib){
  if(!missing(lib)) stop("forbidden to change lib with install.local.packages()")
  if(!file.exists("ProjectLibrary") %in% dir()) stop("ProjectLibrary not detected")
  install.packages(...,lib="ProjectLibrary")
}

project_library <- function(...){
  .rs.loadPackage(...,lib="ProjectLibrary")
}

#' Setup files
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
#' @param proj.name character string of full path to new_project
#' @export
new_project <- function(proj.name){ ## must be full path.
  ## User function: create new_project
  if(!is_full_path(proj.name)) stop("Need absolute path")
  if(file.exists(proj.name)) stop("project already exists")
  currentwd <- getwd() ; on.exit(setwd(currentwd))
  dir.create(proj.name)
  tryCatch({
    file.copy(file.path(getOption("nmproject.src"),"inst","extdata","CodeLibrary","EmptyProject/."),proj.name,recursive=TRUE) ## copy empty project template
    if(!TRUE %in% file.info(proj.name)$isdir) stop(paste(proj.name,"not created")) # Test if directory exists
    ## Go into newly create project and do some configuring
    setwd(proj.name)
  },
  error=function(e){
    setwd(currentwd)
    message("Something wrong. Reversing changes...")
    unlink(proj.name,recursive = TRUE)
    stop(e)
  })
  if(getOption("git.exists")){
    tryCatch({
      r <- git2r::init(".")
      s <- unique(c(".Rproj.user",".Rhistory",".RData","sdtab*","catab*","patab*","cotab*",
                    "run[a-zA-Z0-9]*\\.[^m]*[^o]*[^d]*",getOption("git.ignore.files")))
      write(s,".gitignore")
      paths <- unlist(git2r::status(r))
      git2r::add(r, paths)
      git2r::config(r, user.name=user(), user.email=getOption("user.email"))
      git2r::commit(r, "initialise_repository")
      ## make local bare repository
      bare.proj.name <- gsub(basename(proj.name),paste0(basename(proj.name),".git"),proj.name)
      proj.name.full <- getwd()
      bare.proj.name.full <- paste0(proj.name.full,".git")
      git2r::clone(proj.name.full,bare.proj.name.full,bare = TRUE)
      setwd("../")
      unlink(proj.name,recursive = TRUE)
      git2r::clone(bare.proj.name.full,proj.name.full)
    },
    error=function(e){
      setwd(currentwd)
      message("Something wrong. Reversing changes...")
      unlink(proj.name,recursive = TRUE)
      unlink(bare.proj.name,recursive = TRUE)
      stop(e)
    })
  }
  message("----------------------------------------------------")
  message(paste("1. Open Rstudio project: ",file.path(proj.name,"Analysis.Proj")))
  message(paste0("2. From project: library(NMproject,lib=\"ProjectLibrary\")"))
}

#' create bare repository
#'


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
