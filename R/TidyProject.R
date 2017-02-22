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
#'   \item{Project structure: Defines standardised directory structure for statistical projects}
#'   \item{Long term reproducibility: Local (project level) installation of packages}
#'   \item{Code sharing/reuse: code library + search functionality}
#'  }
#' }
#'
#'
#' @section Options:
#' \code{scripts.dir} = names of the "scripts" directory in the project
#'
#' \code{models.dir} = names of the "models" directory in the project
#'
#' \code{git.exists} = \code{TRUE} if git is installed (only works on unix currently)
#'
#' \code{code_library_path} = character vector. paths of directories containing code files
#'
#' To modify these options use the options() interface.
#' To modify these values permanently set options() in ~/.Rprofile or R_HOME/etc/Rprofile.site
#'
#' @examples
#' \dontrun{
#' make_project("~/AZDXXXX/PKAE1")         ## creates new_project
#' code_library()                          ## display summary of code_library
#' preview("template1.R")                  ## preview file in code_library
#' copy_script("template1.R")              ## copy_script from code_library to project
#'
#' new_script("test.R")                    ## Creates empty test.R
#' copy_script("output.data.R")            ## copies from code_library
#' copy_script("../pathto/script.R")       ## copies from other location
#' }
#' @docType package
#' @name TidyProject

NULL

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

#' Test if directory is a TidyProject
#'
#' @param proj_path character vector indicating path to TidyProject
is_tidy_project <- function(proj_path = getwd()){
  file.exists(file.path(proj_path,getOption("scripts.dir"))) &
    file.exists(file.path(proj_path,getOption("models.dir"))) #&
  #normalizePath(get(".rs.getProjectDirectory")())==normalizePath(proj_path)
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
    all_templates <- dir(system.file("extdata/EmptyProject",package="TidyProject"),include.dirs = TRUE,all.files = TRUE,recursive = TRUE)
    all_existing <- dir(proj_name,include.dirs = TRUE,all.files = TRUE,recursive = TRUE)

    merge_conf <- intersect(all_templates,all_existing)
    message("\n---Merge conflict on files/folders (will not replace)---:\n")
    message(paste(merge_conf,collapse = "\n"))
    message("")

    file.copy(file.path(system.file("extdata/EmptyProject",package="TidyProject"),"."),proj_name,recursive=TRUE,overwrite = FALSE)
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

#' Create new R script
#' @param name character indicating name of script to create
#' @param overwrite logical. Whether to overwrite existing file (default = FALSE)
#' @param open_file logical. Whether function should open script (default = TRUE)
#' @export
new_script <- function(name,overwrite=FALSE,open_file=TRUE){ ## create black script with comment fields. Add new_script to git
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
  if(open_file) get("file.edit")(to.path)
}

## how to have multiple paths on the code_library
## Can add them in order to the search path

#' Create a recursive dependency_tree of file names
#'
#' @param from character. file name or path of file to copy
#' @param already_got logical. Default = NULL. If true will terminate loop
#'
#' Used by copy_script. Creates an an ordered vector of script dependencies.
dependency_tree <- function(from, ## a file name (full path or a script in current directory)
                            already_got=NULL){
  ## This is a difficult to understand function - it's recursive
  if(!file.exists(from)) stop("can't find \"from\" file")
  suppressWarnings(s0 <- readLines(from))
  depends.on <- s0[grepl("^[#; ]*Depends on: (.*)$",s0)]
  depends.on <- gsub("^[#; ]*Depends on: (.*)$","\\1",depends.on)
  if(length(depends.on)==0) return()
  depends.on <- strsplit(depends.on,",")[[1]]
  depends.on <- gsub("^\\s(.*)$","\\1",depends.on)
  if(!identical(depends.on,basename(depends.on))) stop("Can't read dependencies as full paths")
  if(length(intersect(depends.on,c(basename(from),already_got)))>0) stop("Circular dependency detected")
  ## recursively call function
  depends.on <- c(unlist(sapply(depends.on,function(i)dependency_tree(file.path(dirname(from),i),depends.on))),
                  depends.on)
  names(depends.on) <- NULL
  depends.on
}

#' copy_script
#'
#' Copies a script and dependencies from one location (will search code library) to scripts directory
#'
#' @param from character. file name or path of file to copy
#' @param to character. file name file to create
#' @param dependencies logical. Default = TRUE. will script copy dependencies
#' @param stamp_copy logical. Create a commented timestamp at beginning of file
#' @param overwrite logical. Overwrite "to" file if exists?
#' @param comment_char character. Comment character
#' @param alt_paths character vector. paths to other candidate files to search
#' @export
copy_script <- function(from,to,dependencies=TRUE,
                        stamp_copy=TRUE,overwrite=FALSE,comment_char="#",alt_paths){
  ## User function: copies script from one location (e.g. code_library) to project scripts directory
  if(missing(from)) stop("need \"from\" argument")
  onlyfrom <- missing(to)
  if(missing(to)) to <- basename(from)
  if(to!=basename(to)) stop("name must not be a path")
  to.path <- file.path(getOption("scripts.dir"),to)  ## destination path
  if(file.exists(to.path) & !overwrite) stop(paste(to.path, "already exists. Rerun with overwrite = TRUE"))

  use_code_library <- missing(alt_paths)

  if(onlyfrom) from_path <- locate_file(from,search_path = c()) else
    from_path <- locate_file(from,search_path = getOption("scripts.dir"))

  if(length(from_path)==0){ ## if file is not found directory or in scripts.dir
    ## look in code_library()

    if(use_code_library){
      ## define alt_locations to be code_library
      alt_paths <- code_library(viewer=FALSE,silent=TRUE,return_info = FALSE)
    }

    ## use grep to figure out how many matches.
    matches <- grepl(paste0(from,"$"),alt_paths)
    from0 <- from
    from <- alt_paths[matches]

    if(length(from)==0) stop(paste(from0,"not found"))
    if(length(from)>1 & use_code_library)
      stop("Matched more than one file with that name in code library.\n Try:\n  1) specifying full path OR\n  2) ensuring getOption(\"code_library_path\") points to non-overlapping directories")
    if(length(from)>1 & !use_code_library)
      stop("Matched more than one file with that name in alt_paths.\n Try specifying full path")
  }

  ## assume dependencies are in the same directory: dirname(from)
  ## dependencies should not be from current directory
  if(dependencies){
    depends.on <- dependency_tree(from)
    if(length(depends.on)>0) message("Copying dependencies...")
    for(i in depends.on) {
      if(file.exists(file.path(getOption("scripts.dir"),i))) message(paste("Dependency",file.path(getOption("scripts.dir"),i),"already exists. Will not overwrite")) else
        copy_script(file.path(dirname(from),i),dependencies=FALSE,alt_paths=alt_paths)
    }
  }
  suppressWarnings(s0 <- readLines(from))
  ## modify text at top of "from"
  if(dirname(from)==".") from.path <- file.path(getwd(),from) else from.path <- from
  if(stamp_copy) s <- c(paste0(comment_char,comment_char," Copied from ",normalizePath(from.path),"\n##  (",Sys.time(),") by ",Sys.info()["user"]),s0) else
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

ls_scripts <- function(folder=".",extn="r|R",recursive=TRUE){
  if(is.null(extn)){
    output <- dir(folder,recursive=recursive,full.names = TRUE)
  } else {
    file_match <- paste0("\\.(",extn,")$")
    output <- dir(folder,recursive=recursive,full.names = TRUE,pattern=file_match)
  }
  return(normalizePath(output))
}

#' List information about scripts
#'
#' @param files vector string of file names/paths
#' @param fields vector string of field tags to display
#' @param viewer logical indicating if Rstudio viewer should be used (default = TRUE)
#' @param silent run in quiet mode (default=FALSE)
#' @param base_dirs character vector. group files together that belong to these directory paths
#' @param shorten_paths logical. Default = TRUE. Long paths will be shortened if true in displayed output (not returned object)
#' @examples
#' \dontrun{
#' ls_scripts("~/AZD6094/PK_liver4/") %>%
#'   info_scripts("Description") %>%
#'   filter(grepl("mod",DESCRIPTION))
#' }
#' @export
info_scripts <- function(files,fields=c("Description","Keywords"),
                         viewer=FALSE,silent=FALSE,base_dirs=NULL,shorten_paths=TRUE){
  res <- lapply(files,function(file.name){ ## per file
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
  res <- do.call(rbind,res)

  d <- cbind(data.frame(FULL=normalizePath(files),
                        FOLDER=normalizePath(dirname(files)),
                        NAME=basename(files),stringsAsFactors = FALSE),res)

  if(!is.null(base_dirs)){
    base_dirs <- normalizePath(base_dirs)

    all_matches <- unlist(lapply(base_dirs,function(base_dir){
      grep(paste0("^",base_dir),d$FULL)
    }))

    if(length(unique(all_matches))!=length(all_matches)) stop("duplicate file matches found. Check base directories are not subsets of one another")

    for(base_dir in base_dirs){
      match_base <- grepl(paste0("^",base_dir),d$FULL)

      d$FOLDER[match_base] <- gsub(paste0("^(",base_dir,").*$"),"\\1",d$FULL[match_base])
      d$NAME[match_base] <- gsub(paste0("^",base_dir,.Platform$file.sep,"(.*)$"),"\\1",d$FULL[match_base])
    }
  }

  d <- cbind(data.frame(FOLDER=d$FOLDER,NAME=d$NAME,stringsAsFactors = FALSE),res)

  if(shorten_paths){
    ds <- cbind(data.frame(FOLDER=short_path(d$FOLDER),NAME=d$NAME,stringsAsFactors = FALSE),res)
  } else {
    ds <- d
  }

  if(!silent){
    if(viewer) get("View")(ds,"available files")# else print(ds)
  }
  invisible(d)
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

#' List files in code library
#'
#' @export

ls_code_library <- function(){
  ls_scripts(extn=".*",folder=getOption("code_library_path"),recursive=TRUE)
}

#' Show Code Library
#'
#' @param extn vector string of extensions to include
#' @param fields character vector of fields to extract
#' @param viewer logical indicating if viewer should be used to display results (default=FALSE)
#' @param silent logical indicating if messages should be silenced (default=FALSE)
#' @param return_info logical (default = FALSE). Return data.frame of results (FALSE= returns file paths)
#' @export
code_library <- function(extn="r|R",fields = "Description",viewer=TRUE,silent=FALSE,return_info=FALSE){
  if(is.null(getOption("code_library_path"))) {
    if(!silent){
      message("No directories attached. To attach add the following command:")
      message("  options(code_library_path=c(\"dir/of/scripts1\",\"dir/of/scripts2\",...))")
      message("     1. (for this session only) in the console")
      message("     2. (for this user) to ~/.Rprofile")
      message(paste0("     3. (for all users) to ",file.path(R.home(component = "home"), "etc", "Rprofile.site")))

      message(" 2. Attach for this user by putting command in ~/.Rprofile:")
    }
    return(data.frame())
  }

  files <- ls_code_library()
  file_match <- paste0("\\.(",extn,")$")
  files <- files[grepl(file_match,files)]

  if(viewer==FALSE & !return_info){
    return(files)
  }
  tryCatch({
    info <- info_scripts(files,fields = fields,viewer=viewer,silent=silent,base_dirs=getOption("code_library_path"))
  },error=function(e){
    if(grepl("duplicate file",e$message)) e$message <- paste0(e$message,".\n  Check getOption(\"code_library_path\") points to non-overlapping folders")
    stop(e)
  })
  if(!silent) message("\nNOTE: Do not source scripts from the code library,\n copy them to your project with copy_script")
  if(return_info){
    if(silent) return_ob <- invisible(info) else return_ob <- info
  } else {
    return_ob <- normalizePath(files)
  }
  if(viewer==FALSE) return(info)
  if(viewer==TRUE) return(invisible(files))

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

#' Preview code_library file
#' @param name character indicating script in code_library to preview
#' @export
preview <- function(name) {  ## preview files in code_library
  if(is_full_path(name)){
    if(!file.exists(name)) stop("file not found")
    file.show(name)
    return()
  }
  d <- code_library(viewer=FALSE,silent=TRUE,return_info = TRUE)
  if(!name %in% d$NAME) stop("file not found in code_library")
  if(length(which(d$NAME %in% name)) > 1) stop("Matched more than one file with that name.\n Try preview() again with full path")
  pos <- match(name,d$NAME)
  path <- file.path(d$FOLDER[pos],d$NAME[pos])
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
  return(character())
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

attach_code_library <- function(path){
  options("code_library_path"=unique(c(path,getOption("code_library_path"))))
}

#' Replaces code library
#'
#' Replace code library search path with path(s)
#'
#' @param path character vector with paths to attach to

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
  scripts <- scripts[!grepl("Renvironment_info\\.R$",scripts)]

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
  lib_statements3 <- unlist(strsplit(lib_statements3,"[^a-zA-Z:]",perl = TRUE))
  lib_statements3 <- lib_statements3[grepl("^.*\\w+::\\w+.*$",lib_statements3)]
  lib_statements3 <- gsub("^.*\\b(\\w+)\\s*::.*$","\\1",lib_statements3)
  lib_statements3 <- gsub("\\s","",lib_statements3)
  lib_statements3 <- unique(lib_statements3)
  if(length(lib_statements3)>0) lib_statements3 <- paste0("library(",lib_statements3,")")

  lib_statements <- c(lib_statements1,lib_statements2,lib_statements3)

  script <- c(
    "## Sourcing this script generate a sessionInfo for project",
    "## It will load all packages referenced in the scripts library",
    "## ",
    "",
    "## Load all packages ",
    lib_statements,
    "",
    "txt <- c(paste0(\"Created at \",Sys.time(),\" by \",Sys.info()[\"user\"],\"\\n\"))",
    "",
    "###### devtools::session_info output ######",
    "#txt <- c(txt,\"###### devtools::session_info output ######\")",
    "#x <- devtools::session_info(include_base = TRUE)",
    "#x$platform$running <- sessionInfo()$running",
    "#txt <- c(txt,capture.output(print(x)))",
    "",
    "###### utils::sessionInfo output ######",
    "txt <- c(txt,capture.output(sessionInfo()))",
    "",
    "writeLines(txt, \"Renvironment_info.txt\")"
  )

  write(script,file=file.path(scripts.dir,"Renvironment_info.R"))
  suppressMessages(setup_file(file.path(scripts.dir,"Renvironment_info.R")))
  message(paste0("Script produced:           ",file.path(getOption("scripts.dir"),"Renvironment_info.R")))
  source(file.path(getOption("scripts.dir"),"Renvironment_info.R"))
  message(paste0("Environment info produced: Renvironment_info.txt"))

}


# Renvironment_info <- function(){
#   #if(!requireNamespace("packrat", quietly = TRUE)) stop("function requires packrat to be installed")
#
#   dependent_packages <- dirDependencies(".")
#   sessionInfo(package = dependent_packages)
#
#   txt <- c(paste0("Created at ",Sys.time()," by ",Sys.info()["user"],"\n"))
#   txt <- c(txt,capture.output(sessionInfo()))
#   writeLines(txt, "Renvironment_info.txt")
#
#   message(paste0("Environment info produced: Renvironment_info.txt"))
#
# }


#' shorten path name
#'
#' @param x character vector. Path to shorten.
short_path <- function(x){
  split_paths <- strsplit(x,.Platform$file.sep)#[[1]]
  short_paths <- lapply(split_paths,function(split_path){
    if(length(split_path)>5) split_path.short <- c(split_path[1:3],"..",split_path[(length(split_path)-1):length(split_path)]) else split_path.short <- split_path
    do.call(file.path,as.list(split_path.short))
  })
  unlist(short_paths)
}

#' rate my code
#'
#' @param script_file character. script file to assess
#' @export

#rate_my_code <- function(script_file){
#  message("Nothing much here. Under construction")
#}

#' rate my code
#' @export

#rate_my_project <- function(proj_name=getwd()){
#  is_tidy_project
#}
