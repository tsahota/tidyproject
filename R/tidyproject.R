#' @importFrom magrittr %>%
#' @export
magrittr::'%>%'

#' @importFrom magrittr %<>%
#' @export
magrittr::'%<>%'

#' @importFrom magrittr %T>%
#' @export
magrittr::'%T>%'

#' @importFrom magrittr %$%
#' @export
magrittr::'%$%'

#' @importFrom rlang .data
#' @export
rlang::.data



#' Project and code management
#'
#' tidyproject is designed to manage tidy project directories and
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
#' \code{scripts.dir} = names of the 'scripts' directory in the project
#'
#' \code{models.dir} = names of the 'models' directory in the project
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
#' make_project('~/AZDXXXX/PKAE1')         ## creates new_project
#' code_library()                          ## display summary of code_library
#' preview('template1.R')                  ## preview file in code_library
#' copy_script('template1.R')              ## copy_script from code_library to project
#'
#' new_script('test.R')                    ## Creates empty test.R
#' copy_script('output.data.R')            ## copies from code_library
#' copy_script('../pathto/script.R')       ## copies from other location
#' }
#' @docType package
#' @name tidyproject

NULL

#' Set project options
set_project_opts <- function() {
  ## Internal function: will set all global variables put as much AZ specific code in
  ## here.
  if (is.null(getOption("scripts.dir"))) 
    options(scripts.dir = "Scripts")
  if (is.null(getOption("models.dir"))) 
    options(models.dir = "Models")
  if (is.null(getOption("git.exists"))) 
    options(git.exists = requireNamespace("git2r", quietly = TRUE))
  if(is.null(getOption("system_cmd"))) options(system_cmd=function(cmd,...) {
    if(.Platform$OS.type == "windows") shell(cmd,...) else system(cmd,...)
  })
  if (is.null(getOption("git.command.line.available"))) 
    options(git.command.line.available = FALSE)
  if (is.null(getOption("git.ignore.files"))) 
    options(git.ignore.files = c(""))
  if (getOption("git.exists") & is.null(getOption("user.email"))) 
    options(user.email = "user@example.org")
}

#' Shortcut to Scripts directory
#' 
#' @param proj_name character. Working directory (default = getwd())
#' @export
scripts_dir <- function(proj_name = getwd()) {
  normalizePath(file.path(proj_name, getOption("scripts.dir")), winslash = "/", mustWork = FALSE)
}

#' recursively get all script dirs
#' 
#' @param path character. Script dirs are in a subdirectory n levels below
#' @param dir_name character (default = "Scripts"). name of scripts dirs
#' @param depth integer. number of levels to search
#' @param maxdepth integer. max number of levels to search
#' @export
#' 
ls_script_dirs <- function(path = ".", dir_name = getOption("scripts.dir"), 
                           depth, maxdepth = 10) {
  
  path <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  script_paths <- path[basename(path) %in% dir_name]
  if(maxdepth > 1){
    if(missing(depth)) {
      if(length(script_paths) == 0) path <- ls_script_dirs(path, dir_name, 
                                                           maxdepth = maxdepth -1)
    } else {
      if(depth > 1) path <- ls_script_dirs(path, dir_name, 
                                           depth = depth - 1, maxdepth = maxdepth - 1) 
    }    
  }
  
  script_paths <- path[basename(path) %in% dir_name]
  normalizePath(script_paths, winslash = "/")
}

#' Shortcut to Models directory
#' 
#' @param proj_name character. Working directory (default = getwd())
#' @export
models_dir <- function(proj_name = getwd()) {
  normalizePath(file.path(proj_name, getOption("models.dir")), winslash = "/", mustWork = FALSE)
}

#' Check if directory is a tidyproject
#' 
#' @param directory character. working directory (default = current working directory)
#' @param return_logical logical. If true will return FALSE instead of error
#' @export
check_if_tidyproject <- function(directory = getwd(),return_logical=FALSE) {
  test <- is_tidyproject(directory)
  if (!test & !return_logical) {
    stop("working directory not a tidyproject base directory")
  } else return(test)
}

is_tidyproject <- function(directory = getwd()) {
  file.exists(scripts_dir(directory)) & file.exists(models_dir(directory)) 
}

#' Reset working directory to rstudio working directory
#' 
#' @export
resetwd <- function(){
  if(exists(".rs.getProjectDirectory")){
    setwd(get(".rs.getProjectDirectory")())
  } else stop("not in rstudio")
}

#' Setup files
#' 
#' This function should not be used directly. Only exported for use by dependent packages
#' 
#' @param file_name character. Indicating name of file to set up
#' @param version_control logical. Should file be added to version control
#' @export
setup_file <- function(file_name,version_control=getOption("git.exists")) {
  check_if_tidyproject()
  #Sys.chmod(file_name, mode = "744")  ## 744= read-write-executable for user, read only for others
  
  if(grepl(file.path(getOption("models.dir"), ".+"),dirname(file_name)))
    version_control <- FALSE
  
  if (version_control) {
    commit_file(file_name)
    Sys.sleep(0.1)
  } else message(paste(file_name, "created"))
}

#' commit individual file(s)
#' 
#' Has side effect that staged changed will be updated to working tree
#'
#' @param file_name character vector. File(s) to be committed
#' @export
#' @examples 
#' \dontrun{
#' commit_file("Scripts/script1.R")
#' }
commit_file <- function(file_name){
  repo <- git2r::repository(".")
  
  old_staged_files <- git2r::status(repo)
  old_staged_files <- unlist(old_staged_files$staged)
  
  if(length(old_staged_files) > 0) {
    on.exit(git2r::add(repo, path = old_staged_files))
    git2r::reset(repo, path = old_staged_files)
  }
  
  git2r::add(repo,path = file_name)
  new_staged_files <- git2r::status(repo)
  new_staged_files <- unlist(new_staged_files$staged)
  if(length(new_staged_files) == 0){
    if(getOption("git.command.line.available")){
      for(f in file_name) system_cmd(paste("git add",f))
      new_staged_files <- git2r::status(repo)
      new_staged_files <- unlist(new_staged_files$staged)
      if(length(new_staged_files) == 0){
        #nothing to commit. file already commited
        return(invisible())
      }
    } else {
      #Skipping adding files to repo: git2r failed. Do it manually if needed
      return(invisible())
    }
  }
  git2r::commit(repo,message = paste("snapshot:", paste(file_name, collapse = ",")))
}

#' system/shell command wrapper
#'
#' @param cmd character. command to send to shell
#' @param dir character. directory to run command in
#' @param ... other arguments passed to system command
#' @export
system_cmd <- function(cmd,dir=".",...){
  if(!dir %in% ".") if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    stop(paste0("Directory \"",dir,"\" doesn't exist."))
  getOption("system_cmd")(cmd,...)
}

#' Test if full path
#'
#' @param x string giving file/path name
#' @return TRUE only when path starts with ~, /, \\ or X: (i.e. when x is a full path), FALSE otherwise
#' @examples
#' \dontrun{
#' is_full_path('file.text.ext')
#' is_full_path('/path/to/file.text.ext')
#' }


is_full_path <- function(x) grepl("^(~|/|\\\\|([a-zA-Z]:))", x, perl = TRUE)


#' Check tidyproject for best practice compliance
#'
#' @param proj_name character. default = current working directory. path to directory.
#' @param silent logical. default = FALSE. suppress messages or not
#' @param check_rstudio logical (default = FALSE). Check rstudio studio project directory
#' @export

check_session <- function(proj_name = getwd(), silent = FALSE, check_rstudio = TRUE) {
  drstudio.exists <- FALSE
  if (check_rstudio & exists(".rs.getProjectDirectory")) {
    drstudio.exists <- FALSE
    drstudio <- do_test("rstudio working dir = current working dir" =
    {
      res <- normalizePath(get(".rs.getProjectDirectory")(), winslash = "/") == 
        normalizePath(".", winslash = "/")
      if (res) 
        return(TRUE) else return(paste0(FALSE, ": switch to main dir"))
    }, silent = silent)
    if (grepl("FALSE",drstudio$result_char[drstudio$test == "rstudio working dir = current working dir"])) 
      stop("FAILED: working directory should be current working directory. setwd() is discouraged")
  }
  
  d <- do_test("directory is a tidyproject"=
                 is_tidyproject(proj_name),
               "contains environment_info" = {
                 files <- list.files(proj_name, pattern = "environment_info.*txt")
                 length(files) > 0
               },
               "up to date environment_info" = {
                 files <- list.files(proj_name, pattern = "environment_info.*txt")
                 if (length(files) == 0) return("no environment_info")
                 d <- file.info(files)
                 d$file_name <- row.names(d)
                 d <- d[order(d$mtime, decreasing = ), ]
                 script_times <- file.info(ls_scripts(scripts_dir(proj_name)))$mtime
                 if (length(script_times) == 0) 
                   return("No scripts")
                 envir_time <- d$mtime[1]
                 time_diff <- difftime(envir_time, max(script_times))
                 result <- time_diff >= 0
                 if (result) 
                   return(TRUE)
                 paste0(result, ": ", signif(time_diff, 2), " ", attr(time_diff, "units"))
               }, "Project library setup" = {
                 if (file.exists(file.path(proj_name, "ProjectLibrary"))) {
                   res <- normalizePath(file.path(proj_name, "ProjectLibrary"), winslash = "/") == 
                     normalizePath(.libPaths()[1], winslash = "/")
                   return(res)
                 } else return(paste0(FALSE, ": no project library"))
               }, "Description fields present" = {
                 field_val_test("Description")
               }, "Author fields present" = {
                 field_val_test("Author")
               },
               silent = silent)
  if (drstudio.exists) 
    d <- rbind(drstudio, d)
  invisible(d)
}

field_val_test <- function(field_name,
                           directory=getOption("scripts.dir"),
                           extension="R"){
  dir0 <- dir(directory,full.names = TRUE)
  dir0 <- dir0[tools::file_ext(dir0) %in% extension]
  field_vals <- lapply(dir0,get_script_field,field_name = field_name)
  names(field_vals) <- basename(dir0)
  field_vals <- unlist(field_vals)
  empty_field_vals <- names(field_vals[field_vals%in%""])
  if(length(empty_field_vals)>0)
    return(paste("FALSE:",paste(empty_field_vals,collapse=","))) else
      return(TRUE)
}

#' Get field from a code file
#' 
#' @param file_name character. path to file
#' @param field_name character. name of field to find.
#' @param n numeric. number of lines of each file to search
#' @export
get_script_field <- function(file_name,field_name,n = 10){
  script <- readLines(file_name,n = n)
  field <- gsub(paste0("^.*",field_name,"s*:\\s*(.*)$"), "\\1",
                script[grepl(paste0("^.*", field_name, "s*:\\s*"), script,ignore.case = TRUE)],
                ignore.case = TRUE)
  if(length(field)==0) field <- ""
  field
}

#' Testing interfact
#' 
#' @param ... named expression to evaluate
#' @param silent logical. Default = FALSE. Should messages be printed.
#' @param append data.frame. Default = empty. Existing set of results to append to
#' @export
do_test <- function(..., silent = FALSE,append=data.frame()) {
  x <- match.call(expand.dots = FALSE)$...
  par_env <- parent.frame()
  eval_x <- unlist(lapply(x, function(i) eval(i,par_env)))
  
  ## check outputs
  lengths <- sapply(eval_x, length)
  if (any(lengths != 1)) 
    stop("Following tests not return single value:\n",
         paste(names(eval_x)[lengths != 1]))
  
  if (!silent) 
    for (test_name in names(eval_x)) {
      message(substr(paste(test_name, paste(rep(".", 600), collapse = "")),
                     1, 50),appendLF = FALSE)
      message(eval_x[[test_name]])
    }
  
  d <- data.frame(test = names(eval_x),
                  result_char = as.character(unlist(eval_x)),
                  result_logical=as.logical(unlist(eval_x)),
                  stringsAsFactors = FALSE)
  row.names(d) <- NULL
  d <- rbind(append,d)
  invisible(d)
}

#' Wait for statement to be true
#'
#' @param x expression to evaluate
#' @param timeout numeric. Maximum time (seconds) to wait
#' @param interval numeric. Number of seconds (default=1s) to wait till rechecking
#' @export
wait_for <- function(x,timeout=NULL,interval=1){
  x <- substitute(x)
  start.time <- Sys.time()
  diff.time <- 0
  while (!eval(x,envir = parent.frame())){
    diff.time <- difftime(Sys.time(),start.time,units="secs")
    if(!is.null(timeout))if(diff.time > timeout) {
      warning(paste("timed out waiting for\n",x,sep=""))
      return(invisible(FALSE))
    }
    Sys.sleep(1)
  }
  invisible(TRUE)
}

#' Logical flag for detecting if R session is on rstudio not
#' @export
is_rstudio <- function() Sys.getenv("RSTUDIO") == "1"

#' Git commit of ctl files, SourceData and Scripts
#'
#' @param message character. Description to be added to commit
#' @param session logical. Should sessionInfo be included in commit message
#' @param ... additional arguments for git2r::commit
#' @export

snapshot <- function(message = "created automatic snapshot", session = TRUE, ...){
  
  files_to_stage <- c(file.path(getOption("scripts.dir"),"*"),
                       file.path("SourceData","*"))
  
  code_snapshot_files(message = message, session = session, files_to_stage = files_to_stage, ...)
  
}

#' Git commit of ctl files, SourceData and Scripts
#'
#' This function should not be used directly.
#' 
#' @param message character. Description to be added to commit
#' @param session logical. Should sessionInfo be included in commit message
#' @param files_to_stage character vector. file paths to add to commit
#' @param ... additional arguments for git2r::commit
#' @export

code_snapshot_files <- function(message = "created automatic snapshot", session = TRUE, files_to_stage, ...){
  
  ## 'all' doesn't seem to work in git2r::commit, so add the 
  repo <- git2r::repository(".")
  git2r::add(repo, files_to_stage)
  
  files <- git2r::status(repo)
  new_unstaged_files <- unlist(files$unstaged)
  new_staged_files <- unlist(files$staged)
  
  if(length(c(new_staged_files,new_unstaged_files)) == 0){
    message("nothing to commit")
    return(invisible())
  }
  git2r::commit(repo, message = message, all = TRUE, session = session, ...)
  message("Committed changes to: ctl files, scripts, and source data")
}

#' List directories
#' 
#' Wrapper around list.dirs() but includes maxdepth and pattern arguments
#'  and removes full.names argument, always return full names.
#' 
#' @param path same as list.dirs()
#' @param full.names same as list.dirs()
#' @param recursive same as list.dirs()
#' @param maxdepth integer (default = 1) maximum depth to search
#' @param pattern character (default = missing) regex pattern match on directory name
#' 
#' @export
list_dirs <- function(path = ".", full.names = TRUE, recursive = FALSE, maxdepth = 1, pattern){
  
  dirs <- list()
  dirs[[1]] <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  
  missing_pattern <- missing(pattern)
  
  return_ready <- function(dirs){ ## prep for return character vector
    dirs <- unlist(dirs)
    if(!missing_pattern) dirs <- dirs[grepl(pattern, basename(dirs))]
    if(!full.names) dirs <- basename(dirs)
    dirs
  }
  
  if(maxdepth == 1 | !recursive) return(return_ready(dirs))
  
  for(i in 2:maxdepth){
    current_dirs <- sapply(dirs[[i-1]], function(path){
      list.dirs(path, full.names = TRUE, recursive = FALSE)
    })
    current_dirs <- unlist(current_dirs)
    names(current_dirs) <- NULL
    
    ## breakpoint if no more dirs
    if(length(current_dirs) == 0) return(return_ready(dirs))
    
    dirs[[i]] <- current_dirs
  }
  
  return(return_ready(dirs))
  
}
