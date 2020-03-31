copy_empty_project <- function(proj_name,
                               lib_strategy = c("project-user","project","user","global"),
                               overwrite_rprofile=FALSE){
  lib_strategy <- match.arg(lib_strategy)
  .Rprofile_name <- normalizePath(file.path(proj_name, ".Rprofile"),winslash = "/",mustWork = FALSE)
  Rprofile.R_name <- normalizePath(file.path(proj_name, "Rprofile.R"),winslash = "/",mustWork = FALSE)
  
  file.copy2(file.path(system.file("extdata/EmptyProject", package = "tidyproject"), 
                       "."), proj_name, recursive = TRUE, overwrite = FALSE)
  if(!file.exists(.Rprofile_name)){
    result <- file.rename(Rprofile.R_name,.Rprofile_name)
    if(!result) stop("unable to create project config file")
  } else {
    existing_lines <- readLines(.Rprofile_name)
    if(any(grepl("ProjectLibrary",existing_lines))){
      if(!overwrite_rprofile){
        stop("Existing ProjectLibrary setup lines found in ",
             .Rprofile_name,
             "\n  Remove and then try again ",
             "\n  Or run again with overwrite_rprofile=TRUE", call. = FALSE)
      } else {
        unlink(.Rprofile_name,force = TRUE)
      }
    }
    new_lines <- readLines(Rprofile.R_name)
    cat(paste0("\n",new_lines),file = .Rprofile_name,append = TRUE)
  }
  config_lines <- readLines(.Rprofile_name)
  
  config_lines <- gsub("project-user", lib_strategy, config_lines)
  
  #config_lines <- gsub("^(\\.remove_user_lib <- )\\S*(.*)$",
  #                     paste0("\\1",remove_user_lib,"\\2"),
  #                     config_lines)
  
  write(config_lines,file=.Rprofile_name)
}

## like file.copy, but only for non binaries 
file.copy2 <- function(from, to, overwrite = FALSE, recursive = FALSE){
  dest <- file.path(to, basename(from))
  for(i in seq_along(from)){
    if(file.info(from[i])$isdir){
      if(overwrite | !file.exists(dest[i])) dir.create(dest[i], showWarnings = FALSE)
      next_list_files <- list.files(from[i], full.names = TRUE)
      if(length(next_list_files) > 0 & recursive)
        file.copy2(from = next_list_files, to = dest[i], recursive = TRUE)
    } else {
      if(overwrite | !file.exists(dest[i])) {
        file.create(dest[i])
        write(readLines(from[i]), file = dest[i])
      }
    }
  }
}

#' Load local package
#'
#' This function loads code stored in localpackage/R and data stored in
#' localpackage/data.  It is a wrapper around devtools::document() and
#' devtools::load_all().  See their documentation for more information.
#'
#' @param ... arguments to be passed to devtools::load_all
#' @param fail_silently logical. default FALSE. Should function fail silently
#' @export

load_localpackage <- function(..., fail_silently = FALSE){
  if(!file.exists("localpackage")) {
    if(!fail_silently){
      stop("localpackage does not exist.
Ensure you are in a tidyproject.
It's possible you created this project before the localpackages were implemented.
In this case, run make_project(\".\") to create the localpackage", call. = FALSE)
    } else return(invisible(FALSE))
  }
  if(!requireNamespace("devtools",quietly = TRUE)) stop("devtools package required", call. = FALSE)
  suppressWarnings(suppressMessages({
    devtools::document("localpackage")
  }))
  devtools::load_all("localpackage", ...)
  return(invisible(TRUE))
}


#' Create new_project
#'
#' Creates directory structure.  User install tidyproject again in
#'
#' @param proj_name character string of full path to new_project
#' @param lib_strategy character either missing, "project","project-user","user",or "global"
#' @param overwrite_rprofile logical. should project .Rprofile be overwritten (default=FALSE)
#'
#' @export
make_project <- function(proj_name, lib_strategy = c("project-user","project","user","global"),
                         overwrite_rprofile = FALSE) {
    ## must be full path.  User function: create new_project
    new_proj <- !file.exists(proj_name)
    if (new_proj) {
        tryCatch({
            message("Directory doesn't exist. Creating...")
            dir.create(proj_name)
            copy_empty_project(proj_name = proj_name,
                               lib_strategy = lib_strategy,
                               overwrite_rprofile = overwrite_rprofile)
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
        copy_empty_project(proj_name=proj_name,lib_strategy = lib_strategy,
                           overwrite_rprofile = overwrite_rprofile)
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
    
    proj_name

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

#' get project library location
#' @export
proj_lib <- function(){
  R_version <- paste0(R.version$major, ".", tools::file_path_sans_ext(R.version$minor))
  file.path("ProjectLibrary", R_version)
}

#' toggle library settings
#' @param lib character either missing, "project","project-user","user",or "global"
#' @export

toggle_libs <- function(lib = c("project","project-user","user","global")){
  current_lib_paths <- normalizePath(.libPaths())
  new_lib_paths <- current_lib_paths
  
  current_wd <- normalizePath(getwd())
  
  ## identify project/user/global libs
  match_project_libs <- grepl(current_wd, current_lib_paths)
  project_lib_pos <- which(match_project_libs)
  project_libs <- current_lib_paths[match_project_libs]
  project_lib_present <- length(project_libs) > 0
  default_project_lib <- proj_lib #normalizePath(list.files(pattern = "ProjectLibrary", full.names = TRUE))
  
  default_user_lib <- normalizePath(Sys.getenv("R_LIBS_USER"), mustWork = FALSE)
  match_user_lib <- grepl(default_user_lib, current_lib_paths)
  user_lib_pos <- which(match_user_lib)
  user_lib_present <- any(grepl(default_user_lib, current_lib_paths))
  default_user_lib_present <- file.exists(default_user_lib)
  
  move_to_front <- function(vec, indices) vec[c(indices, setdiff(seq_along(vec), indices))]
  move_to_back <- function(vec, indices) vec[c(setdiff(seq_along(vec), indices), indices)]
  
  global_lib_pos <- setdiff(seq_along(new_lib_paths), c(project_lib_pos,user_lib_pos))
  global_lib_present <- length(global_lib_pos) > 0
  
  if(missing(lib)){
    if(1 %in% project_lib_pos){
      if(2 %in% user_lib_pos) {
        message("library setting: project-user (reproducibility level: medium)")
        message("packages installed now will be specific to this project and R version")
        return(invisible("project-user")) }
      else {
        message("library setting: project (reproducibility level: high)")
        message("packages installed now will be specific to this project and R version")
        return(invisible("project"))
      }
    }
    if(1 %in% user_lib_pos) {
      message("library setting: user (reproducibility level: low)")
      message("packages installed now will be specific to you as a user and any project with a project-user config")
      return(invisible("user"))
    }
    if(1 %in% global_lib_pos) {
      message("library setting: global (reproducibility level: very low)")
      message("packages installed now will apply to all users and projects")
      return(invisible("global"))
    }
    ## else
    stop("cannot determine library structure")
  }
  
  lib <- match.arg(lib)
  
  user_front_if_exists <- function(){
    match_user_lib <- grepl(default_user_lib, new_lib_paths)
    user_lib_pos <- which(match_user_lib)
    if(user_lib_present){
      new_lib_paths <- move_to_front(new_lib_paths, user_lib_pos)
    }
    new_lib_paths
  }
  
  user_back_if_exists <- function(){
    match_user_lib <- grepl(default_user_lib, new_lib_paths)
    user_lib_pos <- which(match_user_lib)
    if(user_lib_present){
      new_lib_paths <- move_to_back(new_lib_paths, user_lib_pos)
    }
    new_lib_paths
  }
  
  user_front <- function(){
    match_user_lib <- grepl(default_user_lib, new_lib_paths)
    user_lib_pos <- which(match_user_lib)
    if(user_lib_present){
      new_lib_paths <- move_to_front(new_lib_paths, user_lib_pos)
    } else if(default_user_lib_present){
      new_lib_paths <- c(default_user_lib, new_lib_paths)
    } else stop("can't find user library")  
    new_lib_paths
  }
  
  project_front <- function(){
    match_project_libs <- grepl(current_wd, new_lib_paths)
    project_lib_pos <- which(match_project_libs)
    if(project_lib_present){
      new_lib_paths <- move_to_front(new_lib_paths, project_lib_pos)
    } else if(file.exists(default_project_lib)) { 
      new_lib_paths <- c(default_project_lib, new_lib_paths)
    } else stop("can't find a project library")    
    new_lib_paths
  }
  
  project_back_if_exists <- function(){
    match_project_libs <- grepl(current_wd, new_lib_paths)
    project_lib_pos <- which(match_project_libs)
    if(project_lib_present){
      new_lib_paths <- move_to_back(new_lib_paths, project_lib_pos)
    }
    new_lib_paths
  }

  if(lib %in% "project"){
    new_lib_paths <- user_back_if_exists()
    new_lib_paths <- project_front()
  }
  
  
  if(lib %in% "project-user"){
    new_lib_paths <- user_front_if_exists()
    new_lib_paths <- project_front()
  }
  
  if(lib %in% "user"){
    new_lib_paths <- project_back_if_exists()
    new_lib_paths <- user_front()
  }
  
  if(lib %in% "global"){
    if(length(project_lib_pos) + length(user_lib_pos) == length(new_lib_paths))
      stop("can't find global libraries")
    new_lib_paths <- project_back_if_exists()
    new_lib_paths <- user_back_if_exists()
  }

  .libPaths(new_lib_paths)
  toggle_libs()
  
}

#' stage files in project staging area ready for import
#' 
#' @param files character vector. path of files to stage
#' @param destination default empty.  Optional destination directory
#'  by default will be equivalent location in staging/
#' @param additional_sub_dirs character vector. additional subdirectories 
#'  not in standard tidyproject structure
#' @param overwrite logical (default = FALSE).
#' @param silent logical (default = FALSE)
#' @export
stage <- function(files, destination, additional_sub_dirs = c(),
                  overwrite = FALSE, silent = FALSE){
  
  ## send unmodified files into staging area for importation
  
  files <- normalizePath(files)
  
  ##########################  
  sub_dirs <- c("SourceData",
                "DerivedData",
                "localpackage",
                "Scripts",
                "Models",
                tidyproject::models_dir(),
                "Results",
                additional_sub_dirs)
  
  sub_dirs <- basename(sub_dirs)
  
  sub_dirs <- unique(sub_dirs)
  
  key_dirs <- sub_dirs
  
  regex_key_dirs <- paste0("\\b", key_dirs, "\\b")
  
  files_sep <- strsplit(files, .Platform$file.sep)
  
  if(!missing(destination)){
    if(!grepl("staging", destination))
      stop("destination should be in staging area", call. = FALSE)
    destination <- file.path(destination, basename(files))
    destination <- relative_path(destination, "staging")
  } else {
    destination <- sapply(files_sep, function(file_sep){
      file_sep <- rev(file_sep)
      matches <- match(key_dirs, file_sep)
      if(all(is.na(matches))) return(NA_character_)
      matched_dir <- key_dirs[which.min(matches)]
      file_sep <- file_sep[seq_len(match(matched_dir, file_sep))]
      do.call(file.path, as.list(rev(file_sep)))
    })
  }
  
  d <- tibble::tibble(from = files, destination)
  d$staging <- file.path("staging", d$destination)
  
  d <- d[!is.na(d$destination), ]
  dir_names <- unique(dirname(d$staging))
  for(dir_name in dir_names) dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  
  existing_files <- d$staging[file.exists(d$staging)]
  do_copy <- rep(TRUE, nrow(d))  ## default = copy
  if(!overwrite & length(existing_files)){
    #stop("File(s) already exist:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE", call. = FALSE)
    if(!silent) message("File(s) not to be overwritten:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE")
    #d <- d[!d$staging %in% existing_files, ]
    do_copy[file.exists(d$staging)] <- FALSE
  }
  
  file.copy(d$from[do_copy],
            d$staging[do_copy],
            overwrite = overwrite)
  
  if(!silent) message("File(s) staged in project:\n",paste(paste0(" ",d$staging[do_copy]),collapse="\n"), "\nTo import use import()")
  
  invisible(d)
  
}

#' Import staged files into project
#' 
#' @param copy_table data frame output from stage()
#' @param overwrite logical (default = FALSE)
#' @param silent logical (default = FALSE)
#' @export
import <- function(copy_table, overwrite = FALSE, silent = FALSE){
  
  ## import the files_to_copy
  
  ## R scripts in Scripts to be copied with the stamp at the top
  ## Code in Models/. not to be copied - this will be handled by nm() %>% ctl("staging/...")
  ## everything else copied as is
  
  copy_table <- copy_table[!is.na(copy_table$destination), ]
  ## skip everything in Models 
  copy_table <- copy_table[!grepl(paste0("^", getOption("models.dir"), .Platform$file.sep), copy_table$destination), ]
  
  copy_table$extn <- tools::file_ext(copy_table$destination)
  
  d_R <- copy_table[copy_table$extn %in% c("r", "R"), ]
  d_other <- copy_table[!copy_table$extn %in% c("r", "R"), ]
  
  existing_files <- c(
    d_R$destination[file.exists(d_R$destination)],
    d_other$destination[file.exists(d_other$destination)]
  )
  if(!overwrite & length(existing_files) > 0){
    #stop("File(s) already exist:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE", call. = FALSE)
    if(!silent) message("File(s) not to be overwritten:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing project files or use overwrite=TRUE")
    copy_table <- copy_table[!copy_table$destination %in% existing_files, ]
  }
  
  copy_script2(d_R$staging, d_R$destination, overwrite = overwrite)
  file.copy(d_other$staging, d_other$destination, overwrite = overwrite)  ## use copy_file instead?
  
  message("Files imported:\n ",
          paste(copy_table$destination, collapse = "\n "))
  
  invisible()
  
}
