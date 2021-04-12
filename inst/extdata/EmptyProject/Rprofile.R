#####################################
### Begin tidyproject config

if (file.exists(file.path("~", ".Rprofile"))) source(file.path("~", ".Rprofile"))

####################################################
### Set library strategy here
## possible values for lib:
## project, project-user, user, global)
lib <- "project-user"
####################################################

## define copy of proj_lib from tidyproject source
proj_lib <- function(base_dir = "."){
  R_version <- paste0(R.version$major, ".", tools::file_path_sans_ext(R.version$minor))
  base_proj_lib <- file.path(base_dir, "ProjectLibrary")#, R_version)
  base_contents <- dir(base_proj_lib, full.names = TRUE)
  base_dirs <- base_contents[file.info(base_contents)$isdir]
  base_dirs <- basename(base_dirs)
  ## no version dirs
  base_dirs <- base_dirs[!grepl("^[0-9\\.]+$", base_dirs)]
  
  proj_lib_v <- file.path("ProjectLibrary", R_version)
  proj_lib_v_full <- file.path(base_dir, proj_lib_v)
  
  if(file.exists(proj_lib_v_full)) {
    proj_lib <- proj_lib_v
  } else {
    if(length(base_dirs) == 0)
      proj_lib <- proj_lib_v else
        proj_lib <- "ProjectLibrary"
  }
  proj_lib
}

pl <- proj_lib()
dir.create(pl, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(pl,.libPaths()))

## set default library config strategy
## define copy of toggle_libs from tidyproject source
toggle_libs <- function(lib = c("project","project-user","user","global")){
  current_lib_paths <- normalizePath(.libPaths(), winslash = "/")
  new_lib_paths <- current_lib_paths
  
  current_wd <- normalizePath(getwd(), winslash = "/")
  
  ## identify project/user/global libs
  match_project_libs <- grepl(current_wd, current_lib_paths)
  project_lib_pos <- which(match_project_libs)
  project_libs <- current_lib_paths[match_project_libs]
  project_lib_present <- length(project_libs) > 0
  default_project_lib <- proj_lib() #normalizePath(list.files(pattern = "ProjectLibrary", full.names = TRUE))
  
  default_user_lib <- normalizePath(Sys.getenv("R_LIBS_USER"), mustWork = FALSE, winslash = "/")
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

suppressMessages({
  toggle_libs(lib)
})

rm(list = ls())

### End tidyproject config
#####################################

## Code written below here will execute each time R starts in this project
