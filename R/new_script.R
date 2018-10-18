#' Create new R script
#' @param name character indicating name of script to create
#' @param overwrite logical. Whether to overwrite existing file (default = FALSE)
#' @param open_file logical. Whether function should open script (default = TRUE)
#' @param libs character. What libraries to add.
#' @export
new_script <- function(name, overwrite = FALSE, open_file = TRUE, libs=c("tidyproject")) {
    ## create black script with comment fields. Add new_script to git
    check_if_tidyproject()
    if (name != basename(name)) 
        stop("name must not be a path")
    to_path <- file.path(scripts_dir(), name)  ## destination path
    if (file.exists(to_path) & !overwrite) 
        stop(paste(to_path, "already exists. Rerun with overwrite = TRUE"))
    s <- c(paste0("## ", "Author: ", Sys.info()["user"]), paste0("## ", "First created: ", 
        Sys.Date()), paste0("## ", "Description: "), paste0("## ", 
        "Keywords: "), "", "########################################", "## load packages and source functions here", 
        "", paste0("library(",libs,")"), "", "########################################", 
        "## main script here", "")
    writeLines(s, to_path)
    setup_file(to_path)
    if (open_file) 
        get("file.edit")(to_path)
}

## how to have multiple paths on the code_library Can add them in order to the search
## path

#' Create a recursive dependency_tree of file names
#'
#' @param from character. file name or path of file to copy
#' @param already_got logical. Default = NULL. If true will terminate loop
#'
#' Used by copy_script. Creates an an ordered vector of script dependencies.
dependency_tree <- function(from, already_got = NULL) {
    ## This is a difficult to understand function - it's recursive
    if (!file.exists(from)) 
        stop("can't find \"from\" file")
    suppressWarnings(s0 <- readLines(from))
    s0 <- parse(text=s0)
    calling_env <- environment()
    depends.on <- unique(unlist(lapply(s0,depends_find)))
    if (length(depends.on) == 0) 
        return()
    if (!identical(depends.on, basename(depends.on))) 
        stop("Can't read dependencies as full paths")
    if (length(intersect(depends.on, c(basename(from), already_got))) > 0) 
        stop("Circular dependency detected")
    ## recursively call function
    depends.on <- c(unlist(sapply(depends.on, function(i) dependency_tree(file.path(dirname(from), 
        i), depends.on))), depends.on)
    names(depends.on) <- NULL
    depends.on
}

depends_find <- function(x){
  if(is.name(x) || is.atomic(x)) {
    character()
  } else if(is.call(x)){
    lhs <- character()
    if(is.name(x[[1]])){
      if(identical(x[[1]],quote(source)))
        lhs <- basename(as.character(x[[2]]))
    }
    unique(c(lhs,unlist(lapply(x,depends_find))))
  } else if(is.pairlist(x)){
    unique(unlist(lapply(x,depends_find)))
  } else {
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

#' Copy script to project directory
#'
#' Will search code library and copy script and dependencies into scripts directory.
#' Script will also be stamped with source location, time and user information
#'
#' @param from character. file name or path of file to copy
#' @param to character. file name file to create
#' @param dependencies logical. Default = TRUE. will script copy dependencies
#' @param stamp_copy logical. Create a commented timestamp at beginning of file
#' @param overwrite logical. Overwrite 'to' file if exists?
#' @param comment_char character. Comment character
#' @param alt_paths character vector. paths to other candidate files to search
#' @param proj_path character. Default = current working directory. path to tidyproject
#' @export
copy_script <- function(from, to, dependencies = TRUE, stamp_copy = TRUE, overwrite = FALSE, 
    comment_char = "#", alt_paths, proj_path = ".") {
    ## User function: copies script from one location (e.g. code_library) to project
    ## scripts directory
    check_if_tidyproject(proj_path)
    if (missing(from)) 
        stop("need \"from\" argument")
    onlyfrom <- missing(to)
    if (missing(to)) 
        to <- basename(from)
    if (to != basename(to)) 
        stop("name must not be a path")
    to_path <- file.path(scripts_dir(proj_path), to)  ## destination path
    if (file.exists(to_path) & !overwrite) 
        stop(paste(to_path, "already exists. Rerun with overwrite = TRUE"))
    
    use_code_library <- missing(alt_paths)
    
    if (onlyfrom) 
        from_path <- locate_file(from, search_path = NULL) else from_path <- locate_file(from, search_path = scripts_dir(proj_path))
    
    if (length(from_path) == 0) {
        ## if file is not found directory or in scripts.dir
        if (use_code_library) 
            alt_paths <- getOption("code_library_path")
        
        from_path <- locate_file(from, search_path = alt_paths, recursive = TRUE)
        
        if (length(from_path) == 0) 
            stop(paste(from, "not found"))
        if (length(from_path) > 1 & use_code_library) 
            stop("Matched more than one file with that name in code library.\n Try:\n  1) specifying full path OR\n  2) ensuring getOption(\"code_library_path\") points to non-overlapping directories")
        if (length(from_path) > 1 & !use_code_library) 
            stop("Matched more than one file with that name in alt_paths.\n Try specifying full path")
    }
    from <- from_path
    ## assume dependencies are in the same directory: dirname(from_path) dependencies
    ## should not be from_path current directory
    if (dependencies) {
        depends.on <- dependency_tree(from_path)
        if (length(depends.on) > 0) 
            message("Copying dependencies...")
        for (i in depends.on) {
            if (file.exists(file.path(scripts_dir(proj_path), i))) 
                message(paste("Dependency", file.path(getOption("scripts.dir"), i), "already exists. Will not overwrite")) else copy_script(file.path(dirname(from_path), i), dependencies = FALSE, 
                alt_paths = alt_paths)
        }
    }
    suppressWarnings(s0 <- readLines(from_path))
    ## modify text at top of 'from_path'
    if (stamp_copy) 
        s <- c(paste0(comment_char, comment_char, " Copied from ", from_path, "\n##  (", 
            Sys.time(), ") by ", Sys.info()["user"]), s0) else s <- s0
    writeLines(s, to_path)
    setup_file(to_path)
}

#' Copy file to project directory
#'
#' Will copy any file from an external location (e.g. code library) into project
#' No modification of that file will take place
#'
#' @param from character. file name or path of file to copy
#' @param dest character. file name file to create
#' @param overwrite logical. Overwrite 'to' file if exists?
#' @param alt_paths character vector. paths to other candidate files to search
#' @param version_control logical. Should file be added to version control (default = FALSE)
#' @export

copy_file <- function(from, dest, overwrite = FALSE, alt_paths, version_control=FALSE) {
    ## dest is the location direcdestry
    if (missing(from)) 
        stop("need \"from\" argument")
    dest_path <- normalizePath(dest, winslash = "/", mustWork = FALSE)
    
    use_code_library <- missing(alt_paths)
    
    from_path <- locate_file(from, search_path = NULL)
    
    if (length(from_path) == 0) {
        ## if file is not found directory or directory
        if (use_code_library) 
            alt_paths <- getOption("code_library_path")
        
        from_path <- locate_file(from, search_path = alt_paths, recursive = TRUE)
        
        if (length(from_path) == 0) 
            stop(paste(from, "not found"))
        if (length(from_path) > 1 & use_code_library) 
            stop("Matched more than one file with that name in code library.\n Try:\n  1) specifying full path OR\n  2) ensuring getOption(\"code_library_path\") points to non-overlapping directories")
        if (length(from_path) > 1 & !use_code_library) 
            stop("Matched more than one file with that name in alt_paths.\n Try specifying full path")
    }
    file.copy(from_path, dest_path, overwrite = overwrite)
    setup_file(dest_path,version_control=version_control)
}

copy_directory <- function(){
  
}


detect_type <- function(x){
  
  ## detect type of x
  ##   if x = directory do copy_directory
  ##   if x = Rscript
  ##   if x = NM control file
  
}

copy_code <- function(x){
  
  type <- detect_type(x)
   
  ## Apply copy_x to it:
  ##  read in required contents of x
  ##  transform if necessary
  ##  return (list of) file contents with paths
  
  if(type %in% "Rscript"){
    #res <- get_contents_Rscript(x)
  }
  if(type %in% "unknown"){
    #res <- get_contents_ascii(x)
  }
  
  ## write and setup

  #setup_file(file.path(dest_path,basename(from_path)),version_control=version_control)
  
}


#' Locate file from search path
#'
#' Finds first file in search_path that exists
#' @param x string for file name
#' @param search_path vector of strings giving search path
#' @param recursive logical. Default TRUE. whether to do recusive search or not
#' @return Path of located file.  Returns error if file not found.
#'
#' @export
#' @examples
#' \dontrun{
#' locate_file('script.R',c('.','Scripts')) ## looks in current working directory, then Scripts folder
#' }
locate_file <- function(x, search_path = c("."), recursive = FALSE) {
    ## internal function: locate_file from an ordered vector of directories
    if (!is.null(x)) 
        if (file.exists(x)) 
            return(normalizePath(x, winslash = "/"))
    all_files <- unlist(lapply(search_path, function(dir) {
        x <- list.files(path = dir, all.files = TRUE, full.names = TRUE, recursive = recursive)
        if (length(x) > 0) 
            return(normalizePath(x, winslash = "/")) else return(character())
    }))
    all_files[grepl(paste0(x, "$"), all_files)]
}
