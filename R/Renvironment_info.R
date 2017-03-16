#' Make R session record
#'
#' Create a record of the R version and package versions used in a particular NMproject
#'
#' @export


Renvironment_info <- function() {
    check_if_tidyproject()
    scripts <- ls_scripts(scripts_dir())
    
    text <- lapply(scripts, readLines)
    text <- unlist(text)
    text <- parse(text = text)
    
    pkgs <- unique(unlist(lapply(text,recursive_lib_find)))
    
    txt <- c(paste0("Created at ", Sys.time(), " by ", Sys.info()["user"], "\n"))
    txt <- c(txt, utils::capture.output(utils::sessionInfo(package = pkgs)))
    writeLines(txt, "Renvironment_info.txt")
    message(paste0("Environment info produced: Renvironment_info.txt"))
    
}

recursive_lib_find <- function(x){
  if(is.name(x) || is.atomic(x)) {
    character()
  } else if(is.call(x)){
    lhs <- character()
    if(is.name(x[[1]])){
      if(identical(x[[1]],quote(library)) ||
         identical(x[[1]],quote(require)) ||
         identical(x[[1]],quote(loadNamespace)) ||
         identical(x[[1]],quote(requireNamespace)) ||
         identical(x[[1]],as.name("::")) ||
         identical(x[[1]],as.name(":::")))
        lhs <- as.character(x[[2]])
    }
    unique(c(lhs,unlist(lapply(x,recursive_lib_find))))
  } else if(is.pairlist(x)){
    unique(unlist(lapply(x,recursive_lib_find)))
  } else {
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

