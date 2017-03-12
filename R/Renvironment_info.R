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
    
    lib_statements1 <- text[grep("\\blibrary\\([^)]+\\)", text)]
    lib_statements1 <- gsub(".*(library\\([^)]+\\)).*", "\\1", lib_statements1)
    ## remove reasons why lib_statements1 may be different (e.g. spaces)
    lib_statements1 <- gsub("\\s", "", lib_statements1)
    lib_statements1 <- gsub("\\\\", "", lib_statements1)  ## no statement should have \\
    lib_statements1 <- unique(lib_statements1)
    
    lib_statements2 <- text[grep("\\brequire\\([^)]+\\)", text)]
    lib_statements2 <- gsub(".*(require\\([^)]+\\)).*", "\\1", lib_statements2)
    ## remove reasons why lib_statements2 may be different (e.g. spaces)
    lib_statements2 <- gsub("\\s", "", lib_statements2)
    lib_statements2 <- gsub("\\\\", "", lib_statements2)  ## no statement should have \\
    lib_statements2 <- unique(lib_statements2)
    
    lib_statements3 <- text[grep("\\w+\\s*::", text)]
    lib_statements3 <- unlist(strsplit(lib_statements3, "[^a-zA-Z:]", perl = TRUE))
    lib_statements3 <- lib_statements3[grepl("^.*\\w+::\\w+.*$", lib_statements3)]
    lib_statements3 <- gsub("^.*\\b(\\w+)\\s*::.*$", "\\1", lib_statements3)
    lib_statements3 <- gsub("\\s", "", lib_statements3)
    lib_statements3 <- unique(lib_statements3)
    if (length(lib_statements3) > 0) 
        lib_statements3 <- paste0("library(", lib_statements3, ")")
    
    lib_statements <- c(lib_statements1, lib_statements2, lib_statements3)
    
    lib_statements
    pkgs <- gsub("library\\((.*)\\)", "\\1", lib_statements)
    pkgs <- gsub("require\\((.*)\\)", "\\1", pkgs)
    
    txt <- c(paste0("Created at ", Sys.time(), " by ", Sys.info()["user"], "\n"))
    txt <- c(txt, utils::capture.output(utils::sessionInfo(package = pkgs)))
    writeLines(txt, "Renvironment_info.txt")
    message(paste0("Environment info produced: Renvironment_info.txt"))
    
}

# Renvironment_info <- function(){ #if(!requireNamespace('packrat', quietly = TRUE))
# stop('function requires packrat to be installed') dependent_packages <-
# dirDependencies('.') sessionInfo(package = dependent_packages) txt <-
# c(paste0('Created at ',Sys.time(),' by ',Sys.info()['user'],'\n')) txt <-
# c(txt,capture.output(sessionInfo())) writeLines(txt, 'Renvironment_info.txt')
# message(paste0('Environment info produced: Renvironment_info.txt')) }

