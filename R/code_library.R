

#' List scripts
#'
#' @param folder string describing folder to search recursively in
#' @param extn character (can be regex) giving extension to limit search to
#' @param recursive by default TRUE
#' @examples
#' \dontrun{
#' ls_scripts('~/AZD6094/PK_liver4/') %>%
#'   info_scripts('Description') %>%
#'   filter(grepl('mod',DESCRIPTION))
#' }
#' @export

ls_scripts <- function(folder = ".", extn = "r|R", recursive = TRUE) {
    if (is.null(extn)) {
        output <- dir(folder, recursive = recursive, full.names = TRUE)
    } else {
        file_match <- paste0("\\.(", extn, ")$")
        output <- dir(folder, recursive = recursive, full.names = TRUE, pattern = file_match)
    }
    return(normalizePath(output, winslash = "/"))
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
#' ls_scripts('~/AZD6094/PK_liver4/') %>%
#'   info_scripts('Description') %>%
#'   filter(grepl('mod',DESCRIPTION))
#' }
#' @export
info_scripts <- function(files, fields = c("Description"), viewer = TRUE, silent = FALSE, 
    base_dirs = NULL, shorten_paths = TRUE) {
    if (length(fields) > 0) {
        res <- lapply(files, function(file.name) {
            ## per file
            suppressWarnings({
                s <- readLines(file.name, n = 30)
                field.vals <- as.data.frame(lapply(fields, function(field) {
                  field <- gsub(paste0("^.*", field, ": (.*)$"), "\\1", s[grepl(paste0("^.*", 
                    field, ": "), s, ignore.case = TRUE)], ignore.case = TRUE)
                  field <- field[!field %in% ""]
                  if (length(field) == 0) 
                    return(as.character(NA))
                  field[1]  ## in case multiple, take only first
                }))
                names(field.vals) <- fields
            })
            field.vals
        })
        res <- do.call(rbind, res)
    } else res <- data.frame(row.names = seq_along(files))
    
    d <- cbind(data.frame(FULL = normalizePath(files, winslash = "/"), FOLDER = normalizePath(dirname(files), 
        winslash = "/"), NAME = basename(files), stringsAsFactors = FALSE), res)
    
    if (!is.null(base_dirs)) {
        base_dirs <- normalizePath(base_dirs, winslash = "/")
        
        all_matches <- unlist(lapply(base_dirs, function(base_dir) {
            grep(paste0("^", base_dir), d$FULL)
        }))
        
        if (length(unique(all_matches)) != length(all_matches)) 
            stop("duplicate file matches found. Check base directories are not subsets of one another")
        
        for (base_dir in base_dirs) {
            match_base <- grepl(paste0("^", base_dir), d$FULL)
            
            d$FOLDER[match_base] <- gsub(paste0("^(", base_dir, ").*$"), "\\1", d$FULL[match_base])
            d$NAME[match_base] <- gsub(paste0("^", base_dir, .Platform$file.sep, "(.*)$"), 
                "\\1", d$FULL[match_base])
        }
    }
    
    d <- cbind(data.frame(FOLDER = d$FOLDER, NAME = d$NAME, stringsAsFactors = FALSE), 
        res)
    
    if (shorten_paths) {
        ds <- cbind(data.frame(FOLDER = short_path(d$FOLDER), NAME = d$NAME, stringsAsFactors = FALSE), 
            res)
    } else {
        ds <- d
    }
    
    if (!silent) {
        if (viewer) 
            get("View")(ds, "available files")  # else print(ds)
    }
    invisible(d)
}

#' Search for string in files
#'
#' @param files vector string of file names/paths
#' @param text string (can be regex) to search for
#' @export

search_scripts <- function(files, text) {
    res <- unlist(sapply(files, function(file.name) {
        suppressWarnings(s <- readLines(file.name))
        if (suppressWarnings(length(grep(text, s)) == 0)) 
            return(NULL) else return(file.name)
    }))
    names(res) <- NULL
    res
}

#' List files in code library
#'
#' @export

ls_code_library <- function() {
    ls_scripts(extn = ".*", folder = getOption("code_library_path"), recursive = TRUE)
}

#' Show Code Library
#'
#' @param extn vector string of extensions to include
#' @param fields character vector of fields to extract
#' @param viewer logical indicating if viewer should be used to display results (default=FALSE)
#' @param silent logical indicating if messages should be silenced (default=FALSE)
#' @param return_info logical (default = FALSE). Return data.frame of results (FALSE= returns file paths)
#' @export
code_library <- function(extn = NULL, fields = "Description", viewer = TRUE, silent = FALSE, 
    return_info = FALSE) {
    if (is.null(getOption("code_library_path"))) {
        if (!silent) {
            message("No directories attached. To attach add the following command:")
            message("  options(code_library_path=c(\"dir/of/scripts1\",\"dir/of/scripts2\",...))")
            message("     1. (for this session only) in the console")
            message("     2. (for this user) to ~/.Rprofile")
            message(paste0("     3. (for all users) to ", file.path(R.home(component = "home"), 
                "etc", "Rprofile.site")))
            
            message(" 2. Attach for this user by putting command in ~/.Rprofile:")
        }
        return(data.frame())
    }
    
    files <- ls_code_library()
    if (!is.null(extn)) {
        file_match <- paste0("\\.(", extn, ")$")
        files <- files[grepl(file_match, files)]
    }
    
    if (viewer == FALSE & !return_info) {
        return(files)
    }
    tryCatch({
        info <- info_scripts(files, fields = fields, viewer = viewer, silent = silent, 
            base_dirs = getOption("code_library_path"))
    }, error = function(e) {
        if (grepl("duplicate file", e$message)) 
            e$message <- paste0(e$message, ".\n  Check getOption(\"code_library_path\") points to non-overlapping folders")
        stop(e)
    })
    if (!silent) 
        message("\nNOTE: Do not source scripts from the code library,\n copy them to your project with copy_script")
    if (return_info) {
        if (silent) 
            return_ob <- invisible(info) else return_ob <- info
    } else {
        return_ob <- normalizePath(files, winslash = "/")
    }
    if (viewer == FALSE) 
        return(info)
    if (viewer == TRUE) 
        return(invisible(files))
    
}


#' Preview code_library file
#' @param name character indicating script in code_library to preview
#' @export
preview <- function(name) {
    ## preview files in code_library
    if (is_full_path(name)) {
        if (!file.exists(name)) 
            stop("file not found")
        file.show(name)
        return()
    }
    d <- code_library(extn = ".*", viewer = FALSE, silent = TRUE, return_info = TRUE, 
        fields = c())
    if (!name %in% d$NAME) 
        stop("file not found in code_library")
    if (length(which(d$NAME %in% name)) > 1) 
        stop("Matched more than one file with that name.\n Try preview() again with full path")
    pos <- match(name, d$NAME)
    path <- file.path(d$FOLDER[pos], d$NAME[pos])
    file.show(path)
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

attach_code_library <- function(path) {
    options(code_library_path = unique(c(path, getOption("code_library_path"))))
}

#' Replaces code library
#'
#' Replace code library search path with path(s)
#'
#' @param path character vector with paths to attach to

replace_code_library <- function(path) {
    options(code_library_path = unique(path))
}



#' shorten path name
#'
#' @param x character vector. Path to shorten.
short_path <- function(x) {
    split_paths <- strsplit(x, .Platform$file.sep)  #[[1]]
    short_paths <- lapply(split_paths, function(split_path) {
        if (length(split_path) > 5) 
            split_path.short <- c(split_path[1:3], "..", split_path[(length(split_path) - 
                1):length(split_path)]) else split_path.short <- split_path
        do.call(file.path, as.list(split_path.short))
    })
    unlist(short_paths)
}
