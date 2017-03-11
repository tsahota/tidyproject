# TidyProject

[![Travis-CI Build Status](https://travis-ci.org/tsahota/TidyProject.svg?branch=master)](https://travis-ci.org/tsahota/TidyProject)
[![Coverage Status](https://coveralls.io/repos/github/tsahota/TidyProject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/TidyProject?branch=master)

Build *Institutional Memory and Learning*:
 
####Institutional Memory:
* Consistent and tidy directory structure for all your projects
* Long term reproducibility:
  * Project library for each project: stores packages alongside project code (like packrat but lighter): 
  * Version control (git) compatibility: roll back your projects when needed.

####Institutional Learning:
* Code library: Store and share scripts, functions, templates in an ever improving repository of code
* Search code library for keywords or raw text

## Installation

```R
install.packages("devtools")
devtools::install_github("tsahota/TidyProject")
```

## Quick Tutorial

### Make a project
Make a TidyProject with

```R
make_project("path/to/directory")
```
Open the newly created Rstudio project with File -> Open Project. **Warning: do not use setwd() to open TidyProjects.**

You should see a new directory structure.  Opening the Rstudio project reconfigures default libraries to use the project library, e.g. try installing a package now:

```R
devtools::install_github("tsahota/TidyProject")
```

This package is now in your "ProjectLibrary" subdirectory. Loading packages from this TidyProject (e.g. with `library`), will cause packages in this specific project library load. If you want to switch projects, use Rstudio's "open project".  Using setwd() is strongly discouraged.

### Make a script
Create a new script:

```R
new_script("scriptname.R")
```
This will pre-fill some comments and store the script in your "Scripts" subdirectory.

Take an snapshot of your R environment:

```R
Renvironment_info()
```

This will search your scripts in your "Scripts" directory for package dependencies and output version and environment information into Renvironment_info.txt of the main directory.

### Add a code library

If you have a directory of scripts somewhere (scripts can be in subdirectories) add it with the following:

```R
options(code_library_path="path/to/code/repository")
```

View code library with:

```R
code_library()
```

Preview code with:

```R
preview("nameofscript.R")
```

copy code into your project "Scripts" directory with:

```R
copy_script("nameofscript.R")
```
Attach multiple code libraries with `options(code_library_path=c("path/to/code/repo1","path/to/code/repo2")")`.

To avoid having to do this each R session, add your `options` command to your user "~/.Rprofile" (or "R_HOME/etc/Rprofile.site" if you want this to apply for all users).
