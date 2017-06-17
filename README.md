# tidyproject

[![Travis-CI Build Status](https://travis-ci.org/tsahota/tidyproject.svg?branch=master)](https://travis-ci.org/tsahota/tidyproject)
[![Coverage Status](https://coveralls.io/repos/github/tsahota/tidyproject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/tidyproject?branch=master)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tsahota/tidyproject?branch=master&svg=true)](https://ci.appveyor.com/project/tsahota/tidyproject)

## Build Institutional Memory and Learning:
 
#### Memory:
* Consistent and tidy directory structure for all your projects
* Long term reproducibility:
  * Project library for each project: stores packages alongside project code (like packrat but lighter): 
  * Version control (git) compatibility: roll back your projects when needed.

#### Learning:
* Code library: Store and share scripts, functions, templates in an ever improving repository of code
* Search code library for keywords or raw text

## Installation

```R
install.packages("devtools")
devtools::install_github("tsahota/tidyproject")
library(tidyproject)
```

## Quick Tutorial

### Make a project
Make a tidyproject with

```R
make_project("path/to/directory")
```
Open the newly created Rstudio project with File -> Open Project. **Warning: do not use setwd() to open tidyprojects.**

You should see a new directory structure.  Opening the Rstudio project reconfigures default libraries to use the project library, e.g. try installing a package now:

```R
devtools::install_github("tsahota/tidyproject")
library(tidyproject)
```

This package is now in your "ProjectLibrary" subdirectory. Loading packages from this tidyproject (e.g. with `library`), will cause packages in this specific project library load. If you want to switch projects, use Rstudio's "open project".  Using setwd() is strongly discouraged.

Create a new script:

```R
new_script("scriptname.R")
```
This will pre-fill some comment fields and store the script in your "Scripts" subdirectory.

### Monitor your compliance

Check your tidyproject is set up correctly by typing the following:

```R
check_session()
```

It complains about Renvironment_info.txt not being present. Take an snapshot of your R environment:

```R
Renvironment_info()
```

This will search your scripts in your "Scripts" directory for package dependencies and output version and environment information into Renvironment_info.txt of the main directory.  If you run `check_session()` again it should pass now.  If tidyproject ever gives you errors, `check_session()` is a good first port of call.


### Add a code library

If you have a directory of scripts somewhere (scripts can be in subdirectories) add it with the following:

```R
options(code_library_path="path/to/code/repository")
```

**Tip 1**: Add this to your user `~/.Rprofile` (create it if it doesn't exist) to avoid doing this every R session (or `R_HOME/etc/Rprofile.site` if you want this to apply for all users).

**Tip 2**: Add multiple directories to the `code_library_path` by specifying a vector of directory paths.

### Use code library

View code library with:

```R
code_library()
```

Preview code with:

```R
preview("nameofscript.R")
```

copy code into your project "Scripts" subdirectory with:

```R
copy_script("nameofscript.R")
```

copy other types of files into other subdirectories (e.g. "Models") with:

```R
copy_file("stantemplate.stan","Models")
```

### Search for code

To list all R scripts in the `./Scripts` subdirectory:

```R
ls_scripts("./Scripts")
```

More refined searching is most easily accomplished with the pipe symbol, `%>%`, e.g. to find all scripts in `./Scripts` that contain the text `text_to_match`:

```R
ls_scripts("./Scripts") %>% search_raw("text_to_match")
```

To find all scripts in the **Code Library** that contain the text `text_to_match`:

```R
ls_code_library() %>% search_raw("text_to_match")
```

To find all scripts in Code Library that match the keyword `keyword_to_match`:

```R
ls_code_library() %>% search_keyword("keyword_to_match")
```

Chain multiple commands for more targetted searching:

```R
ls_code_library() %>%
  search_keyword("keyword_to_match") %>%
  search_raw("text_to_match")
```

