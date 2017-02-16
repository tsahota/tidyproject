When R is started in the main project directory (i.e. the parent directory of this directory) - the default location for installation of R packages will be here.

To disable the project library fuctionality for a project, simply delete this directory.

If you want to use packrat (packrat copies all packages including the base package into the project), delete this directory, and then run the function packrat::init()
