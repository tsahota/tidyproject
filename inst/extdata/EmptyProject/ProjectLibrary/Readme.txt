When R is started in the main project directory (i.e. the parent directory of this directory) - the default location for installation of R packages will be here.

To disable the project library fuctionality for a project, simply delete this directory or the .Rprofile

If you want to use packrat (packrat copies all packages including the base package into the project), simply run the function packrat::init()

GOOD CODING PRACTICE:
When to install a package (into Project Library):

1) when the package is not or cannot be installed into the global R library
2) when your code is sensititive to package versions
3) when a package is version < 1 and therefore likely to break backward compatibility in future
4) when any of the above is true for any package dependency
5) when the global library for your version of R is likely to change
