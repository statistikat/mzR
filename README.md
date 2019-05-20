## mzR

R package to compute estimates and their errors for Austrian Microcensus data.

### Installation

With the devtools package the mzR package can be installed directly with 

```r
devtools::install_github("statistikat/mzR", build_opts = c("--no-resave-data"))
```

In this case, due to package dependencies, you might have to install some R
packages which are necessary for mzR to run (see `help(install.packages)`
for instructions). The development version of the package can also be checked
out from https://github.com/statistikat/mzR and then installed manually.

### Getting started

You can find examples for getting started with mzR with `help(package = mzR)`
under "User guides, package vignettes and other documentation.". The vignette
can be displayed with

```r
vignette("mzR")
```

### Issues

Please report all issues, bugs or feature requests to the github issue tracking
system https://github.com/statistikat/mzR/issues.
