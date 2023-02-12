
# you need to install renv package
# use the lockfile to restore the packages for EWARS
options(renv.consent = TRUE)
renv::restore(lockfile='../renv.lock',repos=c(CRAN='https://packagemanager.rstudio.com/cran/latest',INLA='https://inla.r-inla-download.org/R/stable'))