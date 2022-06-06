#!/usr/bin/Rscript

# This script should get you the dependencies required for genomeSidekick
# Might want to run by hand so you do not overwrite custom installations
# More info:
# https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/

# Define a function to negate the %in% function in R
`%notin%` <- Negate(`%in%`)

# List all dependencies
dependencies <- c("shiny", "shinyBS", "DT", "ggplot2", "ggrepel", "plotly", 
                  "magrittr", "htmlwidgets", "gprofiler2", "data.table", "stringi", 
                  "httr", "jsonlite", "tidyverse", "easyPubMed")

# Check which ones are not met by using %notin%
not.met <- dependencies %notin% rownames(installed.packages())

# Install unmet dependencies
if(any(not.met)) {
  install.packages(dependencies[not.met])
}


