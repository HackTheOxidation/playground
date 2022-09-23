#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
   stop("Error - Expected 1 argument, got ", call. = FALSE)
}

file_name <- args[1]

library(rmarkdown)
rmarkdown::render(file_name)
