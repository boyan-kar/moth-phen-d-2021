
# Script to export citations for packages as bib format
# And packge versions

# Libararies
library(data.table)
library(tidyverse)
library(rmarkdown)
library(knitr)

# Functions
source("scripts/00_functions.R")

# Packages to cite in report
packages <- c("base", "lme4", "MuMIn", "performance", "broom",
              "broom.mixed", "lmerTest")

write_bib(x = packages, file = "references/report_referenced_packages.bib")
versions <-
  packages %>% 
  map(packageVersion) %>% 
  map_chr(as.character)

versions_table <-
  data.table(package = packages, version = versions)

fwrite(versions_table, "references/report_versions_table.csv", eol = "\n")

# All packages used in /scripts

# Get paths to all scripts
all_scripts_paths <- list.files(path = "scripts", pattern = "\\.R$") %>% 
  str_c("scripts/", .)

# Load all scripts
all_scripts <-
  all_scripts_paths %>% 
  map(~readLines(.)) %>% 
  str_c(collapse="")

# Extract all packages loaded as character vector
all_packages <-
  all_scripts %>% 
  str_extract_all(., pattern = "(library\\([[:alnum:]]+\\))|(require\\([[:alnum:]]+\\))") %>% 
  str_extract_all(., pattern = "(?<=\\()[[:alnum:]]+(?=\\))") %>% 
  .[[1]] %>% 
  unique()

# Save as bib
write_bib(x = all_packages, file = "references/all_used_packages.bib")

# Render md file with citations
rmarkdown::render("references/all_package_references.Rmd")
