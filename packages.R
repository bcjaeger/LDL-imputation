
## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
# data management
library(magrittr)
library(labelled)
library(janitor)
# data analysis
library(tidyverse)
library(naniar)
library(mitml)
library(mitools)
library(survey)
library(parsnip)
library(recipes)
# reporting
library(glue)
library(table.glue)
library(officer)
library(flextable)
library(devEMF)
library(magick)
library(paletteer)

conflicted::conflict_prefer("roc",       "pROC")
conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflict_prefer("slice",     "dplyr")
conflicted::conflict_prefer('summarise', 'dplyr')
conflicted::conflict_prefer("gather",    "tidyr")
conflicted::conflict_prefer("set_names", "purrr")
