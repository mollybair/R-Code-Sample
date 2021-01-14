### Load Libraries ###
library(tidyverse)

### Read Data ###
tomo_vars <- c("ADMINLINK", "WAVE", "EMPLOYEE", "CONDITION", 
               "WM_CWH1R", "WM_CWH2R", "WM_CWH3R", "WM_CWH4R", "WM_CWH5R", "WM_CWH6R", "WM_CWH7R",
               "WM_CWH8R", "WM_FSSB1R", "WM_FSSB3R", "WM_FSSB4R", "WM_FSSB5R")
tomo_df <- read_tsv("WFHS_Tomo_Data.tsv")[tomo_vars]
leef_vars <- c("ADMINLINK", "WAVE", "EMPLOYEE", "CONDITION",
               "WM_CWH1R", "WM_CWH2R", "WM_CWH3R", "WM_CWH4R", "WM_CWH5R", "WM_CWH6R", "WM_CWH7R",
               "WM_CWH8R", "WM_FSSB1R", "WM_FSSB3R", "WM_FSSB4R", "WM_FSSB5R")
leef_df <- read_tsv("WFHS_Leef_Data.tsv")[leef_vars]

names(tomo_df) <- tolower(names(tomo_df))
names(leef_df) <- tolower(names(leef_df))

view(tomo_df)
view(leef_df)
