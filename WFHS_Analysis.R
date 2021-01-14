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

tomo_df$condition <- ifelse(tomo_df$condition == 1, 1, 0)

### Merge Data ###
tomo_df$tomo <- 1
tomo_df$leef <- 0
leef_df$leef <- 1
leef_df$tomo <- 0
wfhs_df <- rbind(tomo_df, leef_df)
view(wfhs_df)

### Aggregate Grouped Survey Answers ###
wfhs_df$employee_control <- wfhs_df$wm_cwh1r + wfhs_df$wm_cwh2r + wfhs_df$wm_cwh3r +
  wfhs_df$wm_cwh4r + wfhs_df$wm_cwh5r + wfhs_df$wm_cwh6r + wfhs_df$wm_cwh7r + wfhs_df$wm_cwh8r

wfhs_df$fssb <- wfhs_df$wm_fssb1r + wfhs_df$wm_fssb3r + wfhs_df$wm_fssb4r + wfhs_df$wm_fssb5r









