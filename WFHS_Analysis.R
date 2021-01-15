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
leef_df$condition <- ifelse(leef_df$condition ==1, 1, 0)

### Recode Missing Values ###
vars_numeric_missing <- c("wm_cwh1r", "wm_cwh2r", "wm_cwh3r", "wm_cwh4r", "wm_cwh5r", "wm_cwh6r",
                          "wm_cwh7r", "wm_cwh8r", "wm_fssb1r", "wm_fssb3r", "wm_fssb4r", "wm_fssb5r")
for (var in vars_numeric_missing) {
  tomo_df[[var]] <- ifelse(tomo_df[[var]] < 1, NA, tomo_df[[var]])
  leef_df[[var]] <- ifelse(leef_df[[var]] < 1, NA, leef_df[[var]])
}

### Merge Data ###
tomo_df$tomo <- 1
tomo_df$leef <- 0
leef_df$leef <- 1
leef_df$tomo <- 0
wfhs_df <- rbind(tomo_df, leef_df)

### Aggregate Grouped Survey Answers ###
wfhs_df$employee_control <- rowSums(subset(wfhs_df, select = wm_cwh1r:wm_cwh8r), na.rm = TRUE)
  # higher value indicates employee has greater control over hours/schedule
wfhs_df$fssb <- rowSums(subset(wfhs_df, select = wm_fssb1r:wm_fssb5r), na.rm = TRUE)
  # higher value indicates greater family-supportive supervisor behavior (FSSB)

### Test Correlation Between Employee Choice and FSSB ###
wfhs_pre_treat <- filter(wfhs_df, wave == 1)
baseline_cor <- cor.test(wfhs_pre_treat$employee_control, wfhs_pre_treat$fssb, method = 'pearson')
baseline_cor

filter_cor_test <- function(df, new_df, val1, val2, cor_name) {
  new_df <- filter(df, wave == val1, condition == val2)
  cor_name <- cor.test(new_df$employee_control, new_df$fssb, method = 'pearson')
  cor_name
}

filter_cor_test(wfhs_df, wfhs_treat_wave2, 2, 1, treat_wave2_cor) # treatment group, 6 month follow up
filter_cor_test(wfhs_df, wfhs_treat_wave3, 3, 1, treat_wave3_cor) # treatment group, 12 month follow up
filter_cor_test(wfhs_df, wfhs_treat_wave4, 4, 1, treat_wave4_cor) # treatment group, 18 month follow up

filter_cor_test(wfhs_df, wfhs_control_wave2, 2, 0, control_wave2_cor) # control group, 6 month follow up
filter_cor_test(wfhs_df, wfhs_control_wave3, 3, 0, control_wave3_cor) # control group, 12 month follow up
filter_cor_test(wfhs_df, wfhs_control_wave4, 4, 0, control_wave4_cor) # control group, 18 month follow up












