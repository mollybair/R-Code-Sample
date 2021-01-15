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
filter_cor_test <- function(df, val1, val2, cor_type) {
  new_df <- filter(df, wave == val1, condition == val2)
  cor <- cor_type(new_df$employee_control, new_df$fssb, method = 'pearson')
  return(cor)
}

treat_wave1_cor <- filter_cor_test(wfhs_df, 1, 1, cor.test) # treatment group, baseline
treat_wave2_cor <- filter_cor_test(wfhs_df, 2, 1, cor.test) # treatment group, 6 month follow up
treat_wave3_cor <- filter_cor_test(wfhs_df, 3, 1, cor.test) # treatment group, 12 month follow up
treat_wave4_cor <- filter_cor_test(wfhs_df, 4, 1, cor.test) # treatment group, 18 month follow up

control_wave1_cor <- filter_cor_test(wfhs_df, 1, 0, cor.test) # control group, baseline
control_wave2_cor <- filter_cor_test(wfhs_df, 2, 0, cor.test) # control group, 6 month follow up
control_wave3_cor <- filter_cor_test(wfhs_df, 3, 0, cor.test) # control group, 12 month follow up
control_wave4_cor <- filter_cor_test(wfhs_df, 4, 0, cor.test) # control group, 18 month follow up

### Visualize Correlation Over Time ###
treat_cor_coefs <- rep(NA, 4)
control_cor_coefs <- rep(NA, 4)
for (i in 1:4) {
  treat_cor_coefs[i] <- filter_cor_test(wfhs_df, i, 1, cor)
  control_cor_coefs[i] <- filter_cor_test(wfhs_df, i, 0, cor)
}
time <- c("Baseline", "6 Month Follow-Up", "12 Month Follow-Up", "18 Month Follow-Up")
cor_df$time <- factor(cor_df$time, as.character(cor_df$time))
ggplot(cor_df, mapping = aes(x = time)) +
  geom_point(aes(y = treat_cor_coefs, color = "Treatment Group")) +
  geom_point(aes(y = control_cor_coefs, color = "Control Group")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "Correlation Over Time", x = NULL, y = "Correlation Coefficient")

