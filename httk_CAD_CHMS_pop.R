#######################################################
# CHMS distributions of parameters for httk
# By: Kristin Eccles
# Date Updated: April 2nd, 2025
#######################################################
#set the path
#path <- ("D:/HDAD_ADDS/Statistics-Canada/CHMS/")

# Load libraries
library(haven)
library(tidyverse)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)

#######################################################
#### Cycle 6 Data ####
# load all files
list_of_files <- list.files(path = "D:/HDAD_ADDS/Statistics-Canada/CHMS/Cycle 6 (2018-19)/",
                            recursive = TRUE,
                            pattern = "\\.sas7bdat$",
                            full.names = TRUE)%>%
  as.data.frame()

#### Variables needed ####
# Clinical Full Sample (CLC_FULL)
# id: CLINICID
# Measured_weight (kg): HWM_13KG
# Measured Height(cm): HWM_11CM

# Non-Environmental Lab Data (NEL_FULL)
# Creatine LAB_BCRE
# Hemocrit CBC_HCT

# Household Full Sample (HHD_FULL)
# Racial group: PGDCGT
# Age at household interview: DHH_AGE
# Sex: DHH_SEX

df_data <- list_of_files[c(9, 15,18),] %>%
  set_names() %>% 
  map_df(read_sas, .id = "file_name")

# Define the columns to keep
columns_to_keep <- c("CLINICID", "DHH_AGE", "DHH_SEX", "HWM_13KG",  
                     "PGDCGT", "HWM_11", "CBC_HCT", "LAB_BCRE")

df_list <- df_data %>%
  split(df_data$file_name) %>%
  map(., ~ select(.x, where(~ !all(is.na(.)))))

final_data <- reduce(df_list, left_join, by = "CLINICID")

#subset to only variables needed
df_data_subset <- final_data[,c("CLINICID", "DHH_AGE", "DHH_SEX", "HWM_13KG",  "PGDCGT", "HWM_11CM", "CBC_HCT", "LAB_BCRE")]

# remove missing values
df_cleaned <- df_data_subset %>%
  mutate(across(where(is.numeric), ~ na_if(.x, 999.99)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 9996)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 9999)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 99.99)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 9.999)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 96)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 99.96)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 9.996)))%>%
  mutate(across(where(is.numeric), ~ na_if(.x, 99)))

#convert numeric variables to text
df_cleaned <- df_cleaned %>%
  mutate(Race = recode(as.character(PGDCGT),  # Convert to character for correct mapping
                            "1" = "White",
                            "2" = "Black",
                            "3" = "Korean",
                            "4" = "Filipino",
                            "5" = "Japanese",
                            "6" = "Chinese",
                            "7" = "South Asian",
                            "8" = "Southeast Asian",
                            "9" = "Arab",
                            "10" = "West Asian",
                            "11" = "Latin American",
                            "12" = "Other racial or cultural origin",
                            "13" = "Multiple racial or cultural origins"))%>%
  mutate(Sex = recode(DHH_SEX,
                           "1" = "Male",
                           "2" = "Female"))%>%
  filter(!is.na(Race))

#reclassify racial identity to match NHANES
# Other, Non-Hispanic White, Non-Hispanic Black, Mexican American, Other Hispanic
df_cleaned$NHANES_Race <- ifelse(df_cleaned$Race == "White", "Non-Hispanic White", 
                    ifelse(df_cleaned$Race == "Black", "Non-Hispanic Black", 
                    ifelse(df_cleaned$Race == "Latin American", "Other Hispanic", 
                    ifelse(df_cleaned$Race %in% c("South Asian", "Chinese", "Filipino", "Arab", "Southeast Asian", "West Asian",
                                                  "Korean", "Japanese", "Other", "Other racial or cultural origin",
                                                  "Multiple racial or cultural origins"), "Other", df_cleaned$Race))))
#summarize
summary_NHANES <- df_cleaned %>%
  group_by(NHANES_Race, Sex) %>%
  summarize(
    Count = n(),  # Count of rows per group
    across(c(DHH_AGE, HWM_13KG, HWM_11CM, CBC_HCT, LAB_BCRE), list(
      Min = ~ min(.x, na.rm = TRUE), 
      Median = ~ median(.x, na.rm = TRUE), 
      Mean = ~ mean(.x, na.rm = TRUE), 
      Max = ~ max(.x, na.rm = TRUE), 
      SD = ~ sd(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")  # Custom column names
  )
write.csv(summary_NHANES, "summary_NHANES.csv")

summary_CHMS <- df_cleaned %>%
  group_by(Race, Sex) %>%
  summarize(
    Count = n(),  # Count of rows per group
    across(c(DHH_AGE, HWM_13KG, HWM_11CM, CBC_HCT, LAB_BCRE), list(
      Min = ~ min(.x, na.rm = TRUE), 
      Median = ~ median(.x, na.rm = TRUE), 
      Mean = ~ mean(.x, na.rm = TRUE), 
      Max = ~ max(.x, na.rm = TRUE), 
      SD = ~ sd(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")  # Custom column names
  )
write.csv(summary_CHMS, "summary_CHMS.csv")

################################################################################
#### Pie Plot ####

# Create faceted pie chart
pie_CHMS <- ggplot(summary_CHMS, aes(x = "", y = Count, fill = Race)) +
  geom_bar(stat = "identity", width = 1) +  # Creates bar for each category
  coord_polar(theta = "y") +  # Converts bar chart into a pie chart
  facet_wrap(~ Sex) +  # Facet by Race
  theme_minimal() +  # Apply minimal theme
  labs(title = "CHMS Race", y = "Percentage", x = NULL) +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks = element_blank(),
        panel.grid = element_blank()) 

pie_NHANES <-ggplot(summary_NHANES, aes(x = "", y = Count, fill = NHANES_Race)) +
  geom_bar(stat = "identity", width = 1) +  # Creates bar for each category
  coord_polar(theta = "y") +  # Converts bar chart into a pie chart
  facet_wrap(~ Sex) +  # Facet by Race
  theme_minimal() +  # Apply minimal theme
  labs(title = "CHMS Recoded to NHANES Race", y = "Percentage", x = NULL, fill = "Race" ) +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks = element_blank(),
        panel.grid = element_blank())

pie_combined <- ggarrange(pie_CHMS, pie_NHANES,
                                ncol = 2,
                                labels = "AUTO",
                                common.legend = FALSE,
                                legend = "bottom")
pie_combined

ggsave("Pie_plots.jpg", pie_combined,  height =8, width =14)

################################################################################
#### Histograms ####
hist_hemocrit <- ggplot(df_cleaned, aes(x = CBC_HCT, fill = Sex)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  facet_wrap(Sex~ NHANES_Race, scales = "free", nrow = 2, ncol = 4) +
  labs(x = "Whole Blood Hematocrit (L/L)",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
hist_hemocrit                      

hist_creatinine <- ggplot(df_cleaned, aes(x = LAB_BCRE, fill = Sex)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  facet_wrap(Sex~ NHANES_Race, scales = "free", nrow = 2, ncol = 4) +
  labs(x = "Serum Creatinine (µmol/L)",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
hist_creatinine   

hist_combined <- ggarrange(hist_hemocrit, hist_creatinine,
                          nrow = 2,
                          labels = "AUTO",
                          common.legend = TRUE,
                          legend = "bottom")
hist_combined

ggsave("hist_plots.jpg", hist_combined,  height =10, width =14)
