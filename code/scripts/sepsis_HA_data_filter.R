# Reading and cleaning source data

here::i_am("code/scripts/sepsis_HA_data_filter.R")

# Load libraries if not loaded yet

if(! "dplyr" %in% tolower((.packages()))) {library(dplyr)}

if (!exists("sepsis_HA_df_cleaned")) { sepsis_HA_df_cleaned <- readRDS(here::here("data/processed/sepsis_HA_df_cleaned.rds")) }

## ---- count-missing ----

# Count missing data across variables at time point 0

if (!exists("count_NA_vector")) {
  count_NA_vector <- sepsis_HA_df_cleaned %>%
    filter(time_point == 0) %>%
    mutate(across(contains("_dur_T0_first_end") | contains("_first_end_status"), function(x) tidyr::replace_na(as.character(x), "ND") )) %>% 
    is.na() %>%
    colSums() %>%
    sort()
}

## ---- vars-exclude ----

if (!exists("vars_exclude")) {
  vars_exclude <- names(count_NA_vector[count_NA_vector > 102]) # Здесь устанавливаем порог отсечки по количеству `NA` в точке 0
}

## ---- secondary-filtration ----
 
# Select data for further analysis
sepsis_HA_df_analyze <- sepsis_HA_df_cleaned %>%
  select(!all_of(vars_exclude)) %>% # удалены переменные с количеством `NA` в точке 0 больше порога
  filter(study_id != "L_Sep_1C_1") %>% # исключено исследование L_Sep_1C_1 -- выбыло 9 пациентов
  ungroup()

# summary(sepsis_HA_df_analyze)

# Export selected data to file
sepsis_HA_df_analyze %>% saveRDS(file = here::here("data/processed/sepsis_HA_df_analyze.rds"))
