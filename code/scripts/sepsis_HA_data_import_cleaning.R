# Reading and cleaning source data

here::i_am("code/scripts/sepsis_HA_data_import_cleaning.R")

# Load libraries if not loaded yet

if(! "dplyr" %in% tolower((.packages()))) {library(dplyr)}
if(! "forcats" %in% tolower((.packages()))) {library(forcats)}

## ---- read-data ----

# Read source data from file
sepsis_HA_df <- readRDS(here::here("data/raw/sepsis_HA_df_v3.rds"))

# summary(sepsis_HA_df)

## ---- primary-filtration ----

# Clean and modify data
sepsis_HA_df_cleaned <- sepsis_HA_df %>%  
  
  # Преобразование идентификаторов
  mutate(study_patient_id = gsub(pattern = "К", replacement = "K", study_patient_id)) %>% # замена кириллической К на латинскую в идентификаторах
  mutate(unic_patient_id = paste(study_id, study_patient_id, sep = "_"), .after = study_patient_id) %>% # добавлен сквозной уникальный идентификатор пациента `unic_patient_id`
  ungroup() %>%
  
  # Ручное исправление отдельных некорректных значений в соответствии с первичными данными  
  mutate(
    age = if_else(unic_patient_id == "L_mSep_2dC_11_2-07" & age == 1990, 33, age),
    height = if_else(unic_patient_id == "L_mSep_2dC_11_2-07" & height == 95, 195, height)) %>% # у пациентки L_mSep_2dC_11_2-07 : `age` == 1990 заменен на 33, `height` == 95 заменен на 195  
  mutate(weight = if_else(unic_patient_id == "L_mSep_2dC_11_R5-13" & weight == 19, 79, weight)) %>% # у пациентки L_mSep_2dC_11_R5-13 `weight` == 19 заменен на 79
  mutate(creatinine = if_else(unic_patient_id == "L_mSep_2dC_11_1-09" & time_point == "0", 62, creatinine)) %>% #у пациентки L_mSep_2dC_11_1-09 креатинин в точке 0 установлен 62
  mutate(
    AST = if_else(unic_patient_id == "L_Sep_2dC_7_2-04" & AST == 0, NA, AST),
    ALT = if_else(unic_patient_id == "L_Sep_2dC_7_2-04" & ALT == 0, NA, ALT),
    LDH = if_else(unic_patient_id == "L_Sep_2dC_7_2-04" & LDH == 0, NA, LDH)) %>% # у пациентки L_Sep_2dC_7_2-04 замена 0 на `NA` в `AST`, `ALT`, `LDH`
  mutate(NEU = if_else(unic_patient_id == "CL_Sep_3C_6_K35" & time_point == "0", 23, NEU)) %>% # у пациента CL_Sep_3C_6_K35 уровень нейтрофилов `NEU` в точке 0 установлен 23
  mutate(
    MV_T0 = if_else(unic_patient_id == "CL_Sep_3C_6_014", TRUE, MV_T0),
    MV_dur_T0_first_end = if_else(unic_patient_id == "CL_Sep_3C_6_014", 3, MV_dur_T0_first_end),
    MV_first_end_status = if_else(unic_patient_id == "CL_Sep_3C_6_014", tidyr::replace_na("recovery"), MV_first_end_status),
    MV_first_end_status = as.factor(MV_first_end_status)) %>% # у пациентки CL_Sep_3C_6_014 установлены показатели ИВЛ
  
  # Добавление, удаление и преобразование столбцов
  mutate(combined_efferon = case_when(treatment %in% c("efferon LPS", "efferon CT") ~ "Efferon", treatment == "base" ~ "Control"), 
         .after = treatment,
         combined_efferon = factor(combined_efferon, levels = c("Control", "Efferon"))) %>% # добавлен столбец `combined_efferon` 
  ungroup() %>%
  mutate(across(c(study_id, unic_patient_id, time_point, shock_T0, MV_T0), as.factor)) %>% # переменные `time_point`, `*_id` (кроме `study_patient_id`) и `_T0` преобразованы в `factor`  
  mutate(study_id = fct_inorder(study_id)) %>%
  mutate(unic_patient_id = fct_inorder(unic_patient_id)) %>% # уровни факторов `study_id` и `unic_patient_id` заданы по порядку строк таблицы  
  select(!starts_with("AKI_")) %>% # убраны показатели острого почечного повреждения
  select(!c(NEUr, LYMr)) %>% # удалены столбцы `NEUr`, `LYMr`
  mutate(NLR = NEU/LYM, .after = LYM) %>% 
  mutate(SII = NLR/PLT, .after = PLT) %>% # добавлены столбцы `NLR = NEU/LYM` и `SII = NLR/PLT`  
  ungroup() %>%
  
  # Добавление, удаление и преобразование значений переменных по всей таблице
  filter(!is.na(treatment)) %>% 
  filter(!is.na(sex)) %>% # удалены строки с `NA` в `treatment` и в `sex` -- выбыло 16 пациентов  
  mutate(MAP = if_else(MAP == 0, NA, MAP)) %>% #  в `MAP` 0 заменены на `NA`
  mutate(fibrinogen = if_else(fibrinogen == 0, NA, fibrinogen)) %>% #  в `fibrinogen` 0 заменены на `NA`
  mutate(height = if_else(study_id == "CL_Sep_3C_6" & !(study_patient_id %in% c("040", "041", "042")), height * 100, height)) %>% # в исследовании CL_Sep_3C_6 `height` переведен в см 
  mutate(aPTT = if_else((aPTT == 0) | (aPTT > 150) , 150, aPTT)) %>% #  в `aPTT` замена значений 0 и >150 на 150
  mutate(BMI = if_else(is.na(BMI) & !is.na(weight) & !is.na(height), round(weight / ((height / 100) ^ 2), 2), BMI)) %>% # в `BMI` `NA` заменены на расчетные значения 
  mutate(height = if_else(!is.na(BMI) & !is.na(weight) & is.na(height), round(100*sqrt(weight / BMI)) , height)) %>% # в `height` `NA` заменены на расчетные значения
  mutate(sex = recode(sex, "м" = "male", "ж" = "female", "мужской" = "male", "женский" = "female")) %>% # приведена в однообразный вид переменная `sex`
  
  mutate(across(where(is.factor), droplevels)) %>%
  ungroup()

## ---- export-cleaned-data ----

# Export cleaned data to file
sepsis_HA_df_cleaned %>% saveRDS(file = here::here("data/processed/sepsis_HA_df_cleaned.rds"))

rm(sepsis_HA_df)
