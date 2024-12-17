---
title: "sepsis_project"
author: "Anastasiia Potamoshneva"
date: "2024-11-30"
output: 
  html_document:
    keep_md: true
---



# Reading data





# Adjusting variables to an appropriate format


``` r
# Convert factor variables into factor

sepsis <- sepsis %>% 
  ungroup() %>% 
  mutate(
  across(c(study_id, time_point, sex, shock_T0, MV_T0, AKI_T0, shock_first_end_status, ICU_first_end_status, MV_first_end_status), ~ as.factor(.x))) 

# Convert height to cm

sepsis <- sepsis %>%
  mutate(
    height = ifelse(study_id == "CL_Sep_3C_6" & !(study_patient_id %in% c("040", "041", "042")), height * 100, height)
  )

# Add BMI where missing

sepsis <- sepsis %>%
  mutate(
    BMI = ifelse(
      is.na(BMI) & !is.na(weight) & !is.na(height), 
      round(weight / (height / 100)^2, 1), 
      BMI
      )
  )

# Remove patient with erroneous age and height

sepsis <- sepsis %>%
  filter(study_patient_id != "2-07")

# Replace MAP=0 with NA

sepsis <- sepsis %>%
  mutate(
    MAP = ifelse(MAP == 0, NA, MAP)
  )

# Replace weight=19 with NA

sepsis <- sepsis %>%
  mutate(
    weight = ifelse(study_patient_id == "R5-13", NA, weight)
  )

# Convert creatinine from mg/dL to mcmol/L

sepsis <- sepsis %>%
  mutate(
    creatinine = ifelse(study_patient_id == "1-09" & time_point == "0", creatinine * 88.4, creatinine)
  )

# Leave rows with less than 70% NAs

sepsis <- sepsis %>% filter(rowMeans(is.na(sepsis)) <= 0.7)
```




# Renaming variables


``` r
#Rename columns

sepsis <- sepsis %>% 
  rename_with(function(x) x %>% 
                str_replace("age", "Age, years") %>% 
                str_replace("sex", "Sex") %>% 
                str_replace("weight", "Weight, kg") %>%
                str_replace("height", "Height, cm") %>%
                str_replace("time_point", "Time point, days") %>% 
                str_replace("BMI", "BMI, kg/m^2") %>% 
                str_replace("age", "Age, years") %>% 
                str_replace("MAP", "MAP, mmHg") %>% 
                str_replace("FiO2", "FiO2, %") %>% 
                str_replace("lactate", "Lactate, mmol/L") %>% 
                str_replace("PaO2$", "PaO2, mmHg") %>% 
                str_replace("PF_ratio", "PaO2/FiO2") %>% 
                str_replace("WBC", "WBC, *10^9/L") %>% 
                str_replace("^NEU$", "NEU, *10^9/L") %>% 
                str_replace("^NEUr$", "NEUr, %") %>% 
                str_replace("LYM$", "LYM, *10^9/L") %>% 
                str_replace("LYMr$", "LYMr, %") %>% 
                str_replace("PLT", "PLT, *10^9/L") %>% 
                str_replace("aPTT", "aPTT, s") %>% 
                str_replace("ATIII", "ATIII, %") %>% 
                str_replace("fibrinogen", "Fibrinogen, g/L") %>% 
                str_replace("diuresis", "Diuresis, ml") %>% 
                str_replace("creatinine", "Creatinine, mcmol/L") %>% 
                str_replace("urea", "Urea, mcmol/L") %>% 
                str_replace("protein_total", "Total protein, g/L") %>% 
                str_replace("albumin", "Albumin, g/L") %>% 
                str_replace("bilirubin_total", "Total bilirubin, μmol/L") %>% 
                str_replace("bilirubin_direct", "Direct bilirubin, μmol/L") %>% 
                str_replace("AST", "AST, U/L") %>%  
                str_replace("ALT", "ALT, U/L") %>% 
                str_replace("LDH", "LDH, U/L") %>% 
                str_replace("CRP", "CRP, mg/L") %>% 
                str_replace("PCT", "PCT, μg/L") %>%
                str_replace("ferritin", "Ferritin, μg/L") %>% 
                str_replace("IL_6", "IL-6, pg/mL") 
              )
              

# Rename values in columns

sepsis <- sepsis %>% 
  mutate(
    Sex = recode(Sex,
                 "м" = "male",
                 "ж" = "female",
                 "мужской" = "male",
                 "женский" = "female")
  )
```

# Descriptive stats for Efferon and Control groups


``` r
summary_table <-  sepsis  %>% 
  mutate(
  combined_efferon = case_when(
    treatment %in% c("efferon LPS", "efferon CT")~"Efferon",
    treatment == "base" ~ "Control"),
  combined_efferon = factor(combined_efferon, levels = c("Efferon", "Control"))
) %>% 
  filter(`Time point, days`== "0") %>% 
  select(everything(), -c(study_id, study_patient_id, treatment, "Time point, days", DDimer, EAA, shock_dur_T0_first_end, shock_first_end_status, AKI_dur_T0_first_end, AKI_first_end_status, ICU_dur_T0_first_end, ICU_first_end_status, MV_dur_T0_first_end, MV_first_end_status, presepsin, IL_1b, IL_8, IL_10, TNFa, Ig_A, Ig_G, Ig_M)) %>% 
 
  tbl_summary(by=combined_efferon, 
              statistic = list(all_continuous() ~ c("{median} [{p25};{p75}]")),
              missing = "no") %>% 
  modify_table_body(
    filter, !(paste(variable, label) %in% c(
      "shock_T0 FALSE",
      "MV_T0 FALSE",
      "AKI_T0 FALSE"
    ))) 

table_body <- as_tibble(summary_table$table_body)

table_body <- table_body %>%
  mutate(
    stat_1 = gsub(",", "", stat_1),
        stat_2 = gsub(",", "", stat_2),
    Difference = case_when(
      stat_1 == "NA [NA;NA]" | stat_2 == "NA [NA;NA]" ~ {
        "NA"
      },
      
      var_type == "continuous" & !is.na(stat_1) & !is.na(stat_2) ~ {
        
        median_1 <- as.numeric(str_extract(stat_1, "^[^\\[]+")) 
        median_2 <- as.numeric(str_extract(stat_2, "^[^\\[]+"))
        as.character(round(median_1 - median_2, 2))
      },
      var_type == "categorical" & !is.na(stat_1) & !is.na(stat_2)~ {
        count_1 <- as.numeric(str_extract(stat_1, "^[^\\(]+"))
        count_2 <- as.numeric(str_extract(stat_2, "^[^\\(]+"))
         paste0(round(100 * (count_1 - count_2) / count_2, 2), "%")
      },
      
    )
  )
```

```
## Warning: There were 4 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `Difference = case_when(...)`.
## Caused by warning:
## ! в результате преобразования созданы NA
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.
```

``` r
summary_table <- summary_table %>%
  modify_table_body(
    ~ .x %>% left_join(select(table_body, variable, label, Difference), by = c("variable", "label"))
  ) %>%
  modify_header(
    Difference ~ "**Difference**" 
  )
summary_table
```

```{=html}
<div id="rnkjmfzfju" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rnkjmfzfju table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rnkjmfzfju thead, #rnkjmfzfju tbody, #rnkjmfzfju tfoot, #rnkjmfzfju tr, #rnkjmfzfju td, #rnkjmfzfju th {
  border-style: none;
}

#rnkjmfzfju p {
  margin: 0;
  padding: 0;
}

#rnkjmfzfju .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rnkjmfzfju .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rnkjmfzfju .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rnkjmfzfju .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rnkjmfzfju .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rnkjmfzfju .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnkjmfzfju .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rnkjmfzfju .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rnkjmfzfju .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rnkjmfzfju .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rnkjmfzfju .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rnkjmfzfju .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rnkjmfzfju .gt_spanner_row {
  border-bottom-style: hidden;
}

#rnkjmfzfju .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#rnkjmfzfju .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rnkjmfzfju .gt_from_md > :first-child {
  margin-top: 0;
}

#rnkjmfzfju .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rnkjmfzfju .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rnkjmfzfju .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rnkjmfzfju .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rnkjmfzfju .gt_row_group_first td {
  border-top-width: 2px;
}

#rnkjmfzfju .gt_row_group_first th {
  border-top-width: 2px;
}

#rnkjmfzfju .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnkjmfzfju .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rnkjmfzfju .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rnkjmfzfju .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnkjmfzfju .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnkjmfzfju .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rnkjmfzfju .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rnkjmfzfju .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rnkjmfzfju .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnkjmfzfju .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rnkjmfzfju .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnkjmfzfju .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rnkjmfzfju .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnkjmfzfju .gt_left {
  text-align: left;
}

#rnkjmfzfju .gt_center {
  text-align: center;
}

#rnkjmfzfju .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rnkjmfzfju .gt_font_normal {
  font-weight: normal;
}

#rnkjmfzfju .gt_font_bold {
  font-weight: bold;
}

#rnkjmfzfju .gt_font_italic {
  font-style: italic;
}

#rnkjmfzfju .gt_super {
  font-size: 65%;
}

#rnkjmfzfju .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rnkjmfzfju .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rnkjmfzfju .gt_indent_1 {
  text-indent: 5px;
}

#rnkjmfzfju .gt_indent_2 {
  text-indent: 10px;
}

#rnkjmfzfju .gt_indent_3 {
  text-indent: 15px;
}

#rnkjmfzfju .gt_indent_4 {
  text-indent: 20px;
}

#rnkjmfzfju .gt_indent_5 {
  text-indent: 25px;
}

#rnkjmfzfju .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rnkjmfzfju div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>Efferon</strong><br />
N = 127</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Control</strong><br />
N = 134</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Difference"><span class='gt_from_md'><strong>Difference</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Sex</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="Difference" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    female</td>
<td headers="stat_1" class="gt_row gt_center">76 (60%)</td>
<td headers="stat_2" class="gt_row gt_center">64 (48%)</td>
<td headers="Difference" class="gt_row gt_center">18.75%</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    male</td>
<td headers="stat_1" class="gt_row gt_center">51 (40%)</td>
<td headers="stat_2" class="gt_row gt_center">70 (52%)</td>
<td headers="Difference" class="gt_row gt_center">-27.14%</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age, years</td>
<td headers="stat_1" class="gt_row gt_center">43 [31;63]</td>
<td headers="stat_2" class="gt_row gt_center">47 [34;67]</td>
<td headers="Difference" class="gt_row gt_center">-4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Weight, kg</td>
<td headers="stat_1" class="gt_row gt_center">76 [70;90]</td>
<td headers="stat_2" class="gt_row gt_center">80 [68;91]</td>
<td headers="Difference" class="gt_row gt_center">-4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Height, cm</td>
<td headers="stat_1" class="gt_row gt_center">170 [165;174]</td>
<td headers="stat_2" class="gt_row gt_center">171 [170;172]</td>
<td headers="Difference" class="gt_row gt_center">-1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">BMI, kg/m^2</td>
<td headers="stat_1" class="gt_row gt_center">26.2 [24.0;31.5]</td>
<td headers="stat_2" class="gt_row gt_center">29.0 [23.0;32.0]</td>
<td headers="Difference" class="gt_row gt_center">-2.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">APACHE2</td>
<td headers="stat_1" class="gt_row gt_center">21 [18;25]</td>
<td headers="stat_2" class="gt_row gt_center">22 [17;26]</td>
<td headers="Difference" class="gt_row gt_center">-1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SOFA</td>
<td headers="stat_1" class="gt_row gt_center">8.0 [7.0;10.0]</td>
<td headers="stat_2" class="gt_row gt_center">8.0 [6.0;11.0]</td>
<td headers="Difference" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MAP, mmHg</td>
<td headers="stat_1" class="gt_row gt_center">75 [62;87]</td>
<td headers="stat_2" class="gt_row gt_center">81 [64;91]</td>
<td headers="Difference" class="gt_row gt_center">-6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PaO2, mmHg</td>
<td headers="stat_1" class="gt_row gt_center">98 [81;121]</td>
<td headers="stat_2" class="gt_row gt_center">97 [79;104]</td>
<td headers="Difference" class="gt_row gt_center">1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">FiO2, %</td>
<td headers="stat_1" class="gt_row gt_center">40 [30;50]</td>
<td headers="stat_2" class="gt_row gt_center">40 [40;50]</td>
<td headers="Difference" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PaO2/FiO2</td>
<td headers="stat_1" class="gt_row gt_center">286 [228;342]</td>
<td headers="stat_2" class="gt_row gt_center">250 [200;360]</td>
<td headers="Difference" class="gt_row gt_center">36</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Lactate, mmol/L</td>
<td headers="stat_1" class="gt_row gt_center">3.10 [2.20;5.50]</td>
<td headers="stat_2" class="gt_row gt_center">2.80 [2.00;4.40]</td>
<td headers="Difference" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VIS2020</td>
<td headers="stat_1" class="gt_row gt_center">47 [24;75]</td>
<td headers="stat_2" class="gt_row gt_center">20 [6;48]</td>
<td headers="Difference" class="gt_row gt_center">27</td></tr>
    <tr><td headers="label" class="gt_row gt_left">WBC, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">18 [12;25]</td>
<td headers="stat_2" class="gt_row gt_center">15 [10;23]</td>
<td headers="Difference" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEU, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">15 [10;22]</td>
<td headers="stat_2" class="gt_row gt_center">13 [8;20]</td>
<td headers="Difference" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEUr, %</td>
<td headers="stat_1" class="gt_row gt_center">89 [83;92]</td>
<td headers="stat_2" class="gt_row gt_center">89 [84;92]</td>
<td headers="Difference" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYM, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">1.13 [0.68;1.73]</td>
<td headers="stat_2" class="gt_row gt_center">1.01 [0.63;1.91]</td>
<td headers="Difference" class="gt_row gt_center">0.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYMr, %</td>
<td headers="stat_1" class="gt_row gt_center">5.8 [3.4;9.7]</td>
<td headers="stat_2" class="gt_row gt_center">6.0 [4.1;10.1]</td>
<td headers="Difference" class="gt_row gt_center">-0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PLT, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">189 [110;277]</td>
<td headers="stat_2" class="gt_row gt_center">184 [88;247]</td>
<td headers="Difference" class="gt_row gt_center">5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">aPTT, s</td>
<td headers="stat_1" class="gt_row gt_center">31 [27;37]</td>
<td headers="stat_2" class="gt_row gt_center">33 [28;39]</td>
<td headers="Difference" class="gt_row gt_center">-2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INR</td>
<td headers="stat_1" class="gt_row gt_center">1.22 [1.09;1.41]</td>
<td headers="stat_2" class="gt_row gt_center">1.31 [1.17;1.65]</td>
<td headers="Difference" class="gt_row gt_center">-0.09</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ATIII, %</td>
<td headers="stat_1" class="gt_row gt_center">61 [51;70]</td>
<td headers="stat_2" class="gt_row gt_center">51 [28;67]</td>
<td headers="Difference" class="gt_row gt_center">10</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Fibrinogen, g/L</td>
<td headers="stat_1" class="gt_row gt_center">4.90 [3.43;6.70]</td>
<td headers="stat_2" class="gt_row gt_center">4.09 [3.06;5.65]</td>
<td headers="Difference" class="gt_row gt_center">0.81</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Diuresis, ml</td>
<td headers="stat_1" class="gt_row gt_center">1,000 [500;1,210]</td>
<td headers="stat_2" class="gt_row gt_center">900 [200;2,000]</td>
<td headers="Difference" class="gt_row gt_center">100</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Creatinine, mcmol/L</td>
<td headers="stat_1" class="gt_row gt_center">197 [108;300]</td>
<td headers="stat_2" class="gt_row gt_center">194 [95;302]</td>
<td headers="Difference" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Urea, mcmol/L</td>
<td headers="stat_1" class="gt_row gt_center">12 [4;19]</td>
<td headers="stat_2" class="gt_row gt_center">10 [5;18]</td>
<td headers="Difference" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total protein, g/L</td>
<td headers="stat_1" class="gt_row gt_center">54 [49;62]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="Difference" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Albumin, g/L</td>
<td headers="stat_1" class="gt_row gt_center">27.9 [23.3;30.5]</td>
<td headers="stat_2" class="gt_row gt_center">23.8 [21.2;29.4]</td>
<td headers="Difference" class="gt_row gt_center">4.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total bilirubin, μmol/L</td>
<td headers="stat_1" class="gt_row gt_center">19 [10;35]</td>
<td headers="stat_2" class="gt_row gt_center">16 [10;37]</td>
<td headers="Difference" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Direct bilirubin, μmol/L</td>
<td headers="stat_1" class="gt_row gt_center">9 [4;22]</td>
<td headers="stat_2" class="gt_row gt_center">7 [2;20]</td>
<td headers="Difference" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AST, U/L</td>
<td headers="stat_1" class="gt_row gt_center">37 [24;101]</td>
<td headers="stat_2" class="gt_row gt_center">46 [21;114]</td>
<td headers="Difference" class="gt_row gt_center">-9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ALT, U/L</td>
<td headers="stat_1" class="gt_row gt_center">25 [14;77]</td>
<td headers="stat_2" class="gt_row gt_center">26 [16;71]</td>
<td headers="Difference" class="gt_row gt_center">-1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LDH, U/L</td>
<td headers="stat_1" class="gt_row gt_center">416 [245;618]</td>
<td headers="stat_2" class="gt_row gt_center">500 [187;907]</td>
<td headers="Difference" class="gt_row gt_center">-84</td></tr>
    <tr><td headers="label" class="gt_row gt_left">CRP, mg/L</td>
<td headers="stat_1" class="gt_row gt_center">204 [127;322]</td>
<td headers="stat_2" class="gt_row gt_center">179 [108;256]</td>
<td headers="Difference" class="gt_row gt_center">25</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PCT, μg/L</td>
<td headers="stat_1" class="gt_row gt_center">14 [4;45]</td>
<td headers="stat_2" class="gt_row gt_center">10 [2;32]</td>
<td headers="Difference" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ferritin, μg/L</td>
<td headers="stat_1" class="gt_row gt_center">459 [208;935]</td>
<td headers="stat_2" class="gt_row gt_center">741 [266;1,008]</td>
<td headers="Difference" class="gt_row gt_center">-282</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL-6, pg/mL</td>
<td headers="stat_1" class="gt_row gt_center">272 [92;1,779]</td>
<td headers="stat_2" class="gt_row gt_center">408 [125;1,750]</td>
<td headers="Difference" class="gt_row gt_center">-136</td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="Difference" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">113 (89%)</td>
<td headers="stat_2" class="gt_row gt_center">113 (84%)</td>
<td headers="Difference" class="gt_row gt_center">0%</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="Difference" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">103 (82%)</td>
<td headers="stat_2" class="gt_row gt_center">93 (69%)</td>
<td headers="Difference" class="gt_row gt_center">10.75%</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="Difference" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">96 (76%)</td>
<td headers="stat_2" class="gt_row gt_center">94 (70%)</td>
<td headers="Difference" class="gt_row gt_center">2.13%</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n (%); Median [Q1;Q3]</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


# Descriptive stats for Studies comparison


``` r
sepsis %>%
      filter(`Time point, days`== "0") %>% 
  select(everything(), -c(study_patient_id, "Time point, days", DDimer, EAA, shock_dur_T0_first_end, shock_first_end_status, AKI_dur_T0_first_end, AKI_first_end_status, ICU_dur_T0_first_end, ICU_first_end_status, MV_dur_T0_first_end, MV_first_end_status, presepsin, IL_1b, IL_8, IL_10, TNFa, Ig_A, Ig_G, Ig_M)) %>% 
  tbl_summary(by=study_id,
              type = list(all_continuous()  ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                    "{N_miss}",
                                                    "{mean} ± {sd}",
                                                    "{median} [{p25};{p75}]")),
              missing = "no") %>% 
  add_stat_label(label = list(all_continuous() ~ c("Non-missed",
                                                   "Missed",
                                                   "Mean ± SD",
                                                   "Median [Q1:Q3]")))
```

```{=html}
<div id="umyjpqvfkt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#umyjpqvfkt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#umyjpqvfkt thead, #umyjpqvfkt tbody, #umyjpqvfkt tfoot, #umyjpqvfkt tr, #umyjpqvfkt td, #umyjpqvfkt th {
  border-style: none;
}

#umyjpqvfkt p {
  margin: 0;
  padding: 0;
}

#umyjpqvfkt .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#umyjpqvfkt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#umyjpqvfkt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#umyjpqvfkt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#umyjpqvfkt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#umyjpqvfkt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umyjpqvfkt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#umyjpqvfkt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#umyjpqvfkt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#umyjpqvfkt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#umyjpqvfkt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#umyjpqvfkt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#umyjpqvfkt .gt_spanner_row {
  border-bottom-style: hidden;
}

#umyjpqvfkt .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#umyjpqvfkt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#umyjpqvfkt .gt_from_md > :first-child {
  margin-top: 0;
}

#umyjpqvfkt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#umyjpqvfkt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#umyjpqvfkt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#umyjpqvfkt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#umyjpqvfkt .gt_row_group_first td {
  border-top-width: 2px;
}

#umyjpqvfkt .gt_row_group_first th {
  border-top-width: 2px;
}

#umyjpqvfkt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umyjpqvfkt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#umyjpqvfkt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#umyjpqvfkt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umyjpqvfkt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umyjpqvfkt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#umyjpqvfkt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#umyjpqvfkt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#umyjpqvfkt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umyjpqvfkt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#umyjpqvfkt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umyjpqvfkt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#umyjpqvfkt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umyjpqvfkt .gt_left {
  text-align: left;
}

#umyjpqvfkt .gt_center {
  text-align: center;
}

#umyjpqvfkt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#umyjpqvfkt .gt_font_normal {
  font-weight: normal;
}

#umyjpqvfkt .gt_font_bold {
  font-weight: bold;
}

#umyjpqvfkt .gt_font_italic {
  font-style: italic;
}

#umyjpqvfkt .gt_super {
  font-size: 65%;
}

#umyjpqvfkt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#umyjpqvfkt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#umyjpqvfkt .gt_indent_1 {
  text-indent: 5px;
}

#umyjpqvfkt .gt_indent_2 {
  text-indent: 10px;
}

#umyjpqvfkt .gt_indent_3 {
  text-indent: 15px;
}

#umyjpqvfkt .gt_indent_4 {
  text-indent: 20px;
}

#umyjpqvfkt .gt_indent_5 {
  text-indent: 25px;
}

#umyjpqvfkt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#umyjpqvfkt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>CL_Sep_3C_6</strong><br />
N = 87</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>L_mSep_2dC_11</strong><br />
N = 59</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_3"><span class='gt_from_md'><strong>L_Sep_1C_1</strong><br />
N = 7</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_4"><span class='gt_from_md'><strong>L_Sep_2dC_7</strong><br />
N = 51</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_5"><span class='gt_from_md'><strong>L_Sep_2R_4</strong><br />
N = 57</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">treatment, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    base</td>
<td headers="stat_1" class="gt_row gt_center">45 (52%)</td>
<td headers="stat_2" class="gt_row gt_center">28 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">41 (80%)</td>
<td headers="stat_5" class="gt_row gt_center">20 (35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    efferon LPS</td>
<td headers="stat_1" class="gt_row gt_center">21 (24%)</td>
<td headers="stat_2" class="gt_row gt_center">31 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">7 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">10 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">37 (65%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    efferon CT</td>
<td headers="stat_1" class="gt_row gt_center">21 (24%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_5" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Sex, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    female</td>
<td headers="stat_1" class="gt_row gt_center">23 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">59 (100%)</td>
<td headers="stat_3" class="gt_row gt_center">2 (29%)</td>
<td headers="stat_4" class="gt_row gt_center">27 (53%)</td>
<td headers="stat_5" class="gt_row gt_center">29 (51%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    male</td>
<td headers="stat_1" class="gt_row gt_center">64 (74%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_3" class="gt_row gt_center">5 (71%)</td>
<td headers="stat_4" class="gt_row gt_center">24 (47%)</td>
<td headers="stat_5" class="gt_row gt_center">28 (49%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age, years</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">47 ± 17</td>
<td headers="stat_2" class="gt_row gt_center">31 ± 7</td>
<td headers="stat_3" class="gt_row gt_center">54 ± 16</td>
<td headers="stat_4" class="gt_row gt_center">57 ± 18</td>
<td headers="stat_5" class="gt_row gt_center">59 ± 18</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">47 [33;61]</td>
<td headers="stat_2" class="gt_row gt_center">31 [26;37]</td>
<td headers="stat_3" class="gt_row gt_center">56 [38;66]</td>
<td headers="stat_4" class="gt_row gt_center">58 [41;70]</td>
<td headers="stat_5" class="gt_row gt_center">61 [43;74]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Weight, kg</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">51</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">8</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">85 ± 19</td>
<td headers="stat_2" class="gt_row gt_center">74 ± 14</td>
<td headers="stat_3" class="gt_row gt_center">82 ± 9</td>
<td headers="stat_4" class="gt_row gt_center">73 ± 17</td>
<td headers="stat_5" class="gt_row gt_center">85 ± 19</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">84 [70;95]</td>
<td headers="stat_2" class="gt_row gt_center">70 [66;80]</td>
<td headers="stat_3" class="gt_row gt_center">84 [74;90]</td>
<td headers="stat_4" class="gt_row gt_center">70 [60;80]</td>
<td headers="stat_5" class="gt_row gt_center">84 [70;98]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Height, cm</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">42</td>
<td headers="stat_2" class="gt_row gt_center">31</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">45</td>
<td headers="stat_2" class="gt_row gt_center">28</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">173 ± 8</td>
<td headers="stat_2" class="gt_row gt_center">162 ± 8</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">170 ± 5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">172 [169;180]</td>
<td headers="stat_2" class="gt_row gt_center">165 [160;167]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">170 [168;172]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">BMI, kg/m^2</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">30</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">29</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">28.1 ± 6.8</td>
<td headers="stat_2" class="gt_row gt_center">27.8 ± 5.6</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">29.4 ± 6.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">26.6 [23.1;30.9]</td>
<td headers="stat_2" class="gt_row gt_center">25.7 [24.7;29.4]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">29.0 [24.0;33.0]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">APACHE2</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">34</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">16</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">25</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">35</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">22 ± 6</td>
<td headers="stat_2" class="gt_row gt_center">18 ± 8</td>
<td headers="stat_3" class="gt_row gt_center">27 ± 1</td>
<td headers="stat_4" class="gt_row gt_center">18 ± 6</td>
<td headers="stat_5" class="gt_row gt_center">23 ± 3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">21 [18;26]</td>
<td headers="stat_2" class="gt_row gt_center">17 [12;20]</td>
<td headers="stat_3" class="gt_row gt_center">27 [26;28]</td>
<td headers="stat_4" class="gt_row gt_center">17 [13;22]</td>
<td headers="stat_5" class="gt_row gt_center">24 [22;25]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SOFA</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">9.7 ± 2.8</td>
<td headers="stat_2" class="gt_row gt_center">7.7 ± 3.5</td>
<td headers="stat_3" class="gt_row gt_center">9.6 ± 1.7</td>
<td headers="stat_4" class="gt_row gt_center">8.0 ± 3.5</td>
<td headers="stat_5" class="gt_row gt_center">8.3 ± 2.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">10.0 [7.0;12.0]</td>
<td headers="stat_2" class="gt_row gt_center">7.0 [5.0;9.0]</td>
<td headers="stat_3" class="gt_row gt_center">10.0 [8.0;11.0]</td>
<td headers="stat_4" class="gt_row gt_center">7.0 [6.0;11.0]</td>
<td headers="stat_5" class="gt_row gt_center">7.0 [7.0;10.0]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MAP, mmHg</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">85 ± 12</td>
<td headers="stat_2" class="gt_row gt_center">81 ± 16</td>
<td headers="stat_3" class="gt_row gt_center">50 ± 10</td>
<td headers="stat_4" class="gt_row gt_center">74 ± 20</td>
<td headers="stat_5" class="gt_row gt_center">66 ± 14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">85 [79;93]</td>
<td headers="stat_2" class="gt_row gt_center">82 [70;90]</td>
<td headers="stat_3" class="gt_row gt_center">46 [41;63]</td>
<td headers="stat_4" class="gt_row gt_center">72 [55;87]</td>
<td headers="stat_5" class="gt_row gt_center">63 [57;73]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PaO2, mmHg</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">56</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">35</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">3</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">16</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">111 ± 47</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">93 ± 31</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">99 [82;121]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">96 [72;98]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">FiO2, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">57</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">33</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">2</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">18</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">45 ± 18</td>
<td headers="stat_3" class="gt_row gt_center">40 ± 0</td>
<td headers="stat_4" class="gt_row gt_center">44 ± 8</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">40 [35;50]</td>
<td headers="stat_3" class="gt_row gt_center">40 [40;40]</td>
<td headers="stat_4" class="gt_row gt_center">40 [40;50]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PaO2/FiO2</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">58</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">33</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">18</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">328 ± 100</td>
<td headers="stat_2" class="gt_row gt_center">274 ± 131</td>
<td headers="stat_3" class="gt_row gt_center">249 ± 49</td>
<td headers="stat_4" class="gt_row gt_center">223 ± 70</td>
<td headers="stat_5" class="gt_row gt_center">270 ± 71</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">329 [266;383]</td>
<td headers="stat_2" class="gt_row gt_center">262 [166;360]</td>
<td headers="stat_3" class="gt_row gt_center">230 [215;295]</td>
<td headers="stat_4" class="gt_row gt_center">240 [172;245]</td>
<td headers="stat_5" class="gt_row gt_center">267 [225;290]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Lactate, mmol/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">47</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">2.99 ± 2.40</td>
<td headers="stat_3" class="gt_row gt_center">7.91 ± 4.45</td>
<td headers="stat_4" class="gt_row gt_center">3.87 ± 2.55</td>
<td headers="stat_5" class="gt_row gt_center">4.25 ± 3.15</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">2.60 [1.60;3.50]</td>
<td headers="stat_3" class="gt_row gt_center">8.10 [4.40;12.50]</td>
<td headers="stat_4" class="gt_row gt_center">3.30 [2.10;4.80]</td>
<td headers="stat_5" class="gt_row gt_center">3.10 [2.50;4.50]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VIS2020</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">50</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">1</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">68 ± 51</td>
<td headers="stat_2" class="gt_row gt_center">27 ± 55</td>
<td headers="stat_3" class="gt_row gt_center">55 ± 14</td>
<td headers="stat_4" class="gt_row gt_center">13 ± 22</td>
<td headers="stat_5" class="gt_row gt_center">66 ± 37</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">51 [35;84]</td>
<td headers="stat_2" class="gt_row gt_center">7 [0;36]</td>
<td headers="stat_3" class="gt_row gt_center">55 [40;70]</td>
<td headers="stat_4" class="gt_row gt_center">9 [0;16]</td>
<td headers="stat_5" class="gt_row gt_center">70 [30;90]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">WBC, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">19 ± 13</td>
<td headers="stat_2" class="gt_row gt_center">19 ± 10</td>
<td headers="stat_3" class="gt_row gt_center">26 ± 7</td>
<td headers="stat_4" class="gt_row gt_center">18 ± 11</td>
<td headers="stat_5" class="gt_row gt_center">16 ± 8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">15 [10;26]</td>
<td headers="stat_2" class="gt_row gt_center">17 [14;27]</td>
<td headers="stat_3" class="gt_row gt_center">23 [22;31]</td>
<td headers="stat_4" class="gt_row gt_center">17 [10;23]</td>
<td headers="stat_5" class="gt_row gt_center">16 [11;21]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEU, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">86</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">50</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">1</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">1</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">17 ± 12</td>
<td headers="stat_2" class="gt_row gt_center">17 ± 10</td>
<td headers="stat_3" class="gt_row gt_center">24 ± 6</td>
<td headers="stat_4" class="gt_row gt_center">16 ± 11</td>
<td headers="stat_5" class="gt_row gt_center">14 ± 8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">13 [8;22]</td>
<td headers="stat_2" class="gt_row gt_center">14 [12;24]</td>
<td headers="stat_3" class="gt_row gt_center">22 [20;28]</td>
<td headers="stat_4" class="gt_row gt_center">14 [8;21]</td>
<td headers="stat_5" class="gt_row gt_center">13 [9;20]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEUr, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">31</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">45</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">28</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">6</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">87 ± 8</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">84 ± 18</td>
<td headers="stat_5" class="gt_row gt_center">85 ± 9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">90 [83;93]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">90 [84;92]</td>
<td headers="stat_5" class="gt_row gt_center">87 [83;91]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYM, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">58</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">50</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">1</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">1.44 ± 1.06</td>
<td headers="stat_2" class="gt_row gt_center">1.32 ± 1.03</td>
<td headers="stat_3" class="gt_row gt_center">8.00 ± 3.27</td>
<td headers="stat_4" class="gt_row gt_center">1.49 ± 1.71</td>
<td headers="stat_5" class="gt_row gt_center">1.19 ± 0.87</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">1.20 [0.72;1.90]</td>
<td headers="stat_2" class="gt_row gt_center">0.91 [0.62;1.90]</td>
<td headers="stat_3" class="gt_row gt_center">8.00 [6.00;10.00]</td>
<td headers="stat_4" class="gt_row gt_center">1.01 [0.60;1.70]</td>
<td headers="stat_5" class="gt_row gt_center">1.06 [0.55;1.54]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYMr, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">31</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">43</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">28</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">8</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">6.8 ± 5.3</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">7.6 ± 6.3</td>
<td headers="stat_5" class="gt_row gt_center">8.6 ± 6.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">4.8 [2.7;9.7]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">5.0 [3.5;10.1]</td>
<td headers="stat_5" class="gt_row gt_center">6.3 [4.2;10.6]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PLT, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">180 ± 119</td>
<td headers="stat_2" class="gt_row gt_center">183 ± 143</td>
<td headers="stat_3" class="gt_row gt_center">270 ± 75</td>
<td headers="stat_4" class="gt_row gt_center">215 ± 138</td>
<td headers="stat_5" class="gt_row gt_center">245 ± 149</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">163 [91;234]</td>
<td headers="stat_2" class="gt_row gt_center">154 [82;240]</td>
<td headers="stat_3" class="gt_row gt_center">282 [207;329]</td>
<td headers="stat_4" class="gt_row gt_center">203 [110;290]</td>
<td headers="stat_5" class="gt_row gt_center">227 [131;331]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">aPTT, s</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">34 ± 11</td>
<td headers="stat_3" class="gt_row gt_center">37 ± 16</td>
<td headers="stat_4" class="gt_row gt_center">37 ± 26</td>
<td headers="stat_5" class="gt_row gt_center">34 ± 13</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">32 [28;38]</td>
<td headers="stat_3" class="gt_row gt_center">32 [29;34]</td>
<td headers="stat_4" class="gt_row gt_center">35 [27;40]</td>
<td headers="stat_5" class="gt_row gt_center">30 [25;38]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INR</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">44</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">15</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">1.24 ± 0.27</td>
<td headers="stat_3" class="gt_row gt_center">1.17 ± 0.19</td>
<td headers="stat_4" class="gt_row gt_center">1.54 ± 0.50</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">1.20 [1.07;1.38]</td>
<td headers="stat_3" class="gt_row gt_center">1.10 [1.03;1.22]</td>
<td headers="stat_4" class="gt_row gt_center">1.37 [1.22;1.70]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ATIII, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">22</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">2</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">37</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">49</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">54 ± 20</td>
<td headers="stat_2" class="gt_row gt_center">74 ± 25</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">26 ± 4</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">55 [41;65]</td>
<td headers="stat_2" class="gt_row gt_center">72 [59;89]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">26 [23;28]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Fibrinogen, g/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">58</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">49</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">2</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">5.30 ± 2.23</td>
<td headers="stat_2" class="gt_row gt_center">3.82 ± 2.24</td>
<td headers="stat_3" class="gt_row gt_center">4.90 ± 0.98</td>
<td headers="stat_4" class="gt_row gt_center">5.10 ± 2.37</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">5.15 [3.57;7.02]</td>
<td headers="stat_2" class="gt_row gt_center">3.50 [2.23;4.55]</td>
<td headers="stat_3" class="gt_row gt_center">5.10 [4.30;5.50]</td>
<td headers="stat_4" class="gt_row gt_center">4.51 [3.60;6.60]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Diuresis, ml</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">57</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">49</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">2</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">2</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">1,705 ± 1,701</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">820 ± 844</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">1,200 [604;2,200]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">600 [100;1,200]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Creatinine, mcmol/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">250 ± 142</td>
<td headers="stat_2" class="gt_row gt_center">154 ± 110</td>
<td headers="stat_3" class="gt_row gt_center">476 ± 480</td>
<td headers="stat_4" class="gt_row gt_center">892 ± 4,629</td>
<td headers="stat_5" class="gt_row gt_center">219 ± 182</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">230 [135;328]</td>
<td headers="stat_2" class="gt_row gt_center">103 [70;235]</td>
<td headers="stat_3" class="gt_row gt_center">345 [210;452]</td>
<td headers="stat_4" class="gt_row gt_center">201 [99;407]</td>
<td headers="stat_5" class="gt_row gt_center">166 [106;261]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Urea, mcmol/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">58</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">10 ± 7</td>
<td headers="stat_3" class="gt_row gt_center">28 ± 19</td>
<td headers="stat_4" class="gt_row gt_center">16 ± 11</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">7 [4;14]</td>
<td headers="stat_3" class="gt_row gt_center">23 [21;32]</td>
<td headers="stat_4" class="gt_row gt_center">14 [7;20]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total protein, g/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">42</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">45</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">54 ± 9</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">63 ± 15</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">54 [48;60]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">63 [54;79]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Albumin, g/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">59</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">26.1 ± 5.8</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">25.8 [22.2;30.5]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total bilirubin, μmol/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">58</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">50</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">1</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">32 ± 39</td>
<td headers="stat_2" class="gt_row gt_center">31 ± 37</td>
<td headers="stat_3" class="gt_row gt_center">14 ± 16</td>
<td headers="stat_4" class="gt_row gt_center">53 ± 77</td>
<td headers="stat_5" class="gt_row gt_center">29 ± 61</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">19 [12;40]</td>
<td headers="stat_2" class="gt_row gt_center">15 [10;29]</td>
<td headers="stat_3" class="gt_row gt_center">9 [6;12]</td>
<td headers="stat_4" class="gt_row gt_center">28 [12;61]</td>
<td headers="stat_5" class="gt_row gt_center">18 [9;26]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Direct bilirubin, μmol/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">49</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">38</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">10</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">13</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">18 ± 27</td>
<td headers="stat_3" class="gt_row gt_center">5 ± 7</td>
<td headers="stat_4" class="gt_row gt_center">34 ± 60</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">7 [3;17]</td>
<td headers="stat_3" class="gt_row gt_center">2 [1;5]</td>
<td headers="stat_4" class="gt_row gt_center">10 [3;39]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AST, U/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">56</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">48</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">3</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">3</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">197 ± 778</td>
<td headers="stat_3" class="gt_row gt_center">68 ± 81</td>
<td headers="stat_4" class="gt_row gt_center">104 ± 117</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">38 [26;75]</td>
<td headers="stat_3" class="gt_row gt_center">35 [22;101]</td>
<td headers="stat_4" class="gt_row gt_center">56 [20;150]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ALT, U/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">56</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">50</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">3</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">1</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">119 ± 484</td>
<td headers="stat_3" class="gt_row gt_center">41 ± 26</td>
<td headers="stat_4" class="gt_row gt_center">76 ± 97</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">21 [13;49]</td>
<td headers="stat_3" class="gt_row gt_center">32 [22;72]</td>
<td headers="stat_4" class="gt_row gt_center">36 [17;109]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LDH, U/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">44</td>
<td headers="stat_3" class="gt_row gt_center">6</td>
<td headers="stat_4" class="gt_row gt_center">8</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">15</td>
<td headers="stat_3" class="gt_row gt_center">1</td>
<td headers="stat_4" class="gt_row gt_center">43</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">727 ± 787</td>
<td headers="stat_3" class="gt_row gt_center">345 ± 323</td>
<td headers="stat_4" class="gt_row gt_center">228 ± 341</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">442 [319;763]</td>
<td headers="stat_3" class="gt_row gt_center">293 [58;456]</td>
<td headers="stat_4" class="gt_row gt_center">77 [0;365]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">CRP, mg/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">52</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">48</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">7</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">3</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">205 ± 118</td>
<td headers="stat_2" class="gt_row gt_center">221 ± 394</td>
<td headers="stat_3" class="gt_row gt_center">353 ± 47</td>
<td headers="stat_4" class="gt_row gt_center">189 ± 125</td>
<td headers="stat_5" class="gt_row gt_center">246 ± 100</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">200 [113;267]</td>
<td headers="stat_2" class="gt_row gt_center">148 [90;214]</td>
<td headers="stat_3" class="gt_row gt_center">324 [316;412]</td>
<td headers="stat_4" class="gt_row gt_center">171 [92;299]</td>
<td headers="stat_5" class="gt_row gt_center">231 [168;325]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PCT, μg/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">48</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">44</td>
<td headers="stat_5" class="gt_row gt_center">56</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">11</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">7</td>
<td headers="stat_5" class="gt_row gt_center">1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">45 ± 67</td>
<td headers="stat_2" class="gt_row gt_center">24 ± 67</td>
<td headers="stat_3" class="gt_row gt_center">9 ± 3</td>
<td headers="stat_4" class="gt_row gt_center">26 ± 34</td>
<td headers="stat_5" class="gt_row gt_center">27 ± 38</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">12 [5;77]</td>
<td headers="stat_2" class="gt_row gt_center">5 [1;20]</td>
<td headers="stat_3" class="gt_row gt_center">8 [7;10]</td>
<td headers="stat_4" class="gt_row gt_center">8 [2;44]</td>
<td headers="stat_5" class="gt_row gt_center">12 [4;31]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ferritin, μg/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">86</td>
<td headers="stat_2" class="gt_row gt_center">21</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">12</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">1</td>
<td headers="stat_2" class="gt_row gt_center">38</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">39</td>
<td headers="stat_5" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">1,074 ± 1,535</td>
<td headers="stat_2" class="gt_row gt_center">288 ± 276</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">376 ± 414</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">790 [396;1,186]</td>
<td headers="stat_2" class="gt_row gt_center">230 [137;335]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">212 [49;797]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL-6, pg/mL</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">30</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">54</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">29</td>
<td headers="stat_3" class="gt_row gt_center">7</td>
<td headers="stat_4" class="gt_row gt_center">51</td>
<td headers="stat_5" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">2,764 ± 9,198</td>
<td headers="stat_2" class="gt_row gt_center">626 ± 1,617</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">1,293 ± 1,772</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">347 [165;2,342]</td>
<td headers="stat_2" class="gt_row gt_center">152 [50;250]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">542 [111;1,687]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_T0, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="stat_1" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">26 (44%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">9 (18%)</td>
<td headers="stat_5" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">87 (100%)</td>
<td headers="stat_2" class="gt_row gt_center">33 (56%)</td>
<td headers="stat_3" class="gt_row gt_center">7 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">42 (82%)</td>
<td headers="stat_5" class="gt_row gt_center">57 (100%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_T0, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="stat_1" class="gt_row gt_center">5 (5.8%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (29%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">33 (65%)</td>
<td headers="stat_5" class="gt_row gt_center">9 (16%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">81 (94%)</td>
<td headers="stat_2" class="gt_row gt_center">42 (71%)</td>
<td headers="stat_3" class="gt_row gt_center">7 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">18 (35%)</td>
<td headers="stat_5" class="gt_row gt_center">48 (84%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_T0, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="stat_1" class="gt_row gt_center">17 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">24 (41%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">13 (25%)</td>
<td headers="stat_5" class="gt_row gt_center">17 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">70 (80%)</td>
<td headers="stat_2" class="gt_row gt_center">35 (59%)</td>
<td headers="stat_3" class="gt_row gt_center">7 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">38 (75%)</td>
<td headers="stat_5" class="gt_row gt_center">40 (70%)</td></tr>
  </tbody>
  
  
</table>
</div>
```


