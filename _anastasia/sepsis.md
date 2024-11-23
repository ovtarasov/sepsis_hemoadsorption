---
title: "sepsis_project"
author: "Anastasiia Potamoshneva"
date: "2024-11-23"
output: 
  html_document:
    keep_md: true
---



# Reading data





# Adjusting variables to an appropriate format


``` r
# Changing into factor

sepsis <- sepsis %>% 
  ungroup() %>% 
  mutate(
  across(c(study_id, time_point, sex, shock_T0, MV_T0, AKI_T0, shock_first_end_status, ICU_first_end_status, MV_first_end_status), ~ as.factor(.x))) 

# Convert height to cm

sepsis <- sepsis %>%
  mutate(
    height = ifelse(study_id == "CL_Sep_3C_6", height * 100, height)
  )
```




# Renaming variables


``` r
#Renaming columns

sepsis <- sepsis %>% 
  rename_with(function(x) x %>% 
                str_replace("age", "Age, years") %>% 
                str_replace("sex", "Sex") %>% 
                str_replace("weight", "Weight, kg") %>%
                str_replace("height", "Height, cm") %>%
                str_replace("time_point", "Time point, days") %>% 
                str_replace("BMI", "BMI, kg/m^2") %>% 
                str_replace("age", "Age, years") %>% 
                str_replace("PaO2", "PaO2, mmHg") %>% 
                str_replace("FiO2", "FiO2, %") %>% 
                str_replace("lactate", "Lactate, mmol/L") %>% 
                str_replace("PaO2$", "PaO2, mmHg") %>% 
                str_replace("WBC", "WBC, *10^9/L") %>% 
                str_replace("^NEU$", "NEU, *10^9/L") %>% 
                str_replace("^NEUr$", "NEUr, %") %>% 
                str_replace("LYM$", "LYM, *10^9/L") %>% 
                str_replace("LYMr$", "LYMr, %") %>% 
                str_replace("PLT", "PLT, *10^9/L") %>% 
                str_replace("aPTT", "aPTT, s") %>% 
                str_replace("ATIII", "ATIII, %") %>% 
                str_replace("fibrinogen", "Fibrinogen, g/L") %>% 
                str_replace("DDImer", "DDimer, ng/mL") %>% 
                str_replace("diuresis", "Diuresis, ml") %>% 
                str_replace("creatinine", "Creatinine, mcmol/L") 
                
                
              )
              

# Renaming values in columns

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
sepsis  %>% 
  mutate(
  combined_efferon = case_when(
    treatment %in% c("efferon LPS", "efferon CT")~"Efferon",
    treatment == "base" ~ "Control"),
  combined_efferon = factor(combined_efferon, levels = c("Efferon", "Control"))
) %>% 
  select(everything(), -c(study_id, study_patient_id, treatment, "Time point, days", EAA)) %>% 
 
  tbl_summary(by=combined_efferon, 
              statistic = list(all_continuous() ~ c("{median} [{p25};{p75}]")),
              missing = "no") %>% 
  modify_table_body(
    filter, !(paste(variable, label) %in% c(
      "shock_T0 FALSE",
      "MV_T0 FALSE",
      "AKI_T0 FALSE"
    ))) %>% 
  add_p()
```

```
## 4 missing rows in the "combined_efferon" column have been removed.
## The following errors were returned during `add_p()`:
## ✖ For variable `IL_10` (`combined_efferon`) and "statistic" and "p.value"
##   statistics: группирующий фактор должен иметь в точности 2 уровня
## ✖ For variable `IL_1b` (`combined_efferon`) and "statistic" and "p.value"
##   statistics: группирующий фактор должен иметь в точности 2 уровня
## ✖ For variable `IL_8` (`combined_efferon`) and "statistic" and "p.value"
##   statistics: группирующий фактор должен иметь в точности 2 уровня
## ✖ For variable `protein_total` (`combined_efferon`) and "statistic" and
##   "p.value" statistics: группирующий фактор должен иметь в точности 2 уровня
```

```{=html}
<div id="sscjqthqed" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#sscjqthqed table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#sscjqthqed thead, #sscjqthqed tbody, #sscjqthqed tfoot, #sscjqthqed tr, #sscjqthqed td, #sscjqthqed th {
  border-style: none;
}

#sscjqthqed p {
  margin: 0;
  padding: 0;
}

#sscjqthqed .gt_table {
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

#sscjqthqed .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#sscjqthqed .gt_title {
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

#sscjqthqed .gt_subtitle {
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

#sscjqthqed .gt_heading {
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

#sscjqthqed .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sscjqthqed .gt_col_headings {
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

#sscjqthqed .gt_col_heading {
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

#sscjqthqed .gt_column_spanner_outer {
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

#sscjqthqed .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#sscjqthqed .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#sscjqthqed .gt_column_spanner {
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

#sscjqthqed .gt_spanner_row {
  border-bottom-style: hidden;
}

#sscjqthqed .gt_group_heading {
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

#sscjqthqed .gt_empty_group_heading {
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

#sscjqthqed .gt_from_md > :first-child {
  margin-top: 0;
}

#sscjqthqed .gt_from_md > :last-child {
  margin-bottom: 0;
}

#sscjqthqed .gt_row {
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

#sscjqthqed .gt_stub {
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

#sscjqthqed .gt_stub_row_group {
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

#sscjqthqed .gt_row_group_first td {
  border-top-width: 2px;
}

#sscjqthqed .gt_row_group_first th {
  border-top-width: 2px;
}

#sscjqthqed .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sscjqthqed .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#sscjqthqed .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#sscjqthqed .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sscjqthqed .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sscjqthqed .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#sscjqthqed .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#sscjqthqed .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#sscjqthqed .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sscjqthqed .gt_footnotes {
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

#sscjqthqed .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#sscjqthqed .gt_sourcenotes {
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

#sscjqthqed .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#sscjqthqed .gt_left {
  text-align: left;
}

#sscjqthqed .gt_center {
  text-align: center;
}

#sscjqthqed .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#sscjqthqed .gt_font_normal {
  font-weight: normal;
}

#sscjqthqed .gt_font_bold {
  font-weight: bold;
}

#sscjqthqed .gt_font_italic {
  font-style: italic;
}

#sscjqthqed .gt_super {
  font-size: 65%;
}

#sscjqthqed .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#sscjqthqed .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#sscjqthqed .gt_indent_1 {
  text-indent: 5px;
}

#sscjqthqed .gt_indent_2 {
  text-indent: 10px;
}

#sscjqthqed .gt_indent_3 {
  text-indent: 15px;
}

#sscjqthqed .gt_indent_4 {
  text-indent: 20px;
}

#sscjqthqed .gt_indent_5 {
  text-indent: 25px;
}

#sscjqthqed .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#sscjqthqed div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>Efferon</strong><br />
N = 645</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Control</strong><br />
N = 665</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Sex</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    female</td>
<td headers="stat_1" class="gt_row gt_center">412 (64%)</td>
<td headers="stat_2" class="gt_row gt_center">340 (52%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    male</td>
<td headers="stat_1" class="gt_row gt_center">233 (36%)</td>
<td headers="stat_2" class="gt_row gt_center">313 (48%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age, years</td>
<td headers="stat_1" class="gt_row gt_center">41 [32;63]</td>
<td headers="stat_2" class="gt_row gt_center">46 [34;65]</td>
<td headers="p.value" class="gt_row gt_center">0.049</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Weight, kg</td>
<td headers="stat_1" class="gt_row gt_center">75 [68;90]</td>
<td headers="stat_2" class="gt_row gt_center">78 [66;90]</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Height, cm</td>
<td headers="stat_1" class="gt_row gt_center">168 [164;173]</td>
<td headers="stat_2" class="gt_row gt_center">171 [170;172]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">BMI, kg/m^2</td>
<td headers="stat_1" class="gt_row gt_center">26.3 [24.0;32.1]</td>
<td headers="stat_2" class="gt_row gt_center">29.0 [23.0;32.5]</td>
<td headers="p.value" class="gt_row gt_center">0.037</td></tr>
    <tr><td headers="label" class="gt_row gt_left">APACHE2</td>
<td headers="stat_1" class="gt_row gt_center">20 [16;24]</td>
<td headers="stat_2" class="gt_row gt_center">22 [16;26]</td>
<td headers="p.value" class="gt_row gt_center">0.005</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SOFA</td>
<td headers="stat_1" class="gt_row gt_center">6.0 [3.0;9.0]</td>
<td headers="stat_2" class="gt_row gt_center">8.0 [4.5;11.0]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MAP</td>
<td headers="stat_1" class="gt_row gt_center">85 [76;93]</td>
<td headers="stat_2" class="gt_row gt_center">83 [73;93]</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PaO2, mmHg</td>
<td headers="stat_1" class="gt_row gt_center">100 [90;125]</td>
<td headers="stat_2" class="gt_row gt_center">98 [85;106]</td>
<td headers="p.value" class="gt_row gt_center">0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">FiO2, %</td>
<td headers="stat_1" class="gt_row gt_center">30 [21;40]</td>
<td headers="stat_2" class="gt_row gt_center">40 [31;50]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PF_ratio</td>
<td headers="stat_1" class="gt_row gt_center">322 [279;395]</td>
<td headers="stat_2" class="gt_row gt_center">280 [213;375]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Lactate, mmol/L</td>
<td headers="stat_1" class="gt_row gt_center">2.10 [1.30;3.10]</td>
<td headers="stat_2" class="gt_row gt_center">2.42 [1.80;3.55]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VIS2020</td>
<td headers="stat_1" class="gt_row gt_center">1 [0;30]</td>
<td headers="stat_2" class="gt_row gt_center">6 [0;28]</td>
<td headers="p.value" class="gt_row gt_center">0.11</td></tr>
    <tr><td headers="label" class="gt_row gt_left">WBC, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">14 [10;21]</td>
<td headers="stat_2" class="gt_row gt_center">14 [9;21]</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEU, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">12 [8;18]</td>
<td headers="stat_2" class="gt_row gt_center">12 [7;18]</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEUr, %</td>
<td headers="stat_1" class="gt_row gt_center">85 [75;90]</td>
<td headers="stat_2" class="gt_row gt_center">87 [80;91]</td>
<td headers="p.value" class="gt_row gt_center">0.005</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYM, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">1.37 [0.85;2.06]</td>
<td headers="stat_2" class="gt_row gt_center">1.15 [0.75;1.71]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYMr, %</td>
<td headers="stat_1" class="gt_row gt_center">9 [5;15]</td>
<td headers="stat_2" class="gt_row gt_center">7 [5;12]</td>
<td headers="p.value" class="gt_row gt_center">0.041</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PLT, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center">157 [94;254]</td>
<td headers="stat_2" class="gt_row gt_center">153 [87;241]</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">aPTT, s</td>
<td headers="stat_1" class="gt_row gt_center">32 [27;42]</td>
<td headers="stat_2" class="gt_row gt_center">32 [28;38]</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INR</td>
<td headers="stat_1" class="gt_row gt_center">1.19 [1.05;1.34]</td>
<td headers="stat_2" class="gt_row gt_center">1.24 [1.12;1.57]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ATIII, %</td>
<td headers="stat_1" class="gt_row gt_center">63 [54;76]</td>
<td headers="stat_2" class="gt_row gt_center">47 [28;64]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Fibrinogen, g/L</td>
<td headers="stat_1" class="gt_row gt_center">4.10 [2.74;5.60]</td>
<td headers="stat_2" class="gt_row gt_center">4.10 [3.00;5.37]</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">DDimer</td>
<td headers="stat_1" class="gt_row gt_center">1,649 [10;6,459]</td>
<td headers="stat_2" class="gt_row gt_center">3,260 [802;10,001]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Diuresis, ml</td>
<td headers="stat_1" class="gt_row gt_center">1,750 [800;2,700]</td>
<td headers="stat_2" class="gt_row gt_center">2,000 [700;3,000]</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Creatinine, mcmol/L</td>
<td headers="stat_1" class="gt_row gt_center">135 [80;214]</td>
<td headers="stat_2" class="gt_row gt_center">143 [80;306]</td>
<td headers="p.value" class="gt_row gt_center">0.004</td></tr>
    <tr><td headers="label" class="gt_row gt_left">urea</td>
<td headers="stat_1" class="gt_row gt_center">8 [4;13]</td>
<td headers="stat_2" class="gt_row gt_center">8 [5;17]</td>
<td headers="p.value" class="gt_row gt_center">0.017</td></tr>
    <tr><td headers="label" class="gt_row gt_left">protein_total</td>
<td headers="stat_1" class="gt_row gt_center">52 [48;56]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">albumin</td>
<td headers="stat_1" class="gt_row gt_center">27.4 [24.4;30.5]</td>
<td headers="stat_2" class="gt_row gt_center">23.4 [20.3;28.2]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">bilirubin_total</td>
<td headers="stat_1" class="gt_row gt_center">18 [9;31]</td>
<td headers="stat_2" class="gt_row gt_center">15 [9;36]</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">bilirubin_direct</td>
<td headers="stat_1" class="gt_row gt_center">6 [3;16]</td>
<td headers="stat_2" class="gt_row gt_center">5 [2;16]</td>
<td headers="p.value" class="gt_row gt_center">0.043</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AST</td>
<td headers="stat_1" class="gt_row gt_center">31 [18;58]</td>
<td headers="stat_2" class="gt_row gt_center">39 [21;84]</td>
<td headers="p.value" class="gt_row gt_center">0.010</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ALT</td>
<td headers="stat_1" class="gt_row gt_center">22 [12;57]</td>
<td headers="stat_2" class="gt_row gt_center">23 [14;54]</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LDH</td>
<td headers="stat_1" class="gt_row gt_center">374 [241;544]</td>
<td headers="stat_2" class="gt_row gt_center">437 [322;705]</td>
<td headers="p.value" class="gt_row gt_center">0.020</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ferritin</td>
<td headers="stat_1" class="gt_row gt_center">459 [180;837]</td>
<td headers="stat_2" class="gt_row gt_center">701 [272;1,139]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">presepsin</td>
<td headers="stat_1" class="gt_row gt_center">1,067 [423;3,021]</td>
<td headers="stat_2" class="gt_row gt_center">77 [77;213]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_1b</td>
<td headers="stat_1" class="gt_row gt_center">2.7 [1.6;5.1]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_6</td>
<td headers="stat_1" class="gt_row gt_center">219 [82;936]</td>
<td headers="stat_2" class="gt_row gt_center">365 [155;1,857]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_8</td>
<td headers="stat_1" class="gt_row gt_center">38 [21;99]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_10</td>
<td headers="stat_1" class="gt_row gt_center">19 [6;40]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">TNFa</td>
<td headers="stat_1" class="gt_row gt_center">3 [1;11]</td>
<td headers="stat_2" class="gt_row gt_center">1 [0;4]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ig_A</td>
<td headers="stat_1" class="gt_row gt_center">233 [191;302]</td>
<td headers="stat_2" class="gt_row gt_center">148 [93;276]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ig_G</td>
<td headers="stat_1" class="gt_row gt_center">682 [628;780]</td>
<td headers="stat_2" class="gt_row gt_center">595 [442;657]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ig_M</td>
<td headers="stat_1" class="gt_row gt_center">86 [66;108]</td>
<td headers="stat_2" class="gt_row gt_center">63 [46;87]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.036</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">556 (86%)</td>
<td headers="stat_2" class="gt_row gt_center">535 (82%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">507 (79%)</td>
<td headers="stat_2" class="gt_row gt_center">444 (68%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.077</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">477 (74%)</td>
<td headers="stat_2" class="gt_row gt_center">454 (70%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center">3.6 [2.0;5.8]</td>
<td headers="stat_2" class="gt_row gt_center">3.0 [2.0;7.0]</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">103 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">243 (45%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">408 (80%)</td>
<td headers="stat_2" class="gt_row gt_center">292 (55%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center">2.5 [0.6;6.5]</td>
<td headers="stat_2" class="gt_row gt_center">3.0 [1.5;6.3]</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">67 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">168 (54%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">234 (69%)</td>
<td headers="stat_2" class="gt_row gt_center">134 (43%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    censored</td>
<td headers="stat_1" class="gt_row gt_center">36 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">12 (3.8%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">ICU_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center">8 [5;15]</td>
<td headers="stat_2" class="gt_row gt_center">7 [3;14]</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ICU_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">218 (34%)</td>
<td headers="stat_2" class="gt_row gt_center">385 (59%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">427 (66%)</td>
<td headers="stat_2" class="gt_row gt_center">268 (41%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center">5 [2;9]</td>
<td headers="stat_2" class="gt_row gt_center">4 [2;12]</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">98 (19%)</td>
<td headers="stat_2" class="gt_row gt_center">194 (44%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">409 (81%)</td>
<td headers="stat_2" class="gt_row gt_center">250 (56%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n (%); Median [Q1;Q3]</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Pearson’s Chi-squared test; Wilcoxon rank sum test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


# Descriptive stats for numeric variables


``` r
sepsis %>%
  select(where(is.numeric), study_id) %>% 
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
<div id="uqccoequot" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uqccoequot table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#uqccoequot thead, #uqccoequot tbody, #uqccoequot tfoot, #uqccoequot tr, #uqccoequot td, #uqccoequot th {
  border-style: none;
}

#uqccoequot p {
  margin: 0;
  padding: 0;
}

#uqccoequot .gt_table {
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

#uqccoequot .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#uqccoequot .gt_title {
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

#uqccoequot .gt_subtitle {
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

#uqccoequot .gt_heading {
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

#uqccoequot .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uqccoequot .gt_col_headings {
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

#uqccoequot .gt_col_heading {
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

#uqccoequot .gt_column_spanner_outer {
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

#uqccoequot .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uqccoequot .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uqccoequot .gt_column_spanner {
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

#uqccoequot .gt_spanner_row {
  border-bottom-style: hidden;
}

#uqccoequot .gt_group_heading {
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

#uqccoequot .gt_empty_group_heading {
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

#uqccoequot .gt_from_md > :first-child {
  margin-top: 0;
}

#uqccoequot .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uqccoequot .gt_row {
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

#uqccoequot .gt_stub {
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

#uqccoequot .gt_stub_row_group {
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

#uqccoequot .gt_row_group_first td {
  border-top-width: 2px;
}

#uqccoequot .gt_row_group_first th {
  border-top-width: 2px;
}

#uqccoequot .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uqccoequot .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#uqccoequot .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#uqccoequot .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uqccoequot .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uqccoequot .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uqccoequot .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#uqccoequot .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uqccoequot .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uqccoequot .gt_footnotes {
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

#uqccoequot .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uqccoequot .gt_sourcenotes {
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

#uqccoequot .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uqccoequot .gt_left {
  text-align: left;
}

#uqccoequot .gt_center {
  text-align: center;
}

#uqccoequot .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uqccoequot .gt_font_normal {
  font-weight: normal;
}

#uqccoequot .gt_font_bold {
  font-weight: bold;
}

#uqccoequot .gt_font_italic {
  font-style: italic;
}

#uqccoequot .gt_super {
  font-size: 65%;
}

#uqccoequot .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#uqccoequot .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#uqccoequot .gt_indent_1 {
  text-indent: 5px;
}

#uqccoequot .gt_indent_2 {
  text-indent: 10px;
}

#uqccoequot .gt_indent_3 {
  text-indent: 15px;
}

#uqccoequot .gt_indent_4 {
  text-indent: 20px;
}

#uqccoequot .gt_indent_5 {
  text-indent: 25px;
}

#uqccoequot .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#uqccoequot div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>CL_Sep_3C_6</strong><br />
N = 364</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>L_mSep_2dC_11</strong><br />
N = 360</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_3"><span class='gt_from_md'><strong>L_Sep_1C_1</strong><br />
N = 45</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_4"><span class='gt_from_md'><strong>L_Sep_2dC_7</strong><br />
N = 255</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_5"><span class='gt_from_md'><strong>L_Sep_2R_4</strong><br />
N = 290</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age, years</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">348</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">47 ± 17</td>
<td headers="stat_2" class="gt_row gt_center">59 ± 230</td>
<td headers="stat_3" class="gt_row gt_center">58 ± 15</td>
<td headers="stat_4" class="gt_row gt_center">57 ± 18</td>
<td headers="stat_5" class="gt_row gt_center">58 ± 18</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">47 [33;61]</td>
<td headers="stat_2" class="gt_row gt_center">32 [26;37]</td>
<td headers="stat_3" class="gt_row gt_center">63 [52;68]</td>
<td headers="stat_4" class="gt_row gt_center">58 [41;70]</td>
<td headers="stat_5" class="gt_row gt_center">60 [42;74]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Weight, kg</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">348</td>
<td headers="stat_2" class="gt_row gt_center">318</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">42</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">85 ± 19</td>
<td headers="stat_2" class="gt_row gt_center">73 ± 16</td>
<td headers="stat_3" class="gt_row gt_center">78 ± 11</td>
<td headers="stat_4" class="gt_row gt_center">73 ± 17</td>
<td headers="stat_5" class="gt_row gt_center">84 ± 19</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">84 [70;95]</td>
<td headers="stat_2" class="gt_row gt_center">70 [66;80]</td>
<td headers="stat_3" class="gt_row gt_center">76 [74;87]</td>
<td headers="stat_4" class="gt_row gt_center">70 [60;80]</td>
<td headers="stat_5" class="gt_row gt_center">84 [69;98]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Height, cm</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">168</td>
<td headers="stat_2" class="gt_row gt_center">192</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">196</td>
<td headers="stat_2" class="gt_row gt_center">168</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">1,373 ± 4,339</td>
<td headers="stat_2" class="gt_row gt_center">160 ± 14</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">170 ± 5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">175 [170;180]</td>
<td headers="stat_2" class="gt_row gt_center">165 [159;167]</td>
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
<td headers="stat_1" class="gt_row gt_center">348</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">27.9 ± 7.3</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">29.3 ± 6.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">26.6 [23.1;30.9]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
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
<td headers="stat_1" class="gt_row gt_center">348</td>
<td headers="stat_2" class="gt_row gt_center">204</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">80</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">156</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">175</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">21 ± 8</td>
<td headers="stat_2" class="gt_row gt_center">18 ± 8</td>
<td headers="stat_3" class="gt_row gt_center">20 ± 6</td>
<td headers="stat_4" class="gt_row gt_center">18 ± 6</td>
<td headers="stat_5" class="gt_row gt_center">24 ± 3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">21 [15;25]</td>
<td headers="stat_2" class="gt_row gt_center">17 [12;20]</td>
<td headers="stat_3" class="gt_row gt_center">20 [14;24]</td>
<td headers="stat_4" class="gt_row gt_center">16 [13;22]</td>
<td headers="stat_5" class="gt_row gt_center">24 [22;26]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SOFA</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">348</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">196</td>
<td headers="stat_5" class="gt_row gt_center">254</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">59</td>
<td headers="stat_5" class="gt_row gt_center">36</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">8.9 ± 3.4</td>
<td headers="stat_2" class="gt_row gt_center">5.5 ± 4.3</td>
<td headers="stat_3" class="gt_row gt_center">4.7 ± 3.1</td>
<td headers="stat_4" class="gt_row gt_center">7.5 ± 4.4</td>
<td headers="stat_5" class="gt_row gt_center">6.5 ± 3.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">9.0 [6.3;11.0]</td>
<td headers="stat_2" class="gt_row gt_center">5.0 [2.0;8.0]</td>
<td headers="stat_3" class="gt_row gt_center">4.0 [2.0;6.0]</td>
<td headers="stat_4" class="gt_row gt_center">8.0 [4.0;11.0]</td>
<td headers="stat_5" class="gt_row gt_center">6.0 [3.0;9.0]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MAP</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">348</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">195</td>
<td headers="stat_5" class="gt_row gt_center">215</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">60</td>
<td headers="stat_5" class="gt_row gt_center">75</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">87 ± 10</td>
<td headers="stat_2" class="gt_row gt_center">86 ± 13</td>
<td headers="stat_3" class="gt_row gt_center">72 ± 29</td>
<td headers="stat_4" class="gt_row gt_center">79 ± 17</td>
<td headers="stat_5" class="gt_row gt_center">78 ± 18</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">88 [82;94]</td>
<td headers="stat_2" class="gt_row gt_center">85 [79;94]</td>
<td headers="stat_3" class="gt_row gt_center">86 [59;93]</td>
<td headers="stat_4" class="gt_row gt_center">78 [67;90]</td>
<td headers="stat_5" class="gt_row gt_center">80 [65;91]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PaO2, mmHg</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">330</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">127</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">30</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">128</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">110 ± 40</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">92 ± 25</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">100 [90;120]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">97 [81;98]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">FiO2, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">336</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">122</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">24</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">133</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">38 ± 15</td>
<td headers="stat_3" class="gt_row gt_center">36 ± 7</td>
<td headers="stat_4" class="gt_row gt_center">42 ± 14</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">39 [23;49]</td>
<td headers="stat_3" class="gt_row gt_center">40 [30;40]</td>
<td headers="stat_4" class="gt_row gt_center">40 [30;50]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PF_ratio</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">342</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">122</td>
<td headers="stat_5" class="gt_row gt_center">216</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">18</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">133</td>
<td headers="stat_5" class="gt_row gt_center">74</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">348 ± 93</td>
<td headers="stat_2" class="gt_row gt_center">323 ± 120</td>
<td headers="stat_3" class="gt_row gt_center">305 ± 49</td>
<td headers="stat_4" class="gt_row gt_center">258 ± 122</td>
<td headers="stat_5" class="gt_row gt_center">282 ± 70</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">350 [300;409]</td>
<td headers="stat_2" class="gt_row gt_center">310 [244;410]</td>
<td headers="stat_3" class="gt_row gt_center">310 [280;340]</td>
<td headers="stat_4" class="gt_row gt_center">242 [161;326]</td>
<td headers="stat_5" class="gt_row gt_center">285 [235;316]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Lactate, mmol/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">270</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">195</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">90</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">60</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">2.46 ± 1.86</td>
<td headers="stat_3" class="gt_row gt_center">3.77 ± 3.36</td>
<td headers="stat_4" class="gt_row gt_center">3.25 ± 2.58</td>
<td headers="stat_5" class="gt_row gt_center">3.17 ± 2.60</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">2.00 [1.20;3.10]</td>
<td headers="stat_3" class="gt_row gt_center">2.10 [1.30;4.90]</td>
<td headers="stat_4" class="gt_row gt_center">2.20 [1.70;3.80]</td>
<td headers="stat_5" class="gt_row gt_center">2.35 [1.70;3.53]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VIS2020</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">336</td>
<td headers="stat_2" class="gt_row gt_center">351</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">191</td>
<td headers="stat_5" class="gt_row gt_center">216</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">28</td>
<td headers="stat_2" class="gt_row gt_center">9</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">64</td>
<td headers="stat_5" class="gt_row gt_center">74</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">55 ± 54</td>
<td headers="stat_2" class="gt_row gt_center">15 ± 39</td>
<td headers="stat_3" class="gt_row gt_center">20 ± 23</td>
<td headers="stat_4" class="gt_row gt_center">13 ± 27</td>
<td headers="stat_5" class="gt_row gt_center">1 ± 0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">40 [20;73]</td>
<td headers="stat_2" class="gt_row gt_center">0 [0;10]</td>
<td headers="stat_3" class="gt_row gt_center">10 [1;38]</td>
<td headers="stat_4" class="gt_row gt_center">7 [0;16]</td>
<td headers="stat_5" class="gt_row gt_center">0 [0;1]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">WBC, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">348</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">196</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">59</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">18 ± 11</td>
<td headers="stat_2" class="gt_row gt_center">16 ± 11</td>
<td headers="stat_3" class="gt_row gt_center">16 ± 8</td>
<td headers="stat_4" class="gt_row gt_center">16 ± 9</td>
<td headers="stat_5" class="gt_row gt_center">15 ± 7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">15 [10;23]</td>
<td headers="stat_2" class="gt_row gt_center">13 [8;20]</td>
<td headers="stat_3" class="gt_row gt_center">14 [10;21]</td>
<td headers="stat_4" class="gt_row gt_center">14 [9;20]</td>
<td headers="stat_5" class="gt_row gt_center">14 [10;20]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEU, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">329</td>
<td headers="stat_2" class="gt_row gt_center">348</td>
<td headers="stat_3" class="gt_row gt_center">28</td>
<td headers="stat_4" class="gt_row gt_center">193</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">17</td>
<td headers="stat_4" class="gt_row gt_center">62</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">16 ± 11</td>
<td headers="stat_2" class="gt_row gt_center">13 ± 11</td>
<td headers="stat_3" class="gt_row gt_center">16 ± 7</td>
<td headers="stat_4" class="gt_row gt_center">13 ± 9</td>
<td headers="stat_5" class="gt_row gt_center">13 ± 7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">13 [8;20]</td>
<td headers="stat_2" class="gt_row gt_center">11 [6;16]</td>
<td headers="stat_3" class="gt_row gt_center">15 [11;20]</td>
<td headers="stat_4" class="gt_row gt_center">12 [7;18]</td>
<td headers="stat_5" class="gt_row gt_center">12 [8;17]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">NEUr, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">186</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">173</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">174</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">82</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">78 ± 13</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">83 ± 15</td>
<td headers="stat_5" class="gt_row gt_center">85 ± 8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">81 [70;88]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">88 [79;92]</td>
<td headers="stat_5" class="gt_row gt_center">86 [81;90]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYM, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">342</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">193</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">18</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">62</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">1.51 ± 0.96</td>
<td headers="stat_2" class="gt_row gt_center">1.74 ± 1.54</td>
<td headers="stat_3" class="gt_row gt_center">11.31 ± 6.43</td>
<td headers="stat_4" class="gt_row gt_center">1.64 ± 1.93</td>
<td headers="stat_5" class="gt_row gt_center">1.14 ± 0.74</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">1.29 [0.87;1.86]</td>
<td headers="stat_2" class="gt_row gt_center">1.42 [1.00;2.03]</td>
<td headers="stat_3" class="gt_row gt_center">10.00 [6.00;17.00]</td>
<td headers="stat_4" class="gt_row gt_center">1.03 [0.63;1.76]</td>
<td headers="stat_5" class="gt_row gt_center">1.02 [0.60;1.46]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LYMr, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">186</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">168</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">174</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">87</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">14 ± 10</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">9 ± 7</td>
<td headers="stat_5" class="gt_row gt_center">9 ± 6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">11 [6;22]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">7 [5;12]</td>
<td headers="stat_5" class="gt_row gt_center">7 [5;11]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">PLT, *10^9/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">348</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">196</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">59</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">155 ± 107</td>
<td headers="stat_2" class="gt_row gt_center">200 ± 161</td>
<td headers="stat_3" class="gt_row gt_center">219 ± 91</td>
<td headers="stat_4" class="gt_row gt_center">182 ± 122</td>
<td headers="stat_5" class="gt_row gt_center">214 ± 121</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">136 [86;190]</td>
<td headers="stat_2" class="gt_row gt_center">154 [83;272]</td>
<td headers="stat_3" class="gt_row gt_center">205 [157;254]</td>
<td headers="stat_4" class="gt_row gt_center">160 [78;244]</td>
<td headers="stat_5" class="gt_row gt_center">197 [122;292]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">aPTT, s</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">348</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">195</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">60</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">35 ± 15</td>
<td headers="stat_3" class="gt_row gt_center">34 ± 10</td>
<td headers="stat_4" class="gt_row gt_center">40 ± 26</td>
<td headers="stat_5" class="gt_row gt_center">39 ± 28</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">30 [27;38]</td>
<td headers="stat_3" class="gt_row gt_center">31 [28;35]</td>
<td headers="stat_4" class="gt_row gt_center">34 [29;42]</td>
<td headers="stat_5" class="gt_row gt_center">32 [28;41]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INR</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">263</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">195</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">97</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">60</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">1.19 ± 0.25</td>
<td headers="stat_3" class="gt_row gt_center">1.23 ± 0.17</td>
<td headers="stat_4" class="gt_row gt_center">1.50 ± 0.48</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">1.13 [1.03;1.28]</td>
<td headers="stat_3" class="gt_row gt_center">1.20 [1.14;1.28]</td>
<td headers="stat_4" class="gt_row gt_center">1.34 [1.20;1.70]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ATIII, %</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">132</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">6</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">228</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">249</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">53 ± 19</td>
<td headers="stat_2" class="gt_row gt_center">73 ± 24</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">23 ± 19</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">55 [39;66]</td>
<td headers="stat_2" class="gt_row gt_center">72 [55;86]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">16 [10;28]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Fibrinogen, g/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">342</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">188</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">18</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">67</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">4.68 ± 1.95</td>
<td headers="stat_2" class="gt_row gt_center">3.78 ± 2.03</td>
<td headers="stat_3" class="gt_row gt_center">4.62 ± 1.36</td>
<td headers="stat_4" class="gt_row gt_center">4.98 ± 2.28</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">4.38 [3.24;5.87]</td>
<td headers="stat_2" class="gt_row gt_center">3.50 [2.49;4.69]</td>
<td headers="stat_3" class="gt_row gt_center">4.70 [3.70;5.60]</td>
<td headers="stat_4" class="gt_row gt_center">4.50 [3.41;6.55]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">DDimer</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">333</td>
<td headers="stat_2" class="gt_row gt_center">191</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">111</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">31</td>
<td headers="stat_2" class="gt_row gt_center">169</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">144</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">7,330 ± 7,110</td>
<td headers="stat_2" class="gt_row gt_center">7,850 ± 17,964</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">628 ± 1,712</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">4,844 [2,184;10,644]</td>
<td headers="stat_2" class="gt_row gt_center">43 [6;7,430]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">10 [3;33]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Diuresis, ml</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">336</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">186</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">24</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">69</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">2,420 ± 1,590</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">1,323 ± 1,199</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">2,150 [1,300;3,375]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">1,000 [300;2,200]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Creatinine, mcmol/L</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">348</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">196</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">59</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">225 ± 136</td>
<td headers="stat_2" class="gt_row gt_center">129 ± 111</td>
<td headers="stat_3" class="gt_row gt_center">267 ± 265</td>
<td headers="stat_4" class="gt_row gt_center">219 ± 155</td>
<td headers="stat_5" class="gt_row gt_center">182 ± 143</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">205 [110;308]</td>
<td headers="stat_2" class="gt_row gt_center">83 [63;156]</td>
<td headers="stat_3" class="gt_row gt_center">167 [119;356]</td>
<td headers="stat_4" class="gt_row gt_center">166 [99;338]</td>
<td headers="stat_5" class="gt_row gt_center">131 [89;241]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">urea</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">342</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">196</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">18</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">59</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">8 ± 6</td>
<td headers="stat_3" class="gt_row gt_center">16 ± 12</td>
<td headers="stat_4" class="gt_row gt_center">15 ± 11</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">6 [3;10]</td>
<td headers="stat_3" class="gt_row gt_center">12 [8;23]</td>
<td headers="stat_4" class="gt_row gt_center">14 [7;21]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">protein_total</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">165</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">199</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">52 ± 9</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">55 ± 10</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">52 [47;56]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">53 [49;61]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">albumin</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">25.7 ± 5.3</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">25.8 [22.2;29.5]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">bilirubin_total</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">342</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">192</td>
<td headers="stat_5" class="gt_row gt_center">202</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">18</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">63</td>
<td headers="stat_5" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">35 ± 36</td>
<td headers="stat_2" class="gt_row gt_center">24 ± 34</td>
<td headers="stat_3" class="gt_row gt_center">17 ± 20</td>
<td headers="stat_4" class="gt_row gt_center">53 ± 82</td>
<td headers="stat_5" class="gt_row gt_center">31 ± 57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">22 [12;44]</td>
<td headers="stat_2" class="gt_row gt_center">12 [8;23]</td>
<td headers="stat_3" class="gt_row gt_center">8 [7;13]</td>
<td headers="stat_4" class="gt_row gt_center">20 [11;49]</td>
<td headers="stat_5" class="gt_row gt_center">18 [10;28]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">bilirubin_direct</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">288</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">150</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">72</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">105</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">14 ± 26</td>
<td headers="stat_3" class="gt_row gt_center">8 ± 13</td>
<td headers="stat_4" class="gt_row gt_center">36 ± 60</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">5 [2;12]</td>
<td headers="stat_3" class="gt_row gt_center">3 [2;5]</td>
<td headers="stat_4" class="gt_row gt_center">9 [3;24]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AST</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">330</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">187</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">30</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">68</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">91 ± 378</td>
<td headers="stat_3" class="gt_row gt_center">46 ± 41</td>
<td headers="stat_4" class="gt_row gt_center">93 ± 122</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">33 [20;60]</td>
<td headers="stat_3" class="gt_row gt_center">33 [25;51]</td>
<td headers="stat_4" class="gt_row gt_center">44 [20;104]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ALT</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">330</td>
<td headers="stat_3" class="gt_row gt_center">35</td>
<td headers="stat_4" class="gt_row gt_center">193</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">30</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="stat_4" class="gt_row gt_center">62</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">64 ± 234</td>
<td headers="stat_3" class="gt_row gt_center">37 ± 22</td>
<td headers="stat_4" class="gt_row gt_center">68 ± 98</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">19 [12;40]</td>
<td headers="stat_3" class="gt_row gt_center">29 [17;53]</td>
<td headers="stat_4" class="gt_row gt_center">29 [15;78]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">LDH</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">259</td>
<td headers="stat_3" class="gt_row gt_center">30</td>
<td headers="stat_4" class="gt_row gt_center">32</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">101</td>
<td headers="stat_3" class="gt_row gt_center">15</td>
<td headers="stat_4" class="gt_row gt_center">223</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">648 ± 686</td>
<td headers="stat_3" class="gt_row gt_center">310 ± 306</td>
<td headers="stat_4" class="gt_row gt_center">254 ± 327</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">403 [302;657]</td>
<td headers="stat_3" class="gt_row gt_center">238 [72;371]</td>
<td headers="stat_4" class="gt_row gt_center">59 [0;487]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ferritin</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">326</td>
<td headers="stat_2" class="gt_row gt_center">120</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">40</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">240</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">215</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">1,031 ± 1,553</td>
<td headers="stat_2" class="gt_row gt_center">418 ± 435</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">476 ± 424</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">665 [370;1,145]</td>
<td headers="stat_2" class="gt_row gt_center">255 [137;560]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">289 [80;953]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">presepsin</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">62</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">1</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">298</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">254</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">1,997 ± 2,884</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">0 ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">700 [213;2,836]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">0 [0;0]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_1b</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">96</td>
<td headers="stat_3" class="gt_row gt_center">4</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">264</td>
<td headers="stat_3" class="gt_row gt_center">41</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">3.4 ± 2.9</td>
<td headers="stat_3" class="gt_row gt_center">534.5 ± 35.2</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">2.7 [1.5;4.8]</td>
<td headers="stat_3" class="gt_row gt_center">532.2 [507.4;561.7]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_6</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">330</td>
<td headers="stat_2" class="gt_row gt_center">93</td>
<td headers="stat_3" class="gt_row gt_center">2</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">187</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">267</td>
<td headers="stat_3" class="gt_row gt_center">43</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">103</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">2,811 ± 9,909</td>
<td headers="stat_2" class="gt_row gt_center">661 ± 2,422</td>
<td headers="stat_3" class="gt_row gt_center">4 ± 3</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">1,032 ± 1,554</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">308 [119;1,750]</td>
<td headers="stat_2" class="gt_row gt_center">90 [32;171]</td>
<td headers="stat_3" class="gt_row gt_center">4 [2;6]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">411 [137;1,181]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_8</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">93</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">267</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">76 ± 110</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">38 [21;99]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">IL_10</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">93</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">267</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">63 ± 153</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">19 [6;40]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">TNFa</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">262</td>
<td headers="stat_2" class="gt_row gt_center">96</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">102</td>
<td headers="stat_2" class="gt_row gt_center">264</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">7 ± 28</td>
<td headers="stat_2" class="gt_row gt_center">15 ± 19</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">2 [0;4]</td>
<td headers="stat_2" class="gt_row gt_center">9 [2;21]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">EAA</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">165</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">21</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">199</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">24</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">0.53 ± 0.17</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">0.29 ± 0.11</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">0.52 [0.47;0.64]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">0.26 [0.20;0.38]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ig_A</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">296</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">68</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">227 ± 125</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">219 [132;285]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ig_G</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">321</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">43</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">654 ± 200</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">646 [548;749]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ig_M</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">329</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">97 ± 92</td>
<td headers="stat_2" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_3" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_4" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_5" class="gt_row gt_center">NA ± NA</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">77 [55;99]</td>
<td headers="stat_2" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_3" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_4" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_5" class="gt_row gt_center">NA [NA;NA]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">348</td>
<td headers="stat_2" class="gt_row gt_center">198</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">210</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">162</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">45</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">8.9 ± 10.4</td>
<td headers="stat_2" class="gt_row gt_center">3.6 ± 5.6</td>
<td headers="stat_3" class="gt_row gt_center">2.5 ± 1.3</td>
<td headers="stat_4" class="gt_row gt_center">4.4 ± 5.1</td>
<td headers="stat_5" class="gt_row gt_center">4.3 ± 5.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">6.0 [4.0;10.0]</td>
<td headers="stat_2" class="gt_row gt_center">2.2 [1.0;3.1]</td>
<td headers="stat_3" class="gt_row gt_center">2.2 [1.2;3.6]</td>
<td headers="stat_4" class="gt_row gt_center">3.0 [2.0;5.0]</td>
<td headers="stat_5" class="gt_row gt_center">2.5 [1.5;4.9]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">168</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">190</td>
<td headers="stat_5" class="gt_row gt_center">200</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">192</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">65</td>
<td headers="stat_5" class="gt_row gt_center">90</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">NA ± NA</td>
<td headers="stat_2" class="gt_row gt_center">3.3 ± 3.8</td>
<td headers="stat_3" class="gt_row gt_center">10.4 ± 3.8</td>
<td headers="stat_4" class="gt_row gt_center">6.2 ± 7.0</td>
<td headers="stat_5" class="gt_row gt_center">2.9 ± 2.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">NA [NA;NA]</td>
<td headers="stat_2" class="gt_row gt_center">2.0 [0.5;4.6]</td>
<td headers="stat_3" class="gt_row gt_center">11.0 [8.0;14.0]</td>
<td headers="stat_4" class="gt_row gt_center">3.8 [2.5;7.3]</td>
<td headers="stat_5" class="gt_row gt_center">2.3 [0.7;4.6]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ICU_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">348</td>
<td headers="stat_2" class="gt_row gt_center">360</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">255</td>
<td headers="stat_5" class="gt_row gt_center">290</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">16 ± 15</td>
<td headers="stat_2" class="gt_row gt_center">13 ± 12</td>
<td headers="stat_3" class="gt_row gt_center">8 ± 4</td>
<td headers="stat_4" class="gt_row gt_center">6 ± 6</td>
<td headers="stat_5" class="gt_row gt_center">10 ± 10</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">11 [6;21]</td>
<td headers="stat_2" class="gt_row gt_center">8 [5;17]</td>
<td headers="stat_3" class="gt_row gt_center">8 [5;11]</td>
<td headers="stat_4" class="gt_row gt_center">4 [2;8]</td>
<td headers="stat_5" class="gt_row gt_center">6 [3;14]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_dur_T0_first_end</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-missed</td>
<td headers="stat_1" class="gt_row gt_center">324</td>
<td headers="stat_2" class="gt_row gt_center">252</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">90</td>
<td headers="stat_5" class="gt_row gt_center">240</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missed</td>
<td headers="stat_1" class="gt_row gt_center">40</td>
<td headers="stat_2" class="gt_row gt_center">108</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">165</td>
<td headers="stat_5" class="gt_row gt_center">50</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean ± SD</td>
<td headers="stat_1" class="gt_row gt_center">13 ± 15</td>
<td headers="stat_2" class="gt_row gt_center">9 ± 14</td>
<td headers="stat_3" class="gt_row gt_center">5 ± 3</td>
<td headers="stat_4" class="gt_row gt_center">4 ± 4</td>
<td headers="stat_5" class="gt_row gt_center">7 ± 10</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median [Q1:Q3]</td>
<td headers="stat_1" class="gt_row gt_center">8 [4;17]</td>
<td headers="stat_2" class="gt_row gt_center">3 [1;8]</td>
<td headers="stat_3" class="gt_row gt_center">4 [3;8]</td>
<td headers="stat_4" class="gt_row gt_center">3 [2;5]</td>
<td headers="stat_5" class="gt_row gt_center">3 [1;8]</td></tr>
  </tbody>
  
  
</table>
</div>
```

# Descriptive stats for categorical variables


``` r
sepsis %>%
  select(where(is.factor), -c(study_patient_id)) %>% 
  tbl_summary(by=study_id)
```

```{=html}
<div id="khqmkryjhz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#khqmkryjhz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#khqmkryjhz thead, #khqmkryjhz tbody, #khqmkryjhz tfoot, #khqmkryjhz tr, #khqmkryjhz td, #khqmkryjhz th {
  border-style: none;
}

#khqmkryjhz p {
  margin: 0;
  padding: 0;
}

#khqmkryjhz .gt_table {
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

#khqmkryjhz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#khqmkryjhz .gt_title {
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

#khqmkryjhz .gt_subtitle {
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

#khqmkryjhz .gt_heading {
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

#khqmkryjhz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#khqmkryjhz .gt_col_headings {
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

#khqmkryjhz .gt_col_heading {
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

#khqmkryjhz .gt_column_spanner_outer {
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

#khqmkryjhz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#khqmkryjhz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#khqmkryjhz .gt_column_spanner {
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

#khqmkryjhz .gt_spanner_row {
  border-bottom-style: hidden;
}

#khqmkryjhz .gt_group_heading {
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

#khqmkryjhz .gt_empty_group_heading {
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

#khqmkryjhz .gt_from_md > :first-child {
  margin-top: 0;
}

#khqmkryjhz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#khqmkryjhz .gt_row {
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

#khqmkryjhz .gt_stub {
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

#khqmkryjhz .gt_stub_row_group {
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

#khqmkryjhz .gt_row_group_first td {
  border-top-width: 2px;
}

#khqmkryjhz .gt_row_group_first th {
  border-top-width: 2px;
}

#khqmkryjhz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#khqmkryjhz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#khqmkryjhz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#khqmkryjhz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#khqmkryjhz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#khqmkryjhz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#khqmkryjhz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#khqmkryjhz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#khqmkryjhz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#khqmkryjhz .gt_footnotes {
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

#khqmkryjhz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#khqmkryjhz .gt_sourcenotes {
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

#khqmkryjhz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#khqmkryjhz .gt_left {
  text-align: left;
}

#khqmkryjhz .gt_center {
  text-align: center;
}

#khqmkryjhz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#khqmkryjhz .gt_font_normal {
  font-weight: normal;
}

#khqmkryjhz .gt_font_bold {
  font-weight: bold;
}

#khqmkryjhz .gt_font_italic {
  font-style: italic;
}

#khqmkryjhz .gt_super {
  font-size: 65%;
}

#khqmkryjhz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#khqmkryjhz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#khqmkryjhz .gt_indent_1 {
  text-indent: 5px;
}

#khqmkryjhz .gt_indent_2 {
  text-indent: 10px;
}

#khqmkryjhz .gt_indent_3 {
  text-indent: 15px;
}

#khqmkryjhz .gt_indent_4 {
  text-indent: 20px;
}

#khqmkryjhz .gt_indent_5 {
  text-indent: 25px;
}

#khqmkryjhz .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#khqmkryjhz div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>CL_Sep_3C_6</strong><br />
N = 364</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>L_mSep_2dC_11</strong><br />
N = 360</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_3"><span class='gt_from_md'><strong>L_Sep_1C_1</strong><br />
N = 45</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_4"><span class='gt_from_md'><strong>L_Sep_2dC_7</strong><br />
N = 255</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_5"><span class='gt_from_md'><strong>L_Sep_2R_4</strong><br />
N = 290</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    base</td>
<td headers="stat_1" class="gt_row gt_center">192 (53%)</td>
<td headers="stat_2" class="gt_row gt_center">168 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">205 (80%)</td>
<td headers="stat_5" class="gt_row gt_center">100 (34%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    efferon LPS</td>
<td headers="stat_1" class="gt_row gt_center">84 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">192 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">45 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">50 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">190 (66%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    efferon CT</td>
<td headers="stat_1" class="gt_row gt_center">84 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_5" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">4</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Time point, days</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="stat_1" class="gt_row gt_center">91 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">60 (17%)</td>
<td headers="stat_3" class="gt_row gt_center">9 (20%)</td>
<td headers="stat_4" class="gt_row gt_center">51 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">58 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_1" class="gt_row gt_center">91 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">60 (17%)</td>
<td headers="stat_3" class="gt_row gt_center">9 (20%)</td>
<td headers="stat_4" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_5" class="gt_row gt_center">58 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_1" class="gt_row gt_center">91 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">60 (17%)</td>
<td headers="stat_3" class="gt_row gt_center">9 (20%)</td>
<td headers="stat_4" class="gt_row gt_center">51 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">58 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_1" class="gt_row gt_center">91 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">60 (17%)</td>
<td headers="stat_3" class="gt_row gt_center">9 (20%)</td>
<td headers="stat_4" class="gt_row gt_center">51 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">58 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7</td>
<td headers="stat_1" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">60 (17%)</td>
<td headers="stat_3" class="gt_row gt_center">9 (20%)</td>
<td headers="stat_4" class="gt_row gt_center">51 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">58 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    14</td>
<td headers="stat_1" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">60 (17%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">51 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Sex</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    female</td>
<td headers="stat_1" class="gt_row gt_center">92 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">360 (100%)</td>
<td headers="stat_3" class="gt_row gt_center">20 (44%)</td>
<td headers="stat_4" class="gt_row gt_center">135 (53%)</td>
<td headers="stat_5" class="gt_row gt_center">145 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    male</td>
<td headers="stat_1" class="gt_row gt_center">256 (74%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_3" class="gt_row gt_center">25 (56%)</td>
<td headers="stat_4" class="gt_row gt_center">120 (47%)</td>
<td headers="stat_5" class="gt_row gt_center">145 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="stat_1" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">162 (45%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">45 (18%)</td>
<td headers="stat_5" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">348 (100%)</td>
<td headers="stat_2" class="gt_row gt_center">198 (55%)</td>
<td headers="stat_3" class="gt_row gt_center">45 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">210 (82%)</td>
<td headers="stat_5" class="gt_row gt_center">290 (100%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="stat_1" class="gt_row gt_center">20 (5.8%)</td>
<td headers="stat_2" class="gt_row gt_center">108 (30%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">165 (65%)</td>
<td headers="stat_5" class="gt_row gt_center">50 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">324 (94%)</td>
<td headers="stat_2" class="gt_row gt_center">252 (70%)</td>
<td headers="stat_3" class="gt_row gt_center">45 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">90 (35%)</td>
<td headers="stat_5" class="gt_row gt_center">240 (83%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">20</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_T0</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    FALSE</td>
<td headers="stat_1" class="gt_row gt_center">68 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">144 (40%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">65 (25%)</td>
<td headers="stat_5" class="gt_row gt_center">90 (31%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRUE</td>
<td headers="stat_1" class="gt_row gt_center">280 (80%)</td>
<td headers="stat_2" class="gt_row gt_center">216 (60%)</td>
<td headers="stat_3" class="gt_row gt_center">45 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">190 (75%)</td>
<td headers="stat_5" class="gt_row gt_center">200 (69%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">shock_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">92 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">24 (12%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (NA%)</td>
<td headers="stat_4" class="gt_row gt_center">115 (55%)</td>
<td headers="stat_5" class="gt_row gt_center">115 (40%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">256 (74%)</td>
<td headers="stat_2" class="gt_row gt_center">174 (88%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (NA%)</td>
<td headers="stat_4" class="gt_row gt_center">95 (45%)</td>
<td headers="stat_5" class="gt_row gt_center">175 (60%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">162</td>
<td headers="stat_3" class="gt_row gt_center">45</td>
<td headers="stat_4" class="gt_row gt_center">45</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AKI_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">0 (NA%)</td>
<td headers="stat_2" class="gt_row gt_center">30 (14%)</td>
<td headers="stat_3" class="gt_row gt_center">10 (22%)</td>
<td headers="stat_4" class="gt_row gt_center">115 (61%)</td>
<td headers="stat_5" class="gt_row gt_center">80 (40%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">0 (NA%)</td>
<td headers="stat_2" class="gt_row gt_center">138 (64%)</td>
<td headers="stat_3" class="gt_row gt_center">35 (78%)</td>
<td headers="stat_4" class="gt_row gt_center">75 (39%)</td>
<td headers="stat_5" class="gt_row gt_center">120 (60%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    censored</td>
<td headers="stat_1" class="gt_row gt_center">0 (NA%)</td>
<td headers="stat_2" class="gt_row gt_center">48 (22%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_5" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">364</td>
<td headers="stat_2" class="gt_row gt_center">144</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">65</td>
<td headers="stat_5" class="gt_row gt_center">90</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ICU_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">200 (57%)</td>
<td headers="stat_2" class="gt_row gt_center">78 (22%)</td>
<td headers="stat_3" class="gt_row gt_center">10 (22%)</td>
<td headers="stat_4" class="gt_row gt_center">160 (63%)</td>
<td headers="stat_5" class="gt_row gt_center">155 (53%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">148 (43%)</td>
<td headers="stat_2" class="gt_row gt_center">282 (78%)</td>
<td headers="stat_3" class="gt_row gt_center">35 (78%)</td>
<td headers="stat_4" class="gt_row gt_center">95 (37%)</td>
<td headers="stat_5" class="gt_row gt_center">135 (47%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">0</td>
<td headers="stat_5" class="gt_row gt_center">0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">MV_first_end_status</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    death</td>
<td headers="stat_1" class="gt_row gt_center">96 (30%)</td>
<td headers="stat_2" class="gt_row gt_center">6 (2.4%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">70 (78%)</td>
<td headers="stat_5" class="gt_row gt_center">120 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    recovery</td>
<td headers="stat_1" class="gt_row gt_center">228 (70%)</td>
<td headers="stat_2" class="gt_row gt_center">246 (98%)</td>
<td headers="stat_3" class="gt_row gt_center">45 (100%)</td>
<td headers="stat_4" class="gt_row gt_center">20 (22%)</td>
<td headers="stat_5" class="gt_row gt_center">120 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">40</td>
<td headers="stat_2" class="gt_row gt_center">108</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="stat_4" class="gt_row gt_center">165</td>
<td headers="stat_5" class="gt_row gt_center">50</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


