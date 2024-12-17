This folder contains separate `*.R` scripts and functions for data processing and analysis in the current project. Please find short annotations for individual scripts below.  

-----

[sepsis_HA_data_filter.R](sepsis_HA_data_filter.R) -- select data for further analysis after primary processing:  
    - remove patients from the `L_Sep_1C_1` study  
    - remove variables with more-than-treshold number of `NA`s  

[sepsis_HA_data_import_cleaning.R](sepsis_HA_data_import_cleaning.R) -- data primary processing:  
    - manually correct erronic and inconsistent values  
    - remove variables with dubious data  
    - recode factor variables uniformely  
    - add some useful derivative IDs and variables  
