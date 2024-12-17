here::i_am("render_report.R")

sepsisHA_render <- function(x) { rmarkdown::render(x = here::here("code/0_sepsis_HA_full_report.Rmd"), 
                                                   output_dir = here::here("reports"), 
                                                   intermediates_dir = here::here("reports/temp"), 
                                                   envir = new.env()) 
                              }

sepsisHA_render()