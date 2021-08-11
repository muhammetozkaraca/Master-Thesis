library(kableExtra)
library(readxl)
library(tidyverse)

rt_events_gtd2_summarised <- read_excel("terrorgroupsnames.xlsx")

kbl(rt_events_gtd2_summarised,
    caption = 'Religious Terror Organizations coded in GTD and EDTG',
    booktabs = T,
    valign = 't') %>% 
  kable_styling(latex_options = c("hold_position"), full_width = F) 