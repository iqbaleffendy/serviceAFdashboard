library(tidyverse)
library(readxl)
library(writexl)

popdata <- read_excel("datapopulasi.xlsx")

popdata_filtered <- popdata %>% 
  mutate(`BRANCH ASS` = str_to_upper(`BRANCH ASS`)) %>% 
  group_by(`BRANCH ASS`, `SERIES`) %>% 
  count()

write_xlsx(popdata_filtered, "popdata.xlsx")