install_and_load <- function(P) {
  Pi <- P[!(P %in% installed.packages()[,"Package"])]
  
  if (length(Pi)>0) install.packages(Pi)
  for(i in P) library(i,character.only = TRUE)
}

install_and_load(c("tidyverse", "forcats", "extrafont", "formattable", "xtable", "knitr", "ggrepel",
                   "writexl"))

# cur_path <- rstudioapi::getSourceEditorContext()$path
# foo <- str_split(cur_path, "/")
# cur_dir <- str_sub(cur_path, 1, str_locate(cur_path,last(foo[[1]]))-2)
# setwd(cur_dir)
# rm(foo)
# getwd()
load("Diaporama/data/zonages.Rda")
data <- read.csv2("Diaporama/data/valeurs_trimestrielles.csv", encoding = "UTF-8", stringsAsFactors = F)

emploi <- data %>% select(-Période, -idBank) %>% filter(Libellé != "Codes") %>% 
  separate(Libellé, c("Type", "Secteur", "Territoire"), sep = " - ") 

emploi %>% extract(Secteur, c("Activité", "Secteur"), "^(.*) \\((.*)\\)$")

emploidep <- emploi %>% filter(Territoire %in% ldep$ldep ) %>% 
  inner_join(ldep, by = c("Territoire" = "ldep")) %>% select(dep, Territoire, everything())
