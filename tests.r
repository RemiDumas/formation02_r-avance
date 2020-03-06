setwd("C:/Users/eiq6xm/Documents/Formations/Donnees/R - 02 - Avancé/Kit Formation R avancé/Diaporama")

library(tidyverse)

gapmind_messy <- readRDS("data/gapmind_mess.Rds")
gapmind <- gapmind_messy %>% gather(key = variables, value = valeurs, -c(country, continent))


gapmind2 <- gapmind %>% separate(col = variables, into = c("variable", "annee"), sep = "_")

dim(gapmind_messy)
dim(gapmind)
dim(gapmind2)

str(gapmind_messy)
str(gapmind)

load("data/zonages.Rda")

listecom <- ldc %>% mutate(dep = str_sub(dc,1,2), codcom = str_sub(dc,3,5)) %>% select(dep,codcom,libcom=ldc)
saveRDS(listecom, "data/listecom.Rds")

df <- data.frame(
  Nom = c("Pierre", "Vanessa", "Louis", "Anaïs", "Pierre", "Louis", "Anaïs", "Pierre", "Vanessa", "Louis"),
  Ville = c(rep("Venise",4), rep("Rome", 3), rep("Naples",3) ),
  Pays = rep("Italie", 10),
  Note = c(4,3,1,5,4,1,2,3,2,5),
  stringsAsFactors = F
)
df %>% complete(Nom, Ville, fill = list(Pays = "Italie", Note = NA))

df %>% spread(key = Ville, value = Note, fill = "-")

tx_chom <- readxl::read_xls("data/tx_chom.xls")

tx_chom[1:4,1:10]
str(tx_chom)


departements <- c("16","17","19","23","24","33","40","47","64","79","86","87")



tx_chomNA <- tx_chom %>% 
  filter(Code %in% departements) %>% 
  gather(key = periode, value = taux, -c(Code,Libellé)) %>% 
  separate(periode, c("trimestre", "annee")) %>% 
  unite(Département, Code, Libellé, sep=" - ") 




trim <- tx_chomNA %>% 
  distinct(annee, trimestre) %>% 
  arrange(desc(annee), desc(trimestre)) %>% 
  head(1) %>% 
  pull(trimestre) %>% 
  str_sub(2,2) %>% 
  as.numeric

ann <- tx_chomNA %>% 
  distinct(annee, trimestre) %>% 
  arrange(desc(annee), desc(trimestre)) %>% 
  head(1) %>% 
  pull(annee) %>% 
  as.numeric

trimestres <- c(
  paste0("T", trim, "_", ann - 1),
  paste0("T", 1 + ( trim + 2) %% 4, "_", ann),
  paste0("T", trim, "_", ann)
)

tab_evol_chomNA <- tx_chomNA %>% 
  group_by(Département) %>% 
  mutate(
    evol_tri = round(taux - lag(taux, 1),1),
    evol_an = round(taux - lag(taux, 4),1)
  ) %>% 
  ungroup() %>% 
  unite(periode, trimestre ,annee, sep = "_") %>% 
  filter(periode == trimestres[3]) %>% 
  select(-periode, -taux)


tab_tx_chomNA <- tx_chomNA %>% 
  unite(periode, trimestre ,annee, sep = "_") %>% 
  filter(periode %in% trimestres) %>%
  spread(key = periode, value = taux) %>% 
  select(Département, trimestres) # Great !

left_join(tab_tx_chomNA, tab_evol_chomNA, by = c("Département"))
