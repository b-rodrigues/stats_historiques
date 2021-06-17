library(dplyr)
library(tidyr)
library(pdftools)
library(stringr)
library(metaDigitise)
library(ggplot2)

# interactive process,
# so must be commented now
#pluie <- metaDigitise("~/Documents/hist_stats/", summary = FALSE)
#
#pluie_df <- pluie$quant_pluie.png %>%
#  as_tibble %>%
#  select(annee = x,
#         pluie = y) %>%
#  mutate(annee = round(annee))
#
#ggplot(pluie_df) +
#  geom_line(aes(y = pluie, x = annee))
#
#readr::write_csv(pluie_df,
#          "hist_stats/datasets/a_203_quantite_de_pluie.csv")

hist_stats <- pdf_text("hist_stats/stats_historique_statec.pdf")

a201 <- hist_stats[26] %>%
  str_split("\n") %>%
  .[[1]]

a201_1 <- a201[12:17] %>%
  str_remove("\\.\\.\\.\\.\\.\\.\\.") %>%  
  str_replace_all("\\s+", "|") %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                               "Janvier",
                                               "Février",
                                               "Mars",
                                               "Avril",
                                               "Mai",
                                               "Juin",
                                               "Juillet",
                                               "Août",
                                               "Septembre",
                                               "Octobre",
                                               "Novembre",
                                               "Décembre",
                                               "Année"))

readr::write_csv(a201_1,
                 "hist_stats/datasets/a_201_temp_moyenne_lux_ville-moyennes_tricennales.csv")

a201_2 <- a201[21:51] %>%
  str_remove_all('\\"') %>%  
  str_remove_all("\\.\\.+") %>%  
  .[-4] %>%  
  .[-27] %>%  
  str_remove_all("I") %>%  
  str_replace_all("\\s+", "|") %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                              "Janvier",
                                              "Février",
                                              "Mars",
                                              "Avril",
                                              "Mai",
                                              "Juin",
                                              "Juillet",
                                              "Août",
                                              "Septembre",
                                              "Octobre",
                                              "Novembre",
                                              "Décembre",
                                              "Année"))


readr::write_csv(a201_2,
                 "hist_stats/datasets/a_201_temp_moyenne_lux_ville-resultats_par_annee.csv")



a201_3 <- a201[54:57] %>%
  str_remove_all('\\"') %>%  
  str_remove_all("\\.\\.+") %>%  
  str_replace_all("\\s+", "|") %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                              "Janvier",
                                              "Février",
                                              "Mars",
                                              "Avril",
                                              "Mai",
                                              "Juin",
                                              "Juillet",
                                              "Août",
                                              "Septembre",
                                              "Octobre",
                                              "Novembre",
                                              "Décembre",
                                              "Année"))

# two years have the same maximum...

a201_3$`Année`[4] <- "1921/1959"

readr::write_csv(a201_3,
                 "hist_stats/datasets/a_201_temp_moyenne_lux_ville-min_et_max_mensuels.csv")

a202 <- hist_stats[27] %>%
  str_split("\n") %>%
  .[[1]]
