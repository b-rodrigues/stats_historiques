library(dplyr)
library(tidyr)
library(pdftools)
library(stringr)
library(metaDigitise)
library(ggplot2)

# interactive process,
# so must be commented now
#pluie <- metaDigitise("~/Documents/", summary = FALSE)
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
#          "datasets/a_203_quantite_de_pluie.csv")

hist_stats <- pdf_text("stats_historique_statec.pdf")

a201 <- hist_stats[26] %>%
  str_split("\n") %>%
  .[[1]]

a201_1 <- a201[12:17] %>%
  str_remove("\\.\\.\\.\\.\\.\\.\\.") %>%  
  str_replace_all("\\s+", "|") %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                               "01",
                                               "02",
                                               "03",
                                               "04",
                                               "05",
                                               "06",
                                               "07",
                                               "08",
                                               "09",
                                               "10",
                                               "11",
                                               "12",
                                               "Année"))

#readr::write_csv(a201_1,
#                 "datasets/a_201_temp_moyenne_lux_ville-moyennes_tricennales.csv")

a201_2 <- a201[21:51] %>%
  str_remove_all('\\"') %>%  
  str_remove_all("\\.\\.+") %>%  
  .[-4] %>%  
  .[-27] %>%  
  str_remove_all("I") %>%  
  str_replace_all("\\s+", "|") %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                              "01",
                                              "02",
                                              "03",
                                              "04",
                                              "05",
                                              "06",
                                              "07",
                                              "08",
                                              "09",
                                              "10",
                                              "11",
                                              "12",
                                              "Année"))


#readr::write_csv(a201_2,
#                 "datasets/a_201_temp_moyenne_lux_ville-resultats_par_annee.csv")



a201_3 <- a201[54:57] %>%
  str_remove_all('\\"') %>%  
  str_remove_all("\\.\\.+") %>%  
  str_replace_all("\\s+", "|") %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                              "01",
                                              "02",
                                              "03",
                                              "04",
                                              "05",
                                              "06",
                                              "07",
                                              "08",
                                              "09",
                                              "10",
                                              "11",
                                              "12",
                                              "Année"))

# two years have the same maximum...

a201_3$`Année`[4] <- "1921/1959"

#readr::write_csv(a201_3,
#                 "datasets/a_201_temp_moyenne_lux_ville-min_et_max_mensuels.csv")

# Table a202

a202 <- hist_stats[27] %>%
  str_split("\n") %>%
  .[[1]]


a202_1_1 <- a202[seq(from = 14, to = 33, by = 2)] %>% #last lines will be added manually
  str_remove_all('\\"') %>%
  str_remove_all("\\.\\.+") %>%
  str_replace_all("1(?!\\.|\\d{1,}|/)", " ") %>%
  str_remove_all("(\\s|^)\\.(\\s|$)") %>%
  str_replace_all("\\s+", "\\|") %>%
  str_remove_all("I") %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                              "01",
                                              "02",
                                              "03",
                                              "04",
                                              "05",
                                              "06",
                                              "07",
                                              "08",
                                              "09",
                                              "10",
                                              "11",
                                              "12"))

a202_1_1 <- a202_1_1 %>%
  select(-`Périodes`) %>%
  pivot_longer(cols = everything(), names_to = "Mois", values_to = "degres") %>%
  mutate(row = rep(1:10, each = 12),
         col = rep(seq(1, 12), 10)) %>% as.data.frame 

a202_1_2 <- a202[c(seq(from = 15, to = 33, by = 2), 34, 35, 36)] %>% #last lines will be added manually
  str_remove_all('\\"') %>%
  #str_remove_all("\\.\\.+") %>%
  #str_replace_all("1(?!\\.|\\d{1,}|/)", " ") %>%
  str_replace_all(", ", ",") %>%
  str_remove_all("I") %>%
  str_remove_all("(\\s|^)\\.(\\s|$)") %>%
  str_replace_all("\\s+", "\\|")


put_in_string <- function(string, position, character){

  string <- str_split(string, "", simplify = TRUE)
  string[1, position] <- character
  paste0(string, collapse = "")

}

a202_1_2_2 <- paste0("||", a202_1_2[11:13])

a202_1_2_2[1] <- put_in_string(a202_1_2_2[1], 11, "||")
a202_1_2_2[2] <- put_in_string(a202_1_2_2[2], 11, "||")

a202_1_2[11:13] <- a202_1_2_2

a202_1_2 <- a202_1_2 %>%
  as_tibble %>%
  separate(col = value, sep = "\\|", into = c("Périodes",
                                              "01",
                                              "02",
                                              "03",
                                              "04",
                                              "05",
                                              "06",
                                              "07",
                                              "08",
                                              "09",
                                              "10",
                                              "11",
                                              "12")) 

a202_1_2[is.na(a202_1_2)] <- ""

a202_1_2 <- a202_1_2 %>%
  select(-`Périodes`) %>%
  pivot_longer(cols = everything(), names_to = "Mois", values_to = "jour_annee") %>%
  separate(jour_annee, "/", into = c("jour", "annee")) %>%
  mutate(jour = ifelse(nchar(jour) == 1, paste0("0", jour), jour)) %>% 
  mutate(row = c(rep(1:10, each = 12), rep(10, 36)),
         col = rep(seq(1, 12), 13)) %>%
  separate_rows(jour, convert = TRUE)

a202_1 <- full_join(a202_1_1, a202_1_2, by = c("row", "col")) %>%
  select(jour, mois = Mois.x, annee, degres) %>%
  mutate(jour = ifelse(nchar(jour) == 1, paste0("0", jour), jour)) %>%
  mutate(annee = ifelse(annee == 19762, 1976, annee)) %>%
  filter(!is.na(annee)) %>%  
  mutate(date = lubridate::ymd(paste0(annee, "-", mois, "-", jour))) %>%
  select(date, degres)

#readr::write_csv(a202_1,
#                 "datasets/a_202_10_temp_maximum_lux_ville.csv")
