library("haven")
library(tidyverse)
library(lubridate)
library(tictoc)
library(magrittr)
library(fixest)
library(lfe)
library("readxl")
library("writexl")

#répertoires
mortalite_path <- "~/ENSAE/3A/Projet_DSSS/Données/Mortalité"
APL_path <- "~/ENSAE/3A/Projet_DSSS/Données/APL_densité_médicale/APL"
densite_path <- "~/ENSAE/3A/Projet_DSSS/Données/APL_densité_médicale/Densité médicale"
covariable_path <- "~/ENSAE/3A/Projet_DSSS/Données/Covariables"

#décès

files_list <- list.files(sprintf('%s', mortalite_path), pattern = 'deces-[0-9]+.txt', full.names = T)
files_list <- files_list[12:16] #on ne prend que 2015-2019

positions <- c(
  nom = 80,
  sexe = 1,
  naissance_date = 8,
  naissance_code_lieu = 5,
  naissance_commune = 30,
  naissance_pays = 30,
  deces_date = 8,
  deces_code_lieu = 5,
  deces_numero_acte = 9)

deces <- lapply(files_list, read_fwf,
                        col_positions = fwf_widths(positions, col_names = names(positions)),
                        col_types = cols(
                          .default = col_character())
)

# Attention pour les dates : certaines sont approximatives. Lorsque c'est le cas
# la partie incertaine (mois ou jour) est 00 -> remplacer les 00 par 01.
# Pour les années inconnues -> ne rien mettre ?

#transforme dates en numériques
nettoyer_partie_date <- function(
  x,
  debut,
  fin
) {
  rez <- x %>%
    substr(debut, fin) %>% 
    as.integer()
  
  rez[rez==0] <- NA
  rez
}

#impute moyenne
complete_manquant <- function(x) {
  x[is.na(x)] <- as.integer(mean(x, na.rm = TRUE))
  x
}

#applique ces 2 fonctions aux colonnes dates
clean_date <- function(df){
  clean_df <- df %>%
    mutate(
      naissance_annee = nettoyer_partie_date(naissance_date, 1, 4),
      naissance_annee_complete = complete_manquant(naissance_annee), 
      naissance_mois = nettoyer_partie_date(naissance_date, 5, 6),
      naissance_mois_complete = complete_manquant(naissance_mois), 
      naissance_jour = nettoyer_partie_date(naissance_date, 7, 8),
      naissance_jour_complete = complete_manquant(naissance_jour), 
      naissance_date_brute = naissance_date,
      naissance_date = as.Date(naissance_date, '%Y%m%d'),
      naissance_date_complete = as.Date(paste0(naissance_annee_complete, '-', naissance_mois_complete, '-', naissance_jour_complete)),
      deces_annee = nettoyer_partie_date(deces_date, 1, 4),
      deces_annee_complete = complete_manquant(deces_annee), 
      deces_mois = nettoyer_partie_date(deces_date, 5, 6),
      deces_mois_complete = complete_manquant(deces_mois), 
      deces_jour = nettoyer_partie_date(deces_date, 7, 8),
      deces_jour_complete = complete_manquant(deces_jour), 
      deces_date = as.Date(deces_date, '%Y%m%d'),
      deces_date_complete = as.Date(paste0(deces_annee_complete, '-', deces_mois_complete, '-', deces_jour_complete))
    ) %>%
    select(nom, sexe, naissance_date_brute,naissance_date_complete, naissance_code_lieu,
           naissance_commune, naissance_pays, deces_date, deces_date_complete, deces_code_lieu,
           deces_numero_acte)
  clean_df
}



deces <- lapply(deces, clean_date)

deces_15_19 <- bind_rows(deces) %>% distinct(.keep_all = T)

#A <- deces_15_19 %>% group_by(deces_code_lieu) %>% summarise(n = n())

any(is.na(deces_15_19$naissance_date_complete))
any(is.na(deces_15_19$deces_date_complete))


#âge moyen décès par département

deces_15_19 %<>% mutate(age = round(as.numeric((deces_date_complete - naissance_date_complete)/365),1))

# ggplot(deces_15_19, aes(x=age)) + 
#   geom_density()
# 
# quantile(deces_15_19$age, probs = c(0,0.01,0.25,0.5,0.75,0.9,0.99,0.999))
# 
# 
# nrow(deces_15_19 %>% filter(age == 0))
# deces_15_19 %>% group_by(sexe) %>% summarise(age_moyen = mean(age))

table_commune <- read_csv("~/ENSAE/3A/Projet_DSSS/Données/commune2021.csv") %>%
  distinct(COM, .keep_all = T) #37122 communes/arrondissements municipaux

deces_15_19 %<>% distinct(nom, sexe, naissance_date_complete, naissance_code_lieu, deces_date_complete, .keep_all = T) %>%
  left_join(table_commune, by = c('deces_code_lieu' = 'COM')) %>%
  mutate(annee_deces = year(deces_date_complete))

communes_inconnues <- deces_15_19 %>% filter(is.na(NCC)) %>% group_by(deces_code_lieu) %>%
  summarise(n = n()) %>% filter(!grepl("^9[7-9]", deces_code_lieu)) #111 communes
sum(communes_inconnues$n) #4394 décès

A <- deces_15_19 %>% group_by(annee_deces, DEP) %>% summarise(n = n(), age = mean(age)) %>%
  filter(annee_deces > 2014) %>%
  arrange(age, .by_group = T)

#deces_15_19 %>% filter(annee_deces == 2015) %>% distinct(deces_code_lieu)


#APL

files_list <- list.files(sprintf('%s', APL_path), pattern = '^APL_mg_[0-9]+.xlsx', full.names = T)
APL <- lapply(files_list, read_excel)
for (i in 1:5){
  APL[[i]] %<>% mutate(annee = 2014 + i)
}

APL_15_19 <- bind_rows(APL) %>% arrange(`Code commune`) %>%
  rename(APL = `APL aux médecins généralistes (sans borne d'âge)`,
         APL_moins_65 = `APL aux médecins généralistes de moins de 65 ans`,
         APL_moins_62 = `APL aux médecins généralistes de moins de 62 ans`,
         Pop_standardisee = `Population standardisée par la consommation de soins par tranche d'âge`)

APL_15_19 %<>% group_by(`Code commune`) %>% 
  mutate(max_variation = max(APL) - min(APL))

B <- deces_15_19 %>% filter(!(deces_code_lieu %chin% APL_15_19$`Code commune`)) #49000 obs, plupart à l'étranger


#densité médicale

files_list <- list.files(sprintf('%s', densite_path), full.names = T)
densite <- lapply(files_list, function(i){read_csv2(i, skip = 2)})
for (i in 1:5){
  densite[[i]] %<>% mutate(annee = 2014 + i) %>% rename(nb_medecins = 3)
}

densite_15_19 <- bind_rows(densite) %>% arrange(Code)

B <- deces_15_19 %>% filter(!(deces_code_lieu %chin% densite_15_19$Code))







#variance between/within (panel)

deces_15_19_commune <- deces_15_19 %>% filter(annee_deces >= 2015) %>% #& age >= 50
  group_by(deces_code_lieu, annee_deces) %>%
  summarise(nb_morts = n(), age_moyen_deces = mean(age)) %>%
  left_join(APL_15_19, by = c('deces_code_lieu' = 'Code commune', 'annee_deces' = 'annee')) %>%
  left_join(densite_15_19, by = c('deces_code_lieu' = 'Code', 'annee_deces' = 'annee')) %>%
  filter(!is.na(APL) & !is.na(nb_medecins)) %>%
  group_by(deces_code_lieu) %>%
  mutate(n = n(), densite = nb_medecins*1000/`Population totale`,
         log_nb_morts = log(nb_morts), log_pop = log(`Population totale`)) %>% 
  filter(n == 5) #on se restreint aux communes avec au moins 1 mort/an

nb_communes <- n_distinct(deces_15_19_commune$deces_code_lieu) #12042 communes avec au moins 1 mort/an

n <- nrow(deces_15_19_commune)

deces_15_19_commune %<>% ungroup() %>% mutate(moy_age = mean(age_moyen_deces),
                                moy_APL = mean(APL),
                                moy_densite = mean(densite))

deces_15_19_commune %<>% group_by(deces_code_lieu) %>% mutate(moy_age_com = mean(age_moyen_deces),
                                                              moy_APL_com = mean(APL),
                                                              moy_densite_com = mean(densite)) %>%
  mutate(ecart_age_with = age_moyen_deces - moy_age_com,
         ecart_APL_with = APL - moy_APL_com,
         ecart_densite_with = densite - moy_densite_com)

overall_var_age_deces <- var(deces_15_19_commune$age_moyen_deces)
overall_var_APL <- var(deces_15_19_commune$APL)
overall_var_densite <- var(deces_15_19_commune$densite)

between_var <- deces_15_19_commune %>% distinct(deces_code_lieu, .keep_all = T) %>%
  mutate(ecart_age_betw = moy_age_com - moy_age,
         ecart_APL_betw = moy_APL_com - moy_APL,
         ecart_densite_betw = moy_densite_com - moy_densite)
between_var_age_deces <- sum((between_var$ecart_age_betw)^2)/nb_communes
between_var_APL <- sum((between_var$ecart_APL_betw)^2)/nb_communes
between_var_densite <- sum((between_var$ecart_densite_betw)^2)/nb_communes

within_var_age_deces <- sum((deces_15_19_commune$ecart_age_with)^2)/(n-1)
within_var_APL <- sum((deces_15_19_commune$ecart_APL_with)^2)/(n-1)
within_var_densite <- sum((deces_15_19_commune$ecart_densite_with)^2)/(n-1)

var <- data.frame(Variable = c("age_moyen_deces", "APL", "densite"),
                  Overall = c(overall_var_age_deces, overall_var_APL, overall_var_densite),
                  Between = c(between_var_age_deces, between_var_APL, between_var_densite),
                  Within = c(within_var_age_deces, within_var_APL, within_var_densite))
var %<>% mutate(Part_within = Within/Overall)



deces_15_19_commune1 <- deces_15_19_commune %>% group_by(deces_code_lieu) %>%
  mutate(nb_min_morts = min(nb_morts)) %>%
  filter(nb_min_morts >= 5)

nb_communes <- length(unique(deces_15_19_commune1$deces_code_lieu)) #12042 communes avec au moins 1 mort/an

n <- nrow(deces_15_19_commune1)

deces_15_19_commune1 %<>% ungroup() %>% mutate(moy_age = mean(age_moyen_deces),
                                              moy_APL = mean(APL),
                                              moy_densite = mean(densite))

overall_var_age_deces1 <- var(deces_15_19_commune1$age_moyen_deces)
overall_var_APL1 <- var(deces_15_19_commune1$APL)
overall_var_densite1 <- var(deces_15_19_commune1$densite)

between_var1 <- deces_15_19_commune1 %>% distinct(deces_code_lieu, .keep_all = T) %>%
  mutate(ecart_age_betw1 = moy_age_com - moy_age,
         ecart_APL_betw1 = moy_APL_com - moy_APL,
         ecart_densite_betw1 = moy_densite_com - moy_densite)
between_var_age_deces1 <- sum((between_var1$ecart_age_betw1)^2)/nb_communes
between_var_APL1 <- sum((between_var1$ecart_APL_betw1)^2)/nb_communes
between_var_densite1 <- sum((between_var1$ecart_densite_betw1)^2)/nb_communes

within_var_age_deces1 <- sum((deces_15_19_commune1$ecart_age_with)^2)/(n-1)
within_var_APL1 <- sum((deces_15_19_commune1$ecart_APL_with)^2)/(n-1)
within_var_densite1 <- sum((deces_15_19_commune1$ecart_densite_with)^2)/(n-1)

var1 <- data.frame(Variable = c("age_moyen_deces", "APL", "densite"),
                  Overall = c(overall_var_age_deces1, overall_var_APL1, overall_var_densite1),
                  Between = c(between_var_age_deces1, between_var_APL1, between_var_densite1),
                  Within = c(within_var_age_deces1, within_var_APL1, within_var_densite1))
var1 %<>% mutate(Part_within = Within/Overall)




#régression effet fixe

#panel équilibré
data <- deces_15_19_commune %>% group_by(deces_code_lieu) %>% mutate(nb_min = min(nb_morts))
data %>% filter(nb_min >= 5) %>% pull(deces_code_lieu) %>% n_distinct() #5693 communes avec au moins 5 morts/an


fixed <- feols(age_moyen_deces ~ APL + log_pop | deces_code_lieu, data=data)
summary(fixed)

fixed <- feols(age_moyen_deces ~ densite + log_pop | deces_code_lieu, data=data)
summary(fixed)

fixed <- feols(log_nb_morts ~ APL + log_pop | deces_code_lieu, data=data)
summary(fixed)

fixed <- feols(log_nb_morts ~ densite + log_pop | deces_code_lieu, data=data)
summary(fixed)

  

fixed <- feols(age_moyen_deces ~ APL + log_pop | deces_code_lieu, data=data %>% filter(nb_min >= 5))
summary(fixed)

fixed <- feols(age_moyen_deces ~ densite + log_pop | deces_code_lieu, data=data %>% filter(nb_min >= 5))
summary(fixed)

fixed <- feols(log_nb_morts ~ APL + log_pop | deces_code_lieu, data=data %>% filter(nb_min >= 5))
summary(fixed)

fixed <- feols(log_nb_morts ~ densite + log_pop | deces_code_lieu, data=data %>% filter(nb_min >= 5))
summary(fixed)



#instrumenter APL par densité

fixed <- feols(age_moyen_deces ~ log_pop | deces_code_lieu | APL ~ densite, data=data)
summary(fixed)

fixed <- feols(log_nb_morts ~ log_pop | deces_code_lieu | APL ~ densite, data=data)
summary(fixed)

fixed <- feols(age_moyen_deces ~ log_pop | deces_code_lieu | APL ~ densite, data=data %>% filter(nb_min >= 5))
summary(fixed)

fixed <- feols(log_nb_morts ~ log_pop | deces_code_lieu | APL ~ densite, data=data %>% filter(nb_min >= 5))
summary(fixed)




## 2018 ##

#cross-section avec seulement APL

deces_18 <- deces_15_19_commune %>%
  filter(annee_deces == 2018)

deces_18 %>% filter(!(deces_code_lieu %in% APL_18$`Code commune`)) %>% view() #communes déléguées ou à l'étranger

tvs <- read_excel("~/ENSAE/3A/Projet_DSSS/Données/Correspondance_communes_tvs_2019.xlsx")

deces_18 %<>% left_join(tvs, by = c('deces_code_lieu' = 'Code commune'))

model <- lm(age_moyen_deces ~ APL, data = deces_18)
summary(model)

model2 <- lm(log_nb_morts ~ APL + log_pop, data = deces_18)
summary(model2)

model <- lm.cluster(age_moy_deces ~ APL, cluster = 'Code du territoire de vie-santé', data = deces_18)
summary(model)


#cross-section avec APL + covariables

#structure par âge, sexe et PCS

recensement <- read_excel(sprintf('%s/population/base-cc-evol-struct-pop-2018.xlsx', covariable_path), sheet = "COM_2018", skip = 5)
arrdt <- read_excel(sprintf('%s/population/base-cc-evol-struct-pop-2018.xlsx', covariable_path), sheet = "ARM_2018", skip = 5)
recensement %<>% bind_rows(arrdt)

var_sexe_age <- c("P18_H0014", "P18_H1529", "P18_H3044", "P18_H4559", "P18_H6074", "P18_H7589",
         "P18_H90P", "P18_F0014", "P18_F1529", "P18_F3044", "P18_F4559", "P18_F6074",
         "P18_F7589", "P18_F90P")

recensement %<>% mutate(across(var_sexe_age, ~./P18_POP, .names = "{col}_prop"))

var_PCS <- c("C18_POP15P_CS1", "C18_POP15P_CS2", "C18_POP15P_CS3", "C18_POP15P_CS4",
         "C18_POP15P_CS5", "C18_POP15P_CS6", "C18_POP15P_CS7", "C18_POP15P_CS8")

recensement %<>% mutate(across(var_PCS, ~./C18_POP15P, .names = "{col}_prop"))

recensement %<>% relocate(c(1:6, C18_POP15P, 109:130,))

taille_menage <- read_csv2(sprintf('%s/population/taille_moyenne_menage_2018.csv', covariable_path))
taille_menage <- taille_menage[-c(1:2),c(1,3)]
names(taille_menage) <- c("CODGEO", "Taille_menage")
taille_menage %<>% mutate(Taille_menage = as.numeric(Taille_menage))
recensement %<>% left_join(taille_menage, by = 'CODGEO')


#niveau de vie médian

filosofi <- read_excel(sprintf('%s/Filosofi/base-cc-filosofi-2018-geo2021.xlsx', covariable_path), sheet = "COM", skip = 5)
filosofi %<>% select(CODGEO, MED18)
recensement %<>% left_join(filosofi, by = 'CODGEO')

#régression multiple

deces_18 %<>% left_join(recensement, by = c('deces_code_lieu' = 'CODGEO'))

model <- lm(age_moyen_deces ~ APL + P18_H0014_prop + P18_H1529_prop +
            P18_H4559_prop + P18_H6074_prop + P18_H7589_prop + P18_H90P_prop + P18_F0014_prop +
            P18_F1529_prop + P18_F3044_prop + P18_F4559_prop + P18_F6074_prop + P18_F7589_prop +
            P18_F90P_prop + C18_POP15P_CS1_prop + C18_POP15P_CS2_prop + C18_POP15P_CS3_prop +
            C18_POP15P_CS4_prop + C18_POP15P_CS5_prop + C18_POP15P_CS6_prop + C18_POP15P_CS7_prop + 
            as.numeric(Taille_menage) + MED18, data = deces_18)
summary(model)

model2 <- lm(log_nb_morts ~ APL + log_pop + P18_H0014_prop + P18_H1529_prop +
              P18_H4559_prop + P18_H6074_prop + P18_H7589_prop + P18_H90P_prop + P18_F0014_prop +
              P18_F1529_prop + P18_F3044_prop + P18_F4559_prop + P18_F6074_prop + P18_F7589_prop +
              P18_F90P_prop + C18_POP15P_CS1_prop + C18_POP15P_CS2_prop + C18_POP15P_CS3_prop +
              C18_POP15P_CS4_prop + C18_POP15P_CS5_prop + C18_POP15P_CS6_prop + C18_POP15P_CS7_prop + 
              as.numeric(Taille_menage) + MED18, data = deces_18)
summary(model2)




#Niveau TVS

tvs <- read_excel("~/ENSAE/3A/Projet_DSSS/Données/Correspondance_communes_tvs_2019.xlsx")

deces_15_19 %<>% distinct(nom, sexe, naissance_date_complete, naissance_code_lieu, deces_date_complete, .keep_all = T) %>%
  left_join(tvs, by = c('deces_code_lieu' = 'Code commune')) %>%
  mutate(annee_deces = year(deces_date_complete))

communes_inconnues_tvs <- deces_15_19 %>% filter(is.na(`Libellé du territoire de vie-santé`)) %>% group_by(deces_code_lieu) %>%
  summarise(n = n()) %>% filter(!grepl("^9[7-9]", deces_code_lieu)) #111 communes
sum(communes_inconnues_tvs$n) #10948 décès


#APL

files_list <- list.files(sprintf('%s', APL_path), pattern = '^APL_mg_[0-9]+_tvs.xlsx', full.names = T)
APL_tvs <- lapply(files_list, read_excel)
for (i in 1:5){
  APL_tvs[[i]] %<>% mutate(annee = 2014 + i)
}

APL_15_19_tvs <- bind_rows(APL_tvs) %>% arrange(`Code du territoire de vie-santé`) %>%
  rename(APL = `APL aux médecins généralistes (sans borne d'âge)`,
         APL_moins_65 = `APL aux médecins généralistes de moins de 65 ans`,
         APL_moins_62 = `APL aux médecins généralistes de moins de 62 ans`,
         Pop = `Population totale du territoire de vie-santé`,
         Pop_standardisee = `Population standardisée par la consommation de soins par tranche d'âge`)


files_list <- list.files(sprintf('%s/2015-2019', densite_path), full.names = T)
densite <- lapply(files_list, function(i){read_csv2(i, skip = 2)})
for (i in 1:5){
  densite[[i]] %<>% mutate(annee = 2014 + i) %>% rename(nb_medecins = 3)
}

densite_15_19_tvs <- bind_rows(densite) %>% arrange(Code)


deces_15_19_tvs <- deces_15_19 %>% filter(annee_deces >= 2015 & !is.na(`Code du territoire de vie-santé`)) %>% #& age >= 50
  group_by(`Code du territoire de vie-santé`, annee_deces) %>%
  summarise(nb_morts = n(), age_moyen_deces = mean(age)) %>%
  left_join(APL_15_19_tvs, by = c('Code du territoire de vie-santé' = 'Code du territoire de vie-santé', 'annee_deces' = 'annee')) %>%
  left_join(densite_15_19_tvs, by = c('Code du territoire de vie-santé' = 'Code', 'annee_deces' = 'annee')) %>%
  filter(!is.na(APL) & !is.na(nb_medecins)) %>%
  mutate(densite = nb_medecins*1000/Pop,
         log_nb_morts = log(nb_morts), log_pop = log(Pop)) %>%
  group_by(`Code du territoire de vie-santé`) %>%
  mutate(n = n()) %>% 
  filter(n == 5) #on se restreint aux tvs avec au moins 1 mort/an


data <- deces_15_19_tvs %>% group_by(`Code du territoire de vie-santé`) %>% mutate(nb_min = min(nb_morts))
data %>% filter(nb_min >= 5) %>% pull(`Code du territoire de vie-santé`) %>% n_distinct() #2792 tvs avec au moins 5 morts/an

fixed <- feols(age_moyen_deces ~ APL + log_pop | `Code du territoire de vie-santé`, data=data)
summary(fixed)

fixed <- feols(age_moyen_deces ~ densite + log_pop | `Code du territoire de vie-santé`, data=data)
summary(fixed)

fixed <- feols(log_nb_morts ~ APL + log_pop | `Code du territoire de vie-santé`, data=data)
summary(fixed)

fixed <- feols(log_nb_morts ~ densite + log_pop | `Code du territoire de vie-santé`, data=data)
summary(fixed)








#base equipements

data <- read_csv2(file.choose())
A <- data %>% distinct(UU2020, .keep_all = T)
A <- data %>% filter(TYPEQU == "D201")
