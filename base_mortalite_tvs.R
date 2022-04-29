library("haven")
library(tidyverse)
library(lubridate)
library(tictoc)
library(magrittr)
library(fixest)
library(lfe)
library("readxl")
library("writexl")
library(data.table)

#répertoires
mortalite_path <- "~/ENSAE/3A/Projet_DSSS/Données/Mortalité"
APL_path <- "~/ENSAE/3A/Projet_DSSS/Données/APL_densité_médicale/APL"
densite_path <- "~/ENSAE/3A/Projet_DSSS/Données/APL_densité_médicale/Densité médicale"
covariable_path <- "~/ENSAE/3A/Projet_DSSS/Données/Covariables"

communes <- read_csv("~/ENSAE/3A/Projet_DSSS/Données/communes2019.csv") %>%
  rename(libelle_commune = ncc) %>%
  distinct(com, .keep_all = T) #37253 communes/arrondissements municipaux

tvs <- read_excel("~/ENSAE/3A/Projet_DSSS/Données/Correspondance_communes_tvs_2019.xlsx") %>%
  rename(code_commune = `Code commune`) #35012 communes/arrondissements municipaux

#pour les communes déléguées, on garde leur code pour le match avec les décès mais on garde aussi la commune "parent" pour faire le match avec les tvs
communes %<>% mutate(code_commune = if_else(is.na(comparent) | typecom == "ARM", com, comparent)) #pour les communes déléguées, on garde leur code pour le match avec les décès mais on garde aussi la commune 

communes_tvs <- communes %>% select(typecom, com, reg, dep, libelle_commune, code_commune) %>%
  left_join(tvs, by = "code_commune") %>%
  select(-`Libellé de la commune`) %>%
  rename(code_tvs = `Code du territoire de vie-santé`)

rm(communes, tvs) ; gc()


#décès

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
      #naissance_date_brute = naissance_date,
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
    distinct(nom, sexe, naissance_date_complete, naissance_code_lieu, deces_date_complete, deces_code_lieu, .keep_all = T) %>%
    filter(!is.na(deces_code_lieu) & !grepl("^9[7-9]", deces_code_lieu)) %>% #on enlève les décès où le lieu est non renseigné ou à l'étranger
    mutate(age = round(as.numeric((deces_date_complete - naissance_date_complete)/365),1),
           annee_deces = year(deces_date_complete)) %>%
    select(nom, sexe, naissance_code_lieu, deces_date_complete, deces_code_lieu, age, annee_deces)
  clean_df
}

files_list <- list.files(sprintf('%s', mortalite_path), pattern = 'deces-[0-9]+.txt', full.names = T)
files_list_04_11 <- files_list[1:8]
files_list_12_19 <- files_list[9:16]

deces_04_11 <- lapply(files_list_04_11, read_fwf,
                      col_positions = fwf_widths(positions, col_names = names(positions)),
                      col_types = cols(
                        .default = col_character())
)

deces_04_11 <- lapply(deces_04_11, clean_date)

tic()
deces_04_11 <- bind_rows(deces_04_11) %>% distinct(.keep_all = T)
toc()
setDT(deces_04_11)

deces_12_19 <- lapply(files_list_12_19, read_fwf,
                      col_positions = fwf_widths(positions, col_names = names(positions)),
                      col_types = cols(
                        .default = col_character())
)

deces_12_19 <- lapply(deces_12_19, clean_date)

tic()
deces_12_19 <- bind_rows(deces_12_19) %>% distinct(.keep_all = T)
toc()
setDT(deces_12_19)


deces_04_19 <- rbindlist(list(deces_04_11, deces_12_19))

rm(deces_04_11, deces_12_19) ; gc()

#all_equal(A, as_tibble(B))

deces_04_19 <- deces_04_19[annee_deces >= 2007 & annee_deces <= 2018]

deces_lieu <- deces_04_19[, .(nb_morts = .N), by = "deces_code_lieu"]
deces_lieu %>% as_tibble() %>% filter(!(deces_code_lieu %in% communes_tvs$com) &
                                        !grepl("^9[7-9]", deces_code_lieu)) %>% view()

communes_tvs_restreint <- communes_tvs %>% select(com, `Code du territoire de vie-santé`) %>%
  rename(code_tvs = `Code du territoire de vie-santé`)

deces_04_19 <- merge(deces_04_19, communes_tvs_restreint, all.x = TRUE, by.x = "deces_code_lieu", by.y = "com")

deces_04_19 <- deces_04_19 %>% as_tibble() %>% 
  mutate(code_tvs = case_when(
    deces_code_lieu == "28042" ~ "28015",
    deces_code_lieu == "61147" ~ "28280",
    deces_code_lieu == "14604" ~ "14225",
    deces_code_lieu == "72033" ~ "72089",
    deces_code_lieu == "50406" ~ "50237",
    deces_code_lieu == "27024" ~ "27198",
    deces_code_lieu == "14185" ~ "14514",
    deces_code_lieu == "28066" ~ "28051",
    deces_code_lieu == "79211" ~ "79083",
    deces_code_lieu == "38312" ~ "38565",
    deces_code_lieu == "74274" ~ "74225",
    deces_code_lieu == "61042" ~ "28280",
    deces_code_lieu == "49144" ~ "44180",
    deces_code_lieu == "74110" ~ "74224",
    deces_code_lieu == "27293" ~ "27198",
    deces_code_lieu == "27166" ~ "27198",
    deces_code_lieu == "14513" ~ "50601",
    deces_code_lieu == "14697" ~ "14654",
    deces_code_lieu == "30190" ~ "30350",
    deces_code_lieu == "44160" ~ "44003",
    deces_code_lieu == "44060" ~ "49244",
    deces_code_lieu == "74181" ~ "74112",
    TRUE ~ code_tvs
  ))

deces_04_19_tvs <- deces_04_19 %>% group_by(code_tvs, annee_deces) %>%
  summarise(nb_morts = n(), age_moyen_deces = mean(age)) %>%
  ungroup() %>%
  rename(annee = annee_deces)

n_distinct(communes_tvs_restreint$code_tvs) #2841
n_distinct(deces_04_19_tvs$code_tvs) #2728



#données population

col_names_99_12 <- c("code_tvs", "libelle_tvs", "F15_29", "pop_tot", "plus75",
                  "60_74", "45_59", "30_44", "15_29", "Fpop_tot", "Fplus75", "F60_74", "F45_59",
                  "F30_44")

col_names_08_17 <- c("code_tvs", "libelle_tvs", "F15_29", "CSretraites", "pop_tot", 'CSagriculteurs', "plus75",
               "60_74", "45_59", "30_44", "15_29", "Fpop_tot", "Fplus75", "F60_74", "F45_59",
               "F30_44", "CSartisans_commercants", "CSouvriers", "CSemployes", "CScadres", "CSprof_interm")

col_names_18 <- c("code_tvs", "libelle_tvs", "pop_tot", "CSretraites", "plus75", "60_74", "45_59", "30_44",
                  "15_29", "Fpop_tot", "Fplus75", "F60_74", "F45_59", "F30_44", "F15_29", "CSagriculteurs",
                  "CSartisans_commercants", "CSouvriers", "CSemployes", "CScadres", "CSprof_interm")

files_list <- list.files(sprintf('%s/population/', covariable_path), pattern = 'pop_tvs_[0-9]+.xlsx', full.names = T)[2:14]

import_pop <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee <- as.numeric(sprintf("20%s", str_extract(file1, "[0-9]+")))
  if(annee %in% c(1999, 2006, 2007, 2011, 2012)){
    col_names <- col_names_99_12
  }
  else if(annee == 2018){
    col_names <- col_names_18
  }
  else{col_names <- col_names_08_17}
  data <- read_excel(file, col_names = col_names, skip = 4)
  data %<>% mutate(across(3:ncol(data), as.numeric)) %>% mutate(annee = !!annee)
}

pop <- lapply(files_list, import_pop)
pop <- bind_rows(pop) %>% arrange(code_tvs, annee)
pop %<>% mutate(across(starts_with("CS"), ~case_when(annee == 2006 ~ lead(., 2)*(lead(.,2)/lead(.,3))^2, #pmax(round(),0)
                                                     annee == 2007 ~ lead(., 1)*(lead(.,1)/lead(.,2)),
                                                     annee == 2011 ~ lag(., 1)*(lead(.,2)/lag(.,1))^(1/3),
                                                     annee == 2012 ~ lag(., 2)*(lead(.,1)/lag(.,2))^(2/3),
                                                     TRUE ~ .))) %>%
  mutate(across(starts_with("CS"), ~replace_na(.,0))) %>%
  relocate(code_tvs, libelle_tvs, annee, pop_tot,) %>%
  mutate(across(c(5:15), ~.*100/pop_tot))

pop %>% filter(CSartisans_commercants == 0) %>% view()
pop %>% filter(is.na(F15_29)) %>% view()

#en attendant d'imputer pour marseille et lyon
pop %<>% mutate(missing = if_else(is.na(pop_tot), 1, 0)) %>%
  group_by(code_tvs) %>% mutate(missing = max(missing)) %>%
  filter(missing == 0) %>%
  ungroup()

n_distinct(pop$code_tvs) #2808 tvs
deces_04_19_tvs %>% filter(!(code_tvs %in% pop$code_tvs)) %>% view()

pop %<>% mutate(code_tvs = case_when(
  code_tvs == "22007" ~ "22055",
  code_tvs == "35129" ~ "35176",
  code_tvs == "49313" ~ "49218",
  TRUE ~ code_tvs))


#BPE: on s'int�resse �:
#D101 : établissement santé court séjour
#D106 : service d'urgences
#D401 : établissement personnes âgées hébérgement

files_list1 <- list.files(sprintf('%s/BPE/', covariable_path), pattern = 'bpe[0-9]+_ensemble.csv', full.names = T)
files_list2 <- list.files(sprintf('%s/BPE/', covariable_path), pattern = 'bpe[0-9]+_ensemble_com.csv', full.names = T)[1:6]

import_bpe1 <- function(file){
  data <- read_csv2(file) %>% filter(TYPEQU %in% c("D101", "D106", "D401")) %>%
    group_by(DEPCOM, AN, TYPEQU) %>% summarise(nb = sum(NB_EQUIP)) %>%
    ungroup %>%
    pivot_wider(names_from = TYPEQU, values_from = nb) %>%
    rename(CODGEO = DEPCOM, NB_D101 = D101, NB_D106 = D106, NB_D401 = D401, annee = AN) %>%
    mutate(across(starts_with("NB"), ~replace_na(.,0)))
}

import_bpe2 <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee <- as.numeric(sprintf("20%s", str_extract(file1, "[0-9]+")))
  data <- read_csv2(file) %>% select(CODGEO, NB_D101, NB_D106, NB_D401) %>%
    mutate(annee = !!annee)
}

bpe1 <- lapply(files_list1, import_bpe1)
bpe2 <- lapply(files_list2, import_bpe2)
bpe <- bind_rows(bpe1, bpe2)

A <- bpe %>% filter(!(CODGEO %chin% communes_tvs$com) & (NB_D101 > 0 | NB_D106 > 0 | NB_D401 > 0)) %>% distinct(CODGEO) #6 communes

bpe_1 <- bpe %>% left_join(communes_tvs_restreint, by = c("CODGEO" = "com")) %>%
  mutate(code_tvs = case_when(
    CODGEO == "14513" ~ "50601",
    CODGEO == "44160" ~ "44003",
    CODGEO == "44060" ~ "49244",
    CODGEO == "74181" ~ "74112",
    TRUE ~ code_tvs)) %>%
  group_by(code_tvs, annee) %>%
  summarise(NB_D101 = sum(NB_D101), NB_D106 = sum(NB_D106), NB_D401 = sum(NB_D401)) %>%
  ungroup()

bpe_1 %>% filter(NB_D101 == 0 & NB_D106 == 0 & NB_D401 == 0) %>% view()




#densit� m�dicale

files_list <- list.files(sprintf('%s', densite_path), pattern = '[0-9]+_tvs.csv', full.names = T)[4:15]

import_densite <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee <- as.numeric(str_extract(file1, "[0-9]+"))
  data <- read_csv2(file, skip = 2) %>% rename(nb_medecins = 3, code_tvs = Code) %>%
    mutate(nb_medecins = as.numeric(nb_medecins), annee = !!annee) %>%
    select(-`Libell�`)
}

densite <- lapply(files_list, import_densite)
densite <- bind_rows(densite)

#deces_04_19_tvs %>% filter(!(code_tvs %in% densite$code_tvs))

densite %<>% mutate(code_tvs = case_when(
  code_tvs == "22007" ~ "22055",
  code_tvs == "35129" ~ "35176",
  code_tvs == "49313" ~ "49218",
  TRUE ~ code_tvs))

#densite %>% filter(is.na(nb_medecins))



data_tvs <- tibble(code_tvs = rep(unique(deces_04_19_tvs$code_tvs), 12),
                       annee = rep(c(2007:2018), each = n_distinct(deces_04_19_tvs$code_tvs)))


data_tvs %<>% left_join(pop) %>%
  left_join(bpe_1) %>%
  left_join(densite) %>%
  left_join(deces_04_19_tvs) %>%
  mutate(across(starts_with("NB"), ~replace_na(.,0))) %>%
  filter(!is.na(pop_tot)) %>%
  mutate(densite = 1000*nb_medecins/pop_tot, log_pop = log(pop_tot), log_morts = log(nb_morts))

fixed <- feols(age_moyen_deces ~ densite + log_pop + NB_D401 + NB_D106 + NB_D101 + CSprof_interm + CScadres +
                 CSemployes + CSouvriers + CSartisans_commercants + CSagriculteurs + CSretraites + F30_44 + 
                 F45_59 + F60_74 + Fplus75 + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + F15_29
                 | code_tvs, data=data_tvs)
summary(fixed)

fixed <- feols(log_morts ~ densite + log_pop + NB_D401 + NB_D106 + NB_D101 + CSprof_interm + CScadres +
                 CSemployes + CSouvriers + CSartisans_commercants + CSagriculteurs + CSretraites + F30_44 + 
                 F45_59 + F60_74 + Fplus75 + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + F15_29
               | code_tvs, data=data_tvs)
summary(fixed)

A <- data_tvs %>% filter(annee == 2014)
quantile(A$densite, probs = c(0.08))

B <- data_tvs %>% mutate(sous_dense = if_else(densite <= 0.53, 1, 0)) %>%
  group_by(code_tvs) %>% mutate(sous_dense = max(sous_dense)) %>%
  filter(sous_dense == 1)

fixed <- feols(age_moyen_deces ~ densite + log_pop + NB_D401 + NB_D106 + NB_D101 + CSprof_interm + CScadres +
                 CSemployes + CSouvriers + CSartisans_commercants + CSagriculteurs + CSretraites + F30_44 + 
                 F45_59 + F60_74 + Fplus75 + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + F15_29
               | code_tvs, data=B)
summary(fixed)

fixed <- feols(log_morts ~ densite + log_pop + NB_D401 + NB_D106 + NB_D101 + CSprof_interm + CScadres +
                 CSemployes + CSouvriers + CSartisans_commercants + CSagriculteurs + CSretraites + F30_44 + 
                 F45_59 + F60_74 + Fplus75 + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + F15_29
               | code_tvs, data=B)
summary(fixed)


data_tvs %<>% filter(!is.na(pop_tot))
data_tvs %>% filter(is.na(nb_medecins))



#decomposition variance

data_tvs1 <- data_tvs %>% filter(!is.na(age_moyen_deces) & !is.na(densite) & !is.na(nb_morts)) %>%
  mutate(nb_morts_hab = nb_morts/pop_tot) %>%
  mutate(moy_age = mean(age_moyen_deces),
         moy_nb_morts = mean(nb_morts_hab),
         moy_densite = mean(densite))

nb_tvs <- n_distinct(data_tvs1$code_tvs)
n <- nrow(data_tvs1)

data_tvs1 %<>% group_by(code_tvs) %>% mutate(moy_age_tvs = mean(age_moyen_deces),
                                             moy_nb_morts_tvs = mean(nb_morts_hab),
                                             moy_densite_tvs = mean(densite)) %>%
  mutate(ecart_age_with = age_moyen_deces - moy_age_tvs,
         ecart_nb_morts_with = nb_morts_hab - moy_nb_morts_tvs,
         ecart_densite_with = densite - moy_densite_tvs) %>%
  ungroup()

overall_var_age_deces <- var(data_tvs1$age_moyen_deces)
overall_var_nb_morts <- var(data_tvs1$nb_morts_hab)
overall_var_densite <- var(data_tvs1$densite)

between_var <- data_tvs1 %>% distinct(code_tvs, .keep_all = T) %>%
  mutate(ecart_age_betw = moy_age_tvs - moy_age,
         ecart_nb_morts_betw = moy_nb_morts_tvs - moy_nb_morts,
         ecart_densite_betw = moy_densite_tvs - moy_densite)
between_var_age_deces <- sum((between_var$ecart_age_betw)^2)/nb_tvs
between_var_nb_morts <- sum((between_var$ecart_nb_morts_betw)^2)/nb_tvs
between_var_densite <- sum((between_var$ecart_densite_betw)^2)/nb_tvs

within_var_age_deces <- sum((data_tvs1$ecart_age_with)^2)/(n-1)
within_var_nb_morts <- sum((data_tvs1$ecart_nb_morts_with)^2)/(n-1)
within_var_densite <- sum((data_tvs1$ecart_densite_with)^2)/(n-1)

var <- data.frame(Variable = c("age_moyen_deces", "Nb_morts_hab", "densite"),
                  Overall = c(overall_var_age_deces, overall_var_nb_morts, overall_var_densite),
                  Between = c(between_var_age_deces, between_var_nb_morts, between_var_densite),
                  Within = c(within_var_age_deces, within_var_nb_morts, within_var_densite))
var %<>% mutate(Part_within = Within/Overall)

