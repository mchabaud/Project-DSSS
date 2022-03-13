#répertoires
mortalite_path <- "~/ENSAE/3A/Projet_DSSS/Données/Mortalité"
APL_path <- "~/ENSAE/3A/Projet_DSSS/Données/APL_densité_médicale/APL"
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
  deces_numero_acte = 9
)

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

table_commune <- read_csv("~/ENSAE/3A/Projet_DSSS/commune2021.csv") %>%
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

B <- deces_15_19 %>% filter(!(deces_code_lieu %chin% APL$`Code commune`)) #48000 obs, plupart à l'étranger


deces_15 <- deces_15_19 %>% select(1:12) %>%
  mutate(annee_deces = year(deces_date_complete)) %>%
  filter(annee_deces == 2015) %>%
  group_by(deces_code_lieu) %>%
  summarise(age_moy_deces = mean(age))

deces_15 %<>% left_join(APL, by = c('deces_code_lieu' = 'Code commune'))

deces_15 %<>% rename(APL = `APL aux médecins généralistes (sans borne d'âge)`) %>%
  filter(!is.na(`Population totale`))

model <- lm(age_moy_deces ~ APL, data = deces_15)
summary(model)


## 2018 ##

#cross-section avec seulement APL

deces_18 <- deces_15_19 %>%
  filter(annee_deces == 2018) %>%
  group_by(deces_code_lieu) %>%
  summarise(nb_morts = n(), age_moy_deces = mean(age))

APL_18 <- APL[[4]]

deces_18 %>% filter(!(deces_code_lieu %in% APL_18$`Code commune`)) %>% view() #communes déléguées ou à l'étranger

deces_18 %<>% left_join(APL_18, by = c('deces_code_lieu' = 'Code commune')) %>%
  rename(APL = `APL aux médecins généralistes (sans borne d'âge)`)

deces_18 %<>% rename(APL = `APL aux médecins généralistes (sans borne d'âge)`)

model <- lm(age_moy_deces ~ APL, data = deces_18)
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

deces_18 %<>% rename(APL_62 = `APL aux médecins généralistes de moins de 62 ans`) %>%
  mutate(log_nb_morts = log(nb_morts))

model <- lm(age_moy_deces ~ APL + P18_H0014_prop + P18_H1529_prop +
            P18_H4559_prop + P18_H6074_prop + P18_H7589_prop + P18_H90P_prop + P18_F0014_prop +
            P18_F1529_prop + P18_F3044_prop + P18_F4559_prop + P18_F6074_prop + P18_F7589_prop +
            P18_F90P_prop + C18_POP15P_CS1_prop + C18_POP15P_CS2_prop + C18_POP15P_CS3_prop +
            C18_POP15P_CS4_prop + C18_POP15P_CS5_prop + C18_POP15P_CS6_prop + C18_POP15P_CS7_prop + 
            as.numeric(Taille_menage) + MED18, data = deces_18 %>% filter(nb_morts > 3))
summary(model)

model <- lm(log_nb_morts ~ APL + `Population totale` + P18_H0014_prop + P18_H1529_prop +
              P18_H4559_prop + P18_H6074_prop + P18_H7589_prop + P18_H90P_prop + P18_F0014_prop +
              P18_F1529_prop + P18_F3044_prop + P18_F4559_prop + P18_F6074_prop + P18_F7589_prop +
              P18_F90P_prop + C18_POP15P_CS1_prop + C18_POP15P_CS2_prop + C18_POP15P_CS3_prop +
              C18_POP15P_CS4_prop + C18_POP15P_CS5_prop + C18_POP15P_CS6_prop + C18_POP15P_CS7_prop + 
              as.numeric(Taille_menage) + MED18, data = deces_18 %>% filter(nb_morts > 5))
summary(model)


#variance between/within (panel)

