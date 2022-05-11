#### Table de passage communes/TVS ####

communes <- read_csv("~/ENSAE/3A/Projet_DSSS/Données/communes2019.csv") %>%
  rename(libelle_commune = ncc) %>%
  distinct(com, .keep_all = T) #37253 communes/arrondissements municipaux

tvs <- read_excel("~/ENSAE/3A/Projet_DSSS/Données/Correspondance_communes_tvs_2019.xlsx") %>%
  rename(code_commune = `Code commune`) #35012 communes/arrondissements municipaux

#pour les communes déléguées, on garde leur code pour le match avec les décès mais on garde aussi la commune "parent" pour faire le match avec les tvs
communes %<>% mutate(code_commune = if_else(is.na(comparent) | typecom == "ARM", com, comparent))

communes_tvs <- communes %>% select(typecom, com, reg, dep, libelle_commune, code_commune) %>%
  left_join(tvs, by = "code_commune") %>%
  select(-`Libellé de la commune`) %>%
  rename(code_tvs = `Code du territoire de vie-santé`, libelle_tvs = `Libellé du territoire de vie-santé`) %>%
  filter(!is.na(code_tvs)) #on ne perd aucune commune car les 3 seules observations avec valeurs manquantes sont Paris, Lyon et Marseille car leur TVS est au niveau de l'arrondissement donc c'est le code commune arrondissement qui est matché

write_csv(communes_tvs, "~/ENSAE/3A/Projet_DSSS/Données/communes_tvs.csv")
communes_tvs <- read_csv("~/ENSAE/3A/Projet_DSSS/Données/communes_tvs.csv")

rm(communes, tvs)


#### Base des décès ####

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
# Pour les années inconnues: on enlève ces observations

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
      deces_annee = nettoyer_partie_date(deces_date, 1, 4)) %>%
    filter(!is.na(naissance_annee) & !is.na(deces_annee)) %>% #on enlève les décès où l'année de naissance ou l'année de décès est manquante
    mutate(
      naissance_mois = nettoyer_partie_date(naissance_date, 5, 6),
      naissance_mois_complete = complete_manquant(naissance_mois), 
      naissance_jour = nettoyer_partie_date(naissance_date, 7, 8),
      naissance_jour_complete = complete_manquant(naissance_jour),
      naissance_date = as.Date(naissance_date, '%Y%m%d'),
      naissance_date_complete = as.Date(paste0(naissance_annee, '-', naissance_mois_complete, '-', naissance_jour_complete)),
      deces_mois = nettoyer_partie_date(deces_date, 5, 6),
      deces_mois_complete = complete_manquant(deces_mois), 
      deces_jour = nettoyer_partie_date(deces_date, 7, 8),
      deces_jour_complete = complete_manquant(deces_jour), 
      deces_date = as.Date(deces_date, '%Y%m%d'),
      deces_date_complete = as.Date(paste0(deces_annee, '-', deces_mois_complete, '-', deces_jour_complete))) %>%
    distinct(nom, sexe, naissance_date_complete, naissance_code_lieu, deces_date_complete, deces_code_lieu, .keep_all = T) %>%
    filter(!is.na(deces_code_lieu) & !grepl("^9[7-9]", deces_code_lieu)) %>% #on enlève les décès où le lieu est non renseigné ou à l'étranger
    mutate(age = round(as.numeric((deces_date_complete - naissance_date_complete)/365),1),
           annee_deces = year(deces_date_complete)) %>%
    select(nom, sexe, naissance_code_lieu, deces_date_complete, deces_code_lieu, age, annee_deces)
  clean_df
}

#On découpe en 2 la liste de fichiers car c'est trop lourd d'importer tout en même temps
files_list <- list.files(sprintf('%s', mortalite_path), pattern = 'deces-[0-9]+.txt', full.names = T)
files_list_04_11 <- files_list[1:8]
files_list_12_19 <- files_list[9:16]

tic()
deces_04_11 <- lapply(files_list_04_11, read_fwf,
                      col_positions = fwf_widths(positions, col_names = names(positions)),
                      col_types = cols(
                        .default = col_character())
)

deces_04_11 <- lapply(deces_04_11, clean_date)


deces_04_11 <- bind_rows(deces_04_11) %>% distinct(.keep_all = T)
setDT(deces_04_11)
toc()


tic()
deces_12_19 <- lapply(files_list_12_19, read_fwf,
                      col_positions = fwf_widths(positions, col_names = names(positions)),
                      col_types = cols(
                        .default = col_character())
)

deces_12_19 <- lapply(deces_12_19, clean_date)

deces_12_19 <- bind_rows(deces_12_19) %>% distinct(.keep_all = T)
setDT(deces_12_19)
toc()


deces_04_19 <- rbindlist(list(deces_04_11, deces_12_19))

rm(deces_04_11, deces_12_19) ; gc()

deces_04_19 <- unique(deces_04_19) 

deces_04_19 <- deces_04_19[annee_deces >= 2004 & annee_deces <= 2019] #8827409 décès observés entre 2004 et 2019

communes_tvs_restreint <- communes_tvs %>% select(com, code_tvs)

communes_manquantes <- deces_04_19[, .(nb_morts = .N), by = "deces_code_lieu"] %>% as_tibble() %>% filter(!(deces_code_lieu %in% communes_tvs$com))
#il y a 88 communes non matchées à un TVS (car la commune a changé de nom ou a fusionné avant 2019)

deces_04_19 <- merge(deces_04_19, communes_tvs_restreint, all.x = TRUE, by.x = "deces_code_lieu", by.y = "com")

#on remplit "à la main" les TVS des 39 communes qui ont plus de 5 décès sur la période et qui ne sont pas matchées à un TVS 
#pour cela, on cherche sur le site de l'INSEE l'histoire des modifications de la commune (Ex: pour la commune 50071, on trouve sur
#https://www.insee.fr/fr/metadonnees/cog/commune/COM50071-braffais le code de la nouvelle commune, qu'on peut ensuite matcher
#avec son TVS correspondant grâce à notre base "communes_tvs")
com_modifiees <- communes_manquantes %>% filter(nb_morts>5) %>% pull(deces_code_lieu) 
deces_04_19 <- deces_04_19 %>% as_tibble() %>% 
  mutate(code_tvs = case_when(
    deces_code_lieu == "50071" ~ "50237",
    deces_code_lieu == "07252" ~ "07204",
    deces_code_lieu == "25319" ~ "25334",
    deces_code_lieu == "28297" ~ "28422",
    deces_code_lieu == "14307" ~ "14689",
    deces_code_lieu == "79011" ~ "79083",
    deces_code_lieu == "81155" ~ "81257",
    deces_code_lieu == "50206" ~ "50074",
    deces_code_lieu == "25593" ~ "25212",
    deces_code_lieu == "79045" ~ "79083",
    deces_code_lieu == "81107" ~ "81219",
    deces_code_lieu == "25027" ~ "25056",
    deces_code_lieu == "17238" ~ "17197",
    deces_code_lieu == "05043" ~ "05132",
    deces_code_lieu == "33338" ~ "33240",
    deces_code_lieu == "71265" ~ "71076",
    deces_code_lieu == "33092" ~ "33081",
    deces_code_lieu == "28042" ~ "28015",
    deces_code_lieu == "61147" ~ "28280",
    deces_code_lieu == "14604" ~ "14225",
    deces_code_lieu == "72033" ~ "72089",
    deces_code_lieu == "50406" ~ "50237",
    deces_code_lieu == "27024" ~ "27198",
    deces_code_lieu == "24430" ~ "24256",
    deces_code_lieu == "14185" ~ "14514",
    deces_code_lieu == "28066" ~ "28051",
    deces_code_lieu == "79211" ~ "79083",
    deces_code_lieu == "38312" ~ "38565",
    deces_code_lieu == "74274" ~ "74225",
    deces_code_lieu == "61042" ~ "28280",
    deces_code_lieu == "49144" ~ "44180",
    deces_code_lieu == "25530" ~ "25315",
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
  )) %>% filter(!is.na(code_tvs))

deces_com <- deces_04_19 %>% group_by(deces_code_lieu) %>% summarise(nb_tot_morts = n()) %>%
  left_join(communes_tvs, by = c("deces_code_lieu" = "com")) %>% select(deces_code_lieu, libelle_commune, nb_tot_morts)

tvs <- communes_tvs %>% distinct(code_tvs, libelle_tvs)
deces_tvs <- deces_04_19 %>% group_by(code_tvs) %>% summarise(nb_tot_morts = n()) %>%
  left_join(tvs, by = "code_tvs")

deces_04_19 %<>% mutate(mort_jeune = if_else(age<60,1,0), mort_vieux = if_else(age>80,1,0))

deces_04_19_tvs <- deces_04_19 %>% group_by(code_tvs, annee_deces) %>%
  summarise(nb_morts = n(), nb_morts_jeunes = sum(mort_jeune), nb_morts_vieux = sum(mort_vieux),
            age_moyen_deces = mean(age)) %>%
  ungroup() %>%
  rename(annee = annee_deces)

deces_04_19_tvs %<>% left_join(tvs, by = "code_tvs")

write_csv(deces_04_19_tvs, "~/ENSAE/3A/Projet_DSSS/Données/deces_04_19_tvs.csv")


n_distinct(communes_tvs_restreint$code_tvs) #2841
n_distinct(deces_04_19_tvs$code_tvs) #2727



#### Covariables socio-démographiques issues du recensement ####

col_names_99_12 <- c("code_tvs", "libelle_tvs", "F15_29", "pop_tot", "plus75",
                     "60_74", "45_59", "30_44", "15_29", "Fpop_tot", "Fplus75", "F60_74", "F45_59",
                     "F30_44")

col_names_08_17 <- c("code_tvs", "libelle_tvs", "F15_29", "CSretraites", "pop_tot", 'CSagriculteurs', "plus75",
                     "60_74", "45_59", "30_44", "15_29", "Fpop_tot", "Fplus75", "F60_74", "F45_59",
                     "F30_44", "CSartisans_commercants", "CSouvriers", "CSemployes", "CScadres", "CSprof_interm")

col_names_18 <- c("code_tvs", "libelle_tvs", "pop_tot", "CSretraites", "plus75", "60_74", "45_59", "30_44",
                  "15_29", "Fpop_tot", "Fplus75", "F60_74", "F45_59", "F30_44", "F15_29", "CSagriculteurs",
                  "CSartisans_commercants", "CSouvriers", "CSemployes", "CScadres", "CSprof_interm")

files_list <- list.files(sprintf('%s/population/', covariable_path), pattern = 'pop_tvs_[0-9]+.xlsx', full.names = T)

import_pop <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee1 <- str_extract(file1, "[0-9]+")
  if(substr(annee1,1,1)=="9"){annee <- 1999}
  else{annee <- as.numeric(sprintf("20%s",annee1))}
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
pop <- bind_rows(pop) %>% arrange(code_tvs, annee) %>% relocate(code_tvs, libelle_tvs, annee, pop_tot)

pop %<>% group_by(code_tvs, libelle_tvs) %>%
  group_modify(~add_row(.x, annee = c(2004,2005), .before = 2)) %>%
  group_modify(~add_row(.x, annee = 2019)) %>%
  ungroup() %>%
  filter(!grepl("^9[7-9]", code_tvs))



## Imputation: on suppose une croissance constante durant les périodes sans observations ##

#imputation variables CS (années manquantes : 2004, 2005, 2006, 2007, 2011, 2012, 2019)

#on commence par séparer les TVS qui ont 0% de certaines CS sur toute la période de ceux qui en ont 0% juste à quelques périodes
#(pour ceux-là, on remplace le 0% par 0.1% pour éviter de diviser par 0 lors de l'imputation)
pop %<>% group_by(code_tvs) %>% mutate(max_agr = max(CSagriculteurs, na.rm = T),
                                       max_art = max(CSartisans_commercants, na.rm = T),
                                       max_cadr = max(CScadres,na.rm = T),
                                       max_empl = max(CSemployes, na.rm = T),
                                       max_ouvr = max(CSouvriers, na.rm = T),
                                       max_int = max(CSprof_interm, na.rm = T),
                                       max_ret = max(CSretraites, na.rm = T)) %>%
  ungroup()

#Seule la CS agriculteurs est concernée par une proportion nulle durant toute la période pour certains TVS
#Pour ces TVS, on laisse donc cette proportion à 0
pop %<>% mutate(across(starts_with("CS"), ~if_else(.==0,0.1,.))) %>%
  mutate(CSagriculteurs = if_else(max_agr==0,0,CSagriculteurs)) %>%
  select(-c(max_agr, max_art, max_cadr, max_empl, max_ouvr, max_int, max_ret))


#imputation
pop %<>% mutate(across(starts_with("CS"), ~case_when(annee == 2004 ~ lead(., 4)*(lead(.,4)/lead(.,5))^4,
                                                     annee == 2005 ~ lead(., 3)*(lead(.,3)/lead(.,4))^3,
                                                     annee == 2006 ~ lead(., 2)*(lead(.,2)/lead(.,3))^2, 
                                                     annee == 2007 ~ lead(., 1)*(lead(.,1)/lead(.,2)),
                                                     annee == 2011 ~ lag(., 1)*(lead(.,2)/lag(.,1))^(1/3),
                                                     annee == 2012 ~ lag(., 2)*(lead(.,1)/lag(.,2))^(2/3),
                                                     annee == 2019 ~ lag(., 1)*(lag(.,1)/lag(.,2)),
                                                     TRUE ~ .)))

#imputation variables démographiques (années manquantes : 2004, 2005, 2019)

#pour certains TVS (Lyon et Marseille) il y a des valeurs manquantes sur d'autres années que 2004, 2005 et 2019
pop_manquant <- pop %>% filter(!(annee %in% c(2004,2005,2019)) & is.na(pop_tot))

#On commence par imputer pour Lyon et Marseille les valeurs manquantes pour 2004, 2005, 2006, 2009 et 2011 (on le fait spécifiquement pour ces 2 TVS car par ex on a pas de valeurs renseignées en 1999 donc on utilise une méthode moins sophistiquée)
lyon_mars <- unique(pop_manquant$code_tvs)

pop %<>% mutate(across(c(4:15), ~case_when((annee == 2004) & (code_tvs %in% lyon_mars) ~ lead(., 3)*(lead(.,3)/lead(.,4))^3,
                                           (annee == 2005) & (code_tvs %in% lyon_mars) ~ lead(., 2)*(lead(.,2)/lead(.,3))^2,
                                           (annee == 2006) & (code_tvs %in% lyon_mars) ~ lead(., 1)*(lead(.,1)/lead(.,2)),
                                           (annee == 2009) & (code_tvs %in% lyon_mars) ~ lag(., 1)*(lead(.,1)/lag(.,1))^(1/2),
                                           (annee == 2011) & (code_tvs %in% lyon_mars) ~ lag(., 1)*(lead(.,1)/lag(.,1))^(1/2),
                                           TRUE ~ .)))

#On impute maintenant pour le reste des TVS les valeurs manquantes pour 2004 et 2005
pop %<>% mutate(across(c(4:15), ~case_when((annee == 2004) & !(code_tvs %in% lyon_mars) ~ lag(., 1)*(lead(.,2)/lag(.,1))^(5/7),
                                           (annee == 2005) & !(code_tvs %in% lyon_mars) ~ lag(., 2)*(lead(.,1)/lag(.,2))^(6/7),
                                           TRUE ~ .)))

#On impute enfin pour tous les TVS la valeur manquante pour 2019
pop %<>% mutate(across(c(4:15), ~case_when(annee == 2019 ~ lag(., 1)*(lag(.,1)/lag(.,2)),
                                           TRUE ~ .)))


pop %<>% filter(annee != 1999) %>%
  mutate(across(starts_with("CS"), ~replace_na(.,0))) #les NAN créés par la division par 0 sont bien des 0 car le max de la CS sur la période est 0



n_distinct(pop$code_tvs) #2735 tvs

deces_04_19_tvs %>% filter(!(code_tvs %in% unique(pop$code_tvs))) %>% view()
#il y a 3 TVS qui ont des noms différents entre les 2 bases: on harmonise donc "à la main" les codes TVS
pop %<>% mutate(code_tvs = case_when(
  code_tvs == "22007" ~ "22055",
  code_tvs == "35129" ~ "35176",
  code_tvs == "49313" ~ "49218",
  TRUE ~ code_tvs))

pop %>% filter(!(code_tvs %in% unique(deces_04_19_tvs$code_tvs))) %>% view()
#il y a 8 TVS présents dans la base pop mais pas dans notre table de passage communes/TVS
#ces 8 TVS sont en fait des parties des TVS de notre table de passage: il est donc important de les rattacher
#à leur TVS principal car sinon le nombre de décès et le nombre d'habitants ne couvriraient pas la même échelle
#(on surestimerait alors le nombre de décès par habitant dans ces TVS)

pop %<>% mutate(code_tvs = case_when(
  code_tvs == "49042" ~ "49021",
  code_tvs %in% c("50173", "50203", "50602") ~ "50129",
  code_tvs == "57537" ~ "57227",
  code_tvs == "68118" ~ "68224",
  code_tvs %in% c("85060", "85166") ~ "85194",
  TRUE ~ code_tvs
),
libelle_tvs = case_when(
  code_tvs == "49021" ~ "Beaufort-en-Anjou",
  code_tvs == "50129" ~ "Cherbourg-en-Cotentin",
  code_tvs == "57227" ~ "Forbach",
  code_tvs == "68224" ~ "Mulhouse",
  code_tvs == "85194" ~ "Les Sables-d'Olonne",
  TRUE ~ libelle_tvs
))



#on agrège ensuite les variables démo
pop %<>% group_by(code_tvs, annee) %>% mutate(pop_tot1 = sum(pop_tot)) %>%
  mutate(poids = pop_tot/pop_tot1) %>%
  mutate(across(starts_with("CS"), ~weighted.mean(., poids))) %>%
  mutate(across(c(2:13), ~sum(.))) %>%
  ungroup() %>%
  select(-c(poids, pop_tot1)) %>%
  distinct(code_tvs, annee, .keep_all = T)


#on garde les variables démo en proportion
pop %<>% mutate(across(c(5:15), ~.*100/pop_tot))

write_csv(pop, "~/ENSAE/3A/Projet_DSSS/Données/pop_04_19_tvs.csv")

ggplot(pop %>% filter(annee==2006), aes(x = pop_tot)) +
  geom_density()

quantile(pop %>% filter(annee==2006) %>% pull(pop_tot), probs = c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.99,1))




#### Covariables issues de Filosofi (et RFL avant 2012) ####

files_list <- list.files(sprintf('%s/Filosofi/Tables_interet/', covariable_path), full.names = T)


import_filo <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee <- as.numeric(str_extract(file1, "[0-9]+"))
  if(annee %in% c(2004:2011)){
    data <- read.csv2(file) %>%
      select(COM | starts_with("NBMEN") | starts_with("RFUCQ2")) %>%
      mutate(across(starts_with("RFUCQ2"), ~as.numeric(as.character(.))))
  }
  else if(annee %in% c(2012:2014)){
    data <- read_dta(file) %>%
      filter(niv == "COM") %>%
      select(codgeo | starts_with("nbmen") | starts_with("q2"))
  }
  else if(annee %in% c(2015:2018)){
    data <- read_excel(file, sheet = "COM", skip = 5) %>%
      select(CODGEO | starts_with("NBMENFISC") | starts_with("MED"))
  }
  else{
    data <- read_excel(file, sheet = "ENSEMBLE", skip = 5) %>%
      select(CODGEO | starts_with("NBMEN") | starts_with("Q2"))
  }
  
  data %<>% rename(com = 1, nb_menages = 2, med_niveau_vie = 3) %>%
    mutate(annee = !!annee) %>% relocate(com, annee,)
}

filo <- lapply(files_list, import_filo)
filo <- bind_rows(filo) %>% arrange(com, annee)

A <- filo %>% filter(!(com %in% communes_tvs$com) & !(com %in% com_modifiees)) %>% distinct(com, .keep_all = T)
#seulement 16 communes ont des données sur les revenus et ne sont pas matchées à un TVS

filo %<>% left_join(communes_tvs_restreint, by = "com") %>%
  mutate(code_tvs = case_when(
    com == "50071" ~ "50237",
    com == "07252" ~ "07204",
    com == "25319" ~ "25334",
    com == "28297" ~ "28422",
    com == "14307" ~ "14689",
    com == "79011" ~ "79083",
    com == "81155" ~ "81257",
    com == "50206" ~ "50074",
    com == "25593" ~ "25212",
    com == "79045" ~ "79083",
    com == "81107" ~ "81219",
    com == "25027" ~ "25056",
    com == "17238" ~ "17197",
    com == "05043" ~ "05132",
    com == "33338" ~ "33240",
    com == "71265" ~ "71076",
    com == "33092" ~ "33081",
    com == "28042" ~ "28015",
    com == "61147" ~ "28280",
    com == "14604" ~ "14225",
    com == "72033" ~ "72089",
    com == "50406" ~ "50237",
    com == "27024" ~ "27198",
    com == "24430" ~ "24256",
    com == "14185" ~ "14514",
    com == "28066" ~ "28051",
    com == "79211" ~ "79083",
    com == "38312" ~ "38565",
    com == "74274" ~ "74225",
    com == "61042" ~ "28280",
    com == "49144" ~ "44180",
    com == "25530" ~ "25315",
    com == "74110" ~ "74224",
    com == "27293" ~ "27198",
    com == "27166" ~ "27198",
    com == "14513" ~ "50601",
    com == "14697" ~ "14654",
    com == "30190" ~ "30350",
    com == "44160" ~ "44003",
    com == "44060" ~ "49244",
    com == "74181" ~ "74112",
    TRUE ~ code_tvs
  )) %>% filter(!is.na(code_tvs) & !grepl("^9[7-9]", code_tvs))


filo %<>% group_by(code_tvs, annee) %>%
  mutate(nb_menages_tot = sum(nb_menages, na.rm = T)) %>%
  mutate(poids = nb_menages/nb_menages_tot)

filo %<>% group_by(code_tvs, annee, nb_menages_tot) %>%
  summarise(med_niveau_vie = weighted.mean(med_niveau_vie, poids, na.rm = T)) %>%
  filter(!is.na(med_niveau_vie)) %>%
  ungroup()

write_csv(filo, "~/ENSAE/3A/Projet_DSSS/Données/filo_04_19_tvs.csv")



#### Covariables issues de la BPE (et répertoire FINESS avant 2007) ####

## BPE (2007-2019): on s'intéresse à: ##

#D101 : établissement santé court séjour
#D102 : établissement santé court séjour
#D103 : établissement santé court séjour
#D104 : établissement psychiatrique
#D105 : centre lutte cancer
#D106 : service d'urgences
#D401 : établissement personnes âgées hébergement
#D232 : infirmiers

files_list1 <- list.files(sprintf('%s/BPE/', covariable_path), pattern = 'bpe[0-9]+_ensemble.csv', full.names = T)
files_list2 <- list.files(sprintf('%s/BPE/', covariable_path), pattern = 'bpe[0-9]+_ensemble_com.csv', full.names = T)

import_bpe1 <- function(file){
  data <- read_csv2(file) %>% filter(TYPEQU %in% c("D101", "D102", "D103", "D104", "D105", "D106", "D401", "D232")) %>%
    group_by(DEPCOM, AN, TYPEQU) %>% summarise(nb = sum(NB_EQUIP)) %>%
    ungroup %>%
    pivot_wider(names_from = TYPEQU, values_from = nb) %>%
    rename(CODGEO = DEPCOM, NB_D101 = D101, NB_D102 = D102, NB_D103 = D103, NB_D104 = D104, NB_D105 = D105,  NB_D106 = D106, NB_D401 = D401, NB_D232 = D232, annee = AN) %>%
    mutate(across(starts_with("NB"), ~replace_na(.,0)))
}

import_bpe2 <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee <- as.numeric(sprintf("20%s", str_extract(file1, "[0-9]+")))
  data <- read_csv2(file) %>% select(CODGEO, NB_D101, NB_D102, NB_D103, NB_D104, NB_D105, NB_D106, NB_D401, NB_D232) %>%
    mutate(annee = !!annee)
}

bpe1 <- lapply(files_list1, import_bpe1)
bpe2 <- lapply(files_list2, import_bpe2)
bpe <- bind_rows(bpe1, bpe2) %>% rename(com = CODGEO)

A <- bpe %>% filter(!(com %chin% communes_tvs$com) & (NB_D101 > 0 | NB_D106 > 0 | NB_D401 > 0)) %>% distinct(com) #6 communes

bpe %<>% left_join(communes_tvs_restreint, by = "com") %>%
  mutate(code_tvs = case_when(
    com == "50071" ~ "50237",
    com == "07252" ~ "07204",
    com == "25319" ~ "25334",
    com == "28297" ~ "28422",
    com == "14307" ~ "14689",
    com == "79011" ~ "79083",
    com == "81155" ~ "81257",
    com == "50206" ~ "50074",
    com == "25593" ~ "25212",
    com == "79045" ~ "79083",
    com == "81107" ~ "81219",
    com == "25027" ~ "25056",
    com == "17238" ~ "17197",
    com == "05043" ~ "05132",
    com == "33338" ~ "33240",
    com == "71265" ~ "71076",
    com == "33092" ~ "33081",
    com == "28042" ~ "28015",
    com == "61147" ~ "28280",
    com == "14604" ~ "14225",
    com == "72033" ~ "72089",
    com == "50406" ~ "50237",
    com == "27024" ~ "27198",
    com == "24430" ~ "24256",
    com == "14185" ~ "14514",
    com == "28066" ~ "28051",
    com == "79211" ~ "79083",
    com == "38312" ~ "38565",
    com == "74274" ~ "74225",
    com == "61042" ~ "28280",
    com == "49144" ~ "44180",
    com == "25530" ~ "25315",
    com == "74110" ~ "74224",
    com == "27293" ~ "27198",
    com == "27166" ~ "27198",
    com == "14513" ~ "50601",
    com == "14697" ~ "14654",
    com == "30190" ~ "30350",
    com == "44160" ~ "44003",
    com == "44060" ~ "49244",
    com == "74181" ~ "74112",
    TRUE ~ code_tvs)) %>%
  filter(!is.na(code_tvs)) %>%
  group_by(code_tvs, annee) %>%
  summarise(NB_D101 = sum(NB_D101), NB_D102 = sum(NB_D102), NB_D103 = sum(NB_D103), NB_D104 = sum(NB_D104), NB_D105 = sum(NB_D105), NB_D106 = sum(NB_D106), NB_D401 = sum(NB_D401), NB_D232 = sum(NB_D232)) %>%
  ungroup()

n_distinct(bpe$code_tvs)
#2840

bpe %>% filter(NB_D101 == 0 & NB_D106 == 0 & NB_D401 == 0) %>% view()



## FINESS (2004-2006) ##

files_list <- list.files(sprintf('%s/FINESS/', covariable_path), pattern = '.csv$', full.names = T)

import_FINESS <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee <- as.numeric(substr(str_extract(file1, "[0-9]+"),1,4))
  data <- read.csv2(file) %>% select(cog, libcategetab) %>%
    mutate(
      D101 = if_else(libcategetab %in% c("Centre Hospitalier", "Hôpital Local", "Autre Etab. Loi Hosp",
                                         "C.H.R.", "Etab. Soins Chirur.", "Etab. Trans.Sanguine", "S.I.H.",
                                         "Hôpital armées"),1,0), #court séjour
      D102 = if_else(libcategetab %in% c("Etab.Conval.et Repos", "Etab. Réadap. Fonct.", "Maison de Régime",
                                         "Etab.Lutte Tubercul.", "Ctre PostCure Alcool"),1,0), #moyen séjour
      D103 = if_else(libcategetab %in% c("Etab.Soins Long.Dur."),1,0), #long séjour
      D104 = if_else(libcategetab %in% c("C. H. S. Mal. Mental", "Mais. Santé Mal.Ment", "Ctre P-Cure Mal.Men."),1,0), #psychiatrie
      D105 = if_else(libcategetab %in% c("Ctre Lutte C. Cancer"),1,0), #centre lutte contre cancer
      D106 = if_else(libcategetab %in% c("Centre Hospitalier", "Hôpital Local", "C.H.R."),1,0), #service d'urgences
      D401 = if_else(libcategetab %in% c("Maison de Retraite", "Res.Héb.Temp.P.A.", "Etb Expér.Pers.Agées",
                                         "Hospice", "autre res.But lucrat"), 1, 0)) %>% #hébergement personnes âgées %>%
    group_by(cog) %>%
    summarise(NB_D101 = sum(D101),
              NB_D102 = sum(D102),
              NB_D103 = sum(D103),
              NB_D104 = sum(D104),
              NB_D105 = sum(D105),
              NB_D106 = sum(D106),
              NB_D401 = sum(D401)) %>%
    filter(NB_D101 > 0 | NB_D102 > 0 | NB_D103 > 0 | NB_D104 > 0 | NB_D105 > 0 | NB_D106 > 0 | NB_D401 > 0) %>%
    mutate(annee = !!annee)
}

finess <- lapply(files_list, import_FINESS)
finess <- bind_rows(finess) %>% rename(com = cog)

finess %>% filter(!(com %in% communes_tvs$com)) %>% distinct(com, .keep_all = T) %>% view()

finess %<>% left_join(communes_tvs_restreint, by = "com") %>%
  mutate(code_tvs = case_when(com == "30190" ~ "30350",
                              com == "44060" ~ "49244",
                              com == "74181" ~ "74112",
                              TRUE ~ code_tvs)) %>%
  filter(!is.na(code_tvs)) %>%
  group_by(code_tvs, annee) %>%
  summarise(NB_D101 = sum(NB_D101), NB_D102 = sum(NB_D102), NB_D103 = sum(NB_D103), NB_D104 = sum(NB_D104), NB_D105 = sum(NB_D105), NB_D106 = sum(NB_D106), NB_D401 = sum(NB_D401)) %>%
  ungroup()
#2487 TVS  

#pour infirmiers 2006, on récupère directement sur CartoSanté
infirmiers <- read_csv2(sprintf("%s/nb_infirmiers_2006.csv", covariable_path), skip = 2) %>% rename(NB_D232 = 3, code_tvs = Code) %>%
  mutate(NB_D232 = as.numeric(NB_D232), annee = 2006) %>%
  select(-`Libellé`) %>%
  filter(!grepl("^9[7-9]", code_tvs) & !is.na(NB_D232))

infirmiers %<>% mutate(code_tvs = case_when(
  code_tvs == "22007" ~ "22055",
  code_tvs == "35129" ~ "35176",
  code_tvs == "49313" ~ "49218",
  TRUE ~ code_tvs))

infirmiers %<>% mutate(code_tvs = case_when(
  code_tvs == "49042" ~ "49021",
  code_tvs %in% c("50173", "50203", "50602") ~ "50129",
  code_tvs == "57537" ~ "57227",
  code_tvs == "68118" ~ "68224",
  code_tvs %in% c("85060", "85166") ~ "85194",
  TRUE ~ code_tvs
))

infirmiers %<>% group_by(code_tvs, annee) %>% summarise(NB_D232 = sum(NB_D232))

finess <- infirmiers %>% left_join(finess, by = c("code_tvs", "annee"))


bpe <- finess %>% bind_rows(bpe) %>% arrange(code_tvs, annee)

write_csv(bpe, "~/ENSAE/3A/Projet_DSSS/Données/bpe_tvs_06_19.csv")



#### Densité médicale ####

files_list <- list.files(sprintf('%s', densite_path), pattern = '[0-9]+_tvs.csv', full.names = T)

import_densite <- function(file){
  file1 <- str_replace(file, "3A", "")
  annee <- as.numeric(str_extract(file1, "[0-9]+"))
  data <- read_csv2(file, skip = 2) %>% rename(nb_medecins = 3, code_tvs = Code) %>%
    mutate(nb_medecins = as.numeric(nb_medecins), annee = !!annee) %>%
    select(-`Libellé`)
}

densite <- lapply(files_list, import_densite)
densite <- bind_rows(densite) %>% filter(!grepl("^9[7-9]", code_tvs))

deces_04_19_tvs %>% filter(!(code_tvs %in% densite$code_tvs))

densite %<>% mutate(code_tvs = case_when(
  code_tvs == "22007" ~ "22055",
  code_tvs == "35129" ~ "35176",
  code_tvs == "49313" ~ "49218",
  TRUE ~ code_tvs))


densite %>% filter(!(code_tvs %in% unique(deces_04_19_tvs$code_tvs))) %>% distinct(code_tvs, .keep_all = T) %>% view()
#il y a 8 TVS présents dans la base pop mais pas dans notre table de passage communes/TVS
#ces 8 TVS sont en fait des parties des TVS de notre table de passage: il est donc important de les rattacher
#à leur TVS principal car sinon le nombre de décès et le nombre d'habitants ne couvriraient pas la même échelle
#(on surestimerait alors le nombre de décès par habitant dans ces TVS)

densite %<>% mutate(code_tvs = case_when(
  code_tvs == "49042" ~ "49021",
  code_tvs %in% c("50173", "50203", "50602") ~ "50129",
  code_tvs == "57537" ~ "57227",
  code_tvs == "68118" ~ "68224",
  code_tvs %in% c("85060", "85166") ~ "85194",
  TRUE ~ code_tvs
))

#on agrège ensuite les variables démo
densite %<>% group_by(code_tvs, annee) %>% summarise(nb_medecins = sum(nb_medecins))
#2727 TVS

densite %>% filter(is.na(nb_medecins)) %>% view()

write_csv(densite, "~/ENSAE/3A/Projet_DSSS/Données/densite_tvs.csv")


data_tvs <- tibble(code_tvs = rep(unique(deces_04_19_tvs$code_tvs), 16),
                   annee = rep(c(2004:2019), each = n_distinct(deces_04_19_tvs$code_tvs)))


pop %<>% select(-libelle_tvs)

data_tvs %<>% left_join(pop) %>%
  left_join(filo_04_19_tvs) %>%
  left_join(bpe) %>%
  left_join(densite) %>%
  left_join(deces_04_19_tvs) %>%
  mutate(across(starts_with("NB_D"), ~replace_na(.,0))) %>%
  mutate(across(starts_with("nb_morts"), ~replace_na(.,0))) %>%
  arrange(code_tvs, annee) %>%
  relocate(code_tvs, libelle_tvs, annee)

write_csv(data_tvs, "~/ENSAE/3A/Projet_DSSS/Données/base_finale_tvs.csv")
data_tvs <- read_csv("~/ENSAE/3A/Projet_DSSS/Données/base_finale_tvs.csv")

data_tvs %<>% filter(annee >= 2006 & annee < 2019)

data_tvs %<>% mutate(densite = 1000*nb_medecins/pop_tot, log_pop = log(pop_tot),
                     log_morts = log(nb_morts),
                     nb_morts_hab = nb_morts*100/pop_tot,
                     #log_morts_jeunes = if_else(nb_morts_jeunes==0, 0, log(nb_morts_jeunes)),
                     #log_morts_vieux = if_else(nb_morts_vieux==0, 0, log(nb_morts_vieux)),
                     part_jeunes = nb_morts_jeunes*100/nb_morts,
                     part_vieux = nb_morts_vieux*100/nb_morts,
                     taille_menage = pop_tot/nb_menages_tot)

#création d'un panel équilibré avec les TVS où il y a au moins 5 morts/an (2660 TVS)
data_tvs1 <- data_tvs %>% filter(!is.na(nb_menages_tot) & nb_morts>=5) #arrondissements Paris, Lyon, Marseille (filosofi disponible que de 2012 à 2018) + Hoedic (valeurs manquantes de 2008 à 2010)
A <- data_tvs1 %>% group_by(code_tvs) %>% summarise(n = n()) %>% filter(n<13)
data_tvs1 %<>% filter(!(code_tvs %in% A$code_tvs))

data_tvs1 %<>% mutate(densite_infirmiers = NB_D232*1000/pop_tot)

write_csv(data_tvs1, "~/ENSAE/3A/Projet_DSSS/Données/base_finale_tvs_balanced.csv")


