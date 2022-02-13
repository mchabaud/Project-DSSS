#répertoires
mortalite_path <- "~/ENSAE/3A/Projet_DSSS/Données/Mortalité"

data <- read_csv2(file.choose())
data <- read_delim(file.choose(), delim = "|", 
                   col_types = cols(.default = col_character()), n_max = 4398)
data <- read_delim(file.choose(), delim = "|", 
                   col_types = cols(.default = col_character()), skip = 4399, col_names = FALSE)


data <- read_csv2(file.choose())

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

A <- deces_15_19 %>% group_by(deces_code_lieu) %>% summarise(n = n())

any(is.na(deces_15_19$naissance_date_complete))
any(is.na(deces_15_19$deces_date_complete))


#A <- deces8 %>% filter(grepl("MOURLON",nom))




#base equipements

data <- read_csv2(file.choose())
A <- data %>% distinct(UU2020, .keep_all = T)
A <- data %>% filter(TYPEQU == "D201")
