data <- read_csv2(file.choose())
data <- read_delim(file.choose(), delim = "|", 
                   col_types = cols(.default = col_character()), n_max = 4398)
data <- read_delim(file.choose(), delim = "|", 
                   col_types = cols(.default = col_character()), skip = 4399, col_names = FALSE)


data <- read_csv2(file.choose())

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


deces8 <- read_fwf(file = file.choose(), col_positions = fwf_widths(positions, col_names = names(positions)), col_types = cols(.default = col_character()))
A <- deces8 %>% filter(grepl("MOURLON",nom))


#base equipements

data <- read_csv2(file.choose())
A <- data %>% distinct(UU2020, .keep_all = T)
A <- data %>% filter(TYPEQU == "D201")
