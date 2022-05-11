#### Stats desc ####

data_tvs1 <- read_csv("~/ENSAE/3A/Projet_DSSS/Données/base_finale_tvs_balanced.csv")

nb_tvs <- n_distinct(data_tvs1$code_tvs)
n <- nrow(data_tvs1)

var_decompo <- function(data, var){
  df <- data %>% mutate(moy = mean(!!sym(var)))
  df %<>% group_by(code_tvs) %>% mutate(moy_tvs = mean(!!sym(var))) %>%
    mutate(ecart_with = !!sym(var) - moy_tvs) %>%
    ungroup()
  overall_var <- var(df[[var]])
  between <- df %>% distinct(code_tvs, .keep_all = T) %>%
    mutate(ecart_betw = moy_tvs - moy)
  between_var <- sum((between$ecart_betw)^2)/nb_tvs
  within_var <- sum((df$ecart_with)^2)/(n-1)
  var <- data.frame(Variable = var,
                    Overall = overall_var,
                    Between = between_var,
                    Within = within_var) %>% mutate(Part_within = Within*100/Overall)
  return(var)
}

stats_desc <- function(var){
  moy <- mean(data_tvs1[[var]])
  min <- data_tvs1 %>% group_by(code_tvs, libelle_tvs) %>% summarise(moy = mean(!!sym(var))) %>%
    arrange(moy) %>% ungroup() %>% slice(1)
  max <- data_tvs1 %>% group_by(code_tvs, libelle_tvs) %>% summarise(moy = mean(!!sym(var))) %>%
    arrange(desc(moy)) %>% ungroup() %>% slice(1)
  moy06 <- data_tvs1 %>% filter(annee == 2006) %>% pull(var) %>% mean()
  moy18 <- data_tvs1 %>% filter(annee == 2018) %>% pull(var) %>% mean()
  diff_18_06 <- moy18 - moy06
  std <- sd(data_tvs1[[var]])
  variance <- var_decompo(data_tvs1, var)
  print(sprintf("Moyenne %s : %s", var, moy))
  print(sprintf("Min %s : %s (%s)", var, min$moy, min$libelle_tvs))
  print(sprintf("Max %s : %s (%s)", var, max$moy, max$libelle_tvs))
  print(sprintf("2018-2006 %s : %s", var, diff_18_06))
  print(sprintf("Écart-type %s : %s", var, std))
  print(sprintf("Décomposition variance %s :", var))
  variance
}

stats_desc("pop_tot")
stats_desc("age_moyen_deces")
stats_desc("nb_morts_hab")
stats_desc("part_jeunes")
stats_desc("part_vieux")
stats_desc("densite")


#### Cartes ####


library(ggplot2)
library(ggiraph)
library(plotly)
library(readxl)
library(rlang)
library(dplyr)
library(haven)
library(tidyr)
library(ggplot2)
library(grid)
library(skimr)
library(tidyverse)
library(RColorBrewer)
library(sf)
library(sqldf)
library(viridis)
library(ggnewscale)
library(colorspace)
library(ggspatial)

## Données ##

data_map_tmp <- read_csv("base_finale_tvs_balanced.csv")
fra <- readRDS("gadm36_FRA_2_sf.rds") %>% select(c(7, 12, 14))
t_passage <- read_csv("communes_tvs.csv") %>%
  group_by(dep, code_tvs) %>%
  slice(1)

## Carte 1 - AGE MOYEN DE DECES ##
data_map <-  data_map_tmp %>%
  mutate(dep = substring(code_tvs, 1, 2)) %>%
  group_by(code_tvs, libelle_tvs,  annee) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(c(1, 3:4, 36, 44)) %>%
  filter(annee%in%c(2006, 2018)) %>%
  group_by(dep, annee)%>%
  mutate(pop_tot2 = sum(pop_tot), poid = pop_tot/pop_tot2) %>%
  summarise(age_moyen_deces_mean = weighted.mean(age_moyen_deces, 
                                                 poid, na.rm = TRUE)) %>%
  pivot_wider(names_from = annee, values_from = age_moyen_deces_mean) %>%
  mutate(variation_age_moyen_deces = `2018` - `2006`) %>%
  left_join(fra, by = c("dep" = "CC_2")) %>%
  mutate_at(c(2:3), round, 2) %>%
  mutate("Département, âge moyen de décès en 2006 et 2018" = 
           paste0(NAME_2, ", ", `2006`, " en 2006 et ", `2018`, " en 2018.")) %>%
  mutate(Variation = as.factor(case_when(variation_age_moyen_deces <= 2.8 ~ "]+1.7, +2.8]",
                                         variation_age_moyen_deces <=  3.2~ "]+2.8, +3.2]",
                                         variation_age_moyen_deces <= 3.5 ~ "]+3.2, +3.5]",
                                         variation_age_moyen_deces <= 3.8 ~ "]+3.5, +3.8]",
                                         TRUE ~ "]+3.8, +5.4]")))
data_map$Variation <- factor(data_map$Variation, levels = c("]+3.8, +5.4]", "]+3.5, +3.8]",
                                                            "]+3.2, +3.5]", "]+2.8, +3.2]",
                                                            "]+1.7, +2.8]"))

quantile(data_map$variation_age_moyen_deces, c(.2, .4, .6, .8)) # pour définir les catégories

# Création de la carte
fra_map <- ggplot() +
  geom_sf(data = data_map, aes(geometry = geometry, fill = Variation),
          colour = "#636363", lwd = 0.05) +
  scale_fill_manual(values = c("#ca0020", "#f4a582", "#fddbc7",
                               "#92c5de", "#0571b0"),
                    name= "Nombre d'années") +
  coord_sf(datum = NA, expand = FALSE) +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15))


# Exporter la carte 
ggsave(fra_map,
       filename = "map_variation_age_moyen_deces.png",
       width = 10,
       height = 10)


## Carte 2 - DENSITE DE MEDECINS ##

data_map_2 <-  data_map_tmp %>%
  mutate(dep = substring(code_tvs, 1, 2)) %>%
  filter(annee%in%c(2006,2018)) %>%
  group_by(code_tvs, libelle_tvs,  annee) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(c(3:4, 32, 44)) %>%
  group_by(dep, annee) %>% 
  summarise(across(c(1:2), sum, na.rm=TRUE)) %>%
  mutate(densite = nb_medecins/pop_tot * 1000) %>%
  ungroup() %>%
  select(c(1,2,5))%>%
  pivot_wider(names_from = annee, values_from = densite) %>%
  mutate(variation_densite = `2018` - `2006`) %>%
  left_join(fra, by = c("dep" = "CC_2")) %>%
  mutate_at(c(2:3), round, 2) %>%
  mutate("Département, nombre de médecins pour 1000 habs. en 2006 et 2018" = 
           paste0(NAME_2, ", ", `2006`, " en 2006 et ", `2018`, " en 2018.")) %>%
  mutate(Variation = as.factor(case_when(variation_densite <= -0.17 ~ "]-0.27, -0.17]",
                                         variation_densite <= -0.12 ~ "]-0.17, -0.12]",
                                         variation_densite <= -0.09 ~ "]-0.12, -0.09]",
                                         variation_densite <= -0.06 ~ "]-0.09, -0.06]",
                                         TRUE ~ "]-0.06, +0.06]")))
data_map_2$Variation <- factor(data_map_2$Variation, levels = c("]-0.06, +0.06]", "]-0.09, -0.06]",
                                                                "]-0.12, -0.09]", "]-0.17, -0.12]",
                                                                "]-0.27, -0.17]"))

quantile(data_map_2$variation_densite, c(.2, .4, .6, .995)) # pour définir les catégories

# Création de la carte
fra_map_2 <- ggplot() +
  geom_sf(data = data_map_2, aes(geometry = geometry, fill = Variation),
          colour = "#636363", lwd = 0.05) +
  scale_fill_manual(values = c("#ca0020", "#f4a582", "#fddbc7",
                               "#92c5de", "#0571b0"),
                    name= "Nombre de médecins \npour 1000 habitants") +
  coord_sf(datum = NA, expand = FALSE) +
  # ggtitle("Variation de la densité de médecins entre 2006 et 2018")+
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15))

# Exporter la carte 
ggsave(fra_map_2,
       filename = "map_variation_densite_med_g.png",
       width = 10,
       height = 10)