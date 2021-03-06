
data_tvs1 <- read_csv("~/ENSAE/3A/Projet_DSSS/Donn�es/base_finale_tvs_balanced.csv")

#### H�t�rog�n�it� selon la densit� m�dicale (focus sur les TVS sous-denses) ####


#d�finition du seuil de sous-dotation (6% de la population le moins dot�): on consid�re tous les TVS ayant connu au moins une ann�e en sous-dotation
#2018
data18 <- data_tvs1 %>% filter(annee == 2018) %>%
  arrange(densite) %>% mutate(pop_cumulee = cumsum(pop_tot))
pop18 <- sum(data18$pop_tot)
perc6 <- 0.06*pop18
data_sous_denses <- data18 %>% filter(pop_cumulee <= perc6 + 20000)
#seuil = 0.46 m�decins / 1000hab

A <- data_tvs1 %>% filter(densite < 0.5)

A1 <- data_tvs1 %>% filter(code_tvs %in% A$code_tvs)

fixed <- feols(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A1)
summary(fixed)

fixed <- feols(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A1)
summary(fixed)




#on consid�re tous les TVS qui sont en moyenne dans les 20% les moins dot�s

data_moy <- data_tvs1 %>% group_by(code_tvs) %>% summarise(moy_densite = mean(densite))
quantile(data_moy$moy_densite, probs = c(0, 0.1, 0.2, 0.5, 0.75, 0.9, 1))
#20% : 0.66
data_moy %<>% filter(moy_densite <= quantile(data_moy$moy_densite, probs = 0.2))
A2 <- data_tvs1 %>% filter(code_tvs %in% data_moy$code_tvs)

fixed <- feols(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A2)
summary(fixed)

fixed <- feols(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A2)
summary(fixed)


#On consid�re les TVS avec la plus forte variance de densit�

data_var <- data_tvs1 %>% group_by(code_tvs) %>% summarise(var_densite = var(densite))
quantile(data_var$var_densite, probs = c(0, 0.1, 0.2, 0.5, 0.8, 0.9, 1))
#80% : 0.0153
data_var %<>% filter(var_densite >= quantile(data_var$var_densite, probs = 0.8))
A3 <- data_tvs1 %>% filter(code_tvs %in% data_var$code_tvs)

fixed <- feols(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A3)
summary(fixed)

fixed <- feols(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A3)
summary(fixed)


#On consid�re une indicatrice de sous-densit�

A4 <- data_tvs1 %>% group_by(code_tvs) %>% mutate(min = min(densite), max = max(densite)) %>%
  filter(min < 0.5 & max >= 0.5) %>%
  ungroup() %>%
  mutate(desert_med = if_else(densite < 0.5, 1, 0))

fixed <- feols(age_moyen_deces ~ desert_med + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A4)
summary(fixed)

fixed <- feols(log_morts ~ desert_med + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A4)
summary(fixed)




#### Effets retard�s et agr�gation des p�riodes ####


#Avec un lag de 1 
data_tvs %<>% group_by(code_tvs) %>% mutate(densite_4 = lag(densite, 4))
lag_1 <- plm(age_moyen_deces ~ densite_4 + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
               NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
               CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee) - 1,
             data = data_tvs, index = c("code_tvs","annee"), model = "within")
summary(lag_1)
lag_2 <- plm(log_morts ~ densite_1 + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
               NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
               CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee) - 1,
             data = data_tvs, index = c("code_tvs","annee"), model = "within")
#Avec un pas de 3 ans
table(data_tvs$annee)
data_tvs$laps <- ifelse(data_tvs$annee %in% (2006:2009), "2006-2009", ifelse(data_tvs$annee %in% (2010:2012), "2010-2012", ifelse(data_tvs$annee %in% (2013:2015), "2013-2015", "2016-2018")))
table(data_tvs$laps)
laps_1 <- plm(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
                NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(laps) - 1,
              data = data_tvs, index = c("code_tvs","annee"), model = "within")
laps_2 <- plm(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
                NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(laps) - 1,
              data = data_tvs, index = c("code_tvs","annee"), model = "within")
# Latex output
stargazer(lag_1, laps_1, lag_2, laps_2,  title="Results", align=TRUE, keep = "densite")