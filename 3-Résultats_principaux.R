#### Régressions ####

data_tvs1 <- read_csv("~/ENSAE/3A/Projet_DSSS/Données/base_finale_tvs_balanced.csv")

#pooled OLS (lm package)

pooling <- lm(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
                NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 +
                as.factor(annee), 
              data = data_tvs1)
summary(pooling)

pooling <- lm(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
                NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 +
                as.factor(annee), 
              data = data_tvs1)
summary(pooling)


#pooled OLS (plm package)

pooling <- plm(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee),
               data = data_tvs1, index = c("code_tvs","annee"), model = "pooling")
#summary(pooling)
summary(pooling, vcov = function(x) vcovHC(x, method="arellano", cluster = "group")) #variance robuste à l'héroscédasticité et clustérisée à l'échelle du TVS
#summary(pooling, vcov = vcovHC)


# pooling <- lm.cluster(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
#                  NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
#                  CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee),
#                data = data_tvs1, cluster = "code_tvs")
# summary(pooling)


pooling <- plm(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee),
               data = data_tvs1, index = c("code_tvs","annee"), model = "pooling")
summary(pooling, vcov = function(x) vcovHC(x, method="arellano", cluster = "group"))

pooling <- plm(part_jeunes ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee),
               data = data_tvs1, index = c("code_tvs","annee"), model = "pooling")
summary(pooling, vcov = function(x) vcovHC(x, method="arellano", cluster = "group"))



#within

within <- plm(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee) - 1,
              data = data_tvs1, index = c("code_tvs","annee"), model = "within")
summary(within, vcov = function(x) vcovHC(x, method="arellano", cluster = "group"))

within <- plm(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + NB_D401 + NB_D106 +
                NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee) - 1,
              data = data_tvs1, index = c("code_tvs","annee"), model = "within")
summary(within, vcov = function(x) vcovHC(x, method="arellano", cluster = "group"))



fixed_age <- feols(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                     NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                     CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
                   | code_tvs, data=data_tvs1)
summary(fixed_age)


fixed_logmorts <- feols(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                          NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                          CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
                        | code_tvs, data=data_tvs1)
summary(fixed_logmorts)


etable(list(fixed_age, fixed_logmorts), tex=TRUE,dict=c("log_pop" = "Log(population)", "log_morts" = "Log nb. morts", "age_moyen_deces" = "Âge moyen de décès", "densite" = "Densité médicale", "densite_infirmiers" = "Nb. infirmiers pour 1000 hab.", "taille_menage" = "Taille des ménages", "med_niveau_vie" = "Médiane du niveau de vie", "NB_D401" = "Hébergements pour personnes âgées", "NB_D106" = "Services d'Urgences", "NB_D104" = "Etablissements psychiatriques", "NB_D101" = "Etablissements de santé court séjour", "NB_D102" = "Etablissements de santé moyen séjour", "NB_D103" = "Etablissements de santé long séjour", "CSprof_interm" = "Proportion de professions intermédiaires", "CScadres" = "Proportion de cadres", "CSemployes" = "Proportion d'employés", "CSouvriers" = "Proportion d'ouvriers", "CSagriculteurs" = "Proportion d'agriculteurs", "CSartisans_commercants"= "Proportion d'artisans-commerçants", "CSretraites" = "Proportion de retraités", "Fpop_tot" = "Proportion de femmes", "`15_29`" = "Proportion de 15-29 ans", "`30_44`" = "Proportion de 30-44 ans", "`45_59`" = "Proportion de 45-59 ans", "`60_74`" = "Proportion de 60-74 ans", "plus75" = "Proportion de plus de 75 ans", "as.factor(annee)2007" = "2007","as.factor(annee)2008" = "2008", "as.factor(annee)2009" = "2009", "as.factor(annee)2010" = "2010", "as.factor(annee)2011" = "2011", "as.factor(annee)2012" = "2012", "as.factor(annee)2013" = "2013", "as.factor(annee)2014" = "2014", "as.factor(annee)2015" = "2015", "as.factor(annee)2016" = "2016", "as.factor(annee)2017" = "2017","as.factor(annee)2018" = "2018", "NB_D105" = "Centres de Lutte contre le Cancer"))




fixed <- feols(part_jeunes ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=data_tvs1)
summary(fixed)




#first-diff

first_diff <- plm(age_moyen_deces ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                    NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                    CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee) - 1,
                  data = data_tvs1, index = c("code_tvs","annee"), model = "fd")
summary(first_diff, vcov = function(x) vcovHC(x, method="arellano", cluster = "group"))

first_diff <- plm(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                    NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                    CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee) - 1,
                  data = data_tvs1, index = c("code_tvs","annee"), model = "fd")
summary(first_diff, vcov = function(x) vcovHC(x, method="arellano", cluster = "group"))

first_diff <- plm(part_jeunes ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                    NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                    CSagriculteurs + CSretraites + Fpop_tot + X15_29 + X30_44 + X45_59 + X60_74 + plus75 + as.factor(annee) - 1,
                  data = data_tvs1, index = c("code_tvs","annee"), model = "fd")
summary(first_diff, vcov = function(x) vcovHC(x, method="arellano", cluster = "group"))



#TVS avec beaucoup de personnes âgées

data_tvs1 %<>% mutate(part_vieux2 = `60_74` + plus75)
data_moy <- data_tvs1 %>% group_by(code_tvs) %>% summarise(moy_vieux = mean(part_vieux2))
quantile(data_moy$moy_vieux, probs = c(0, 0.1, 0.2, 0.5, 0.75, 0.9, 1))
#20% : 0.66
data_moy %<>% filter(moy_vieux >= quantile(data_moy$moy_vieux, probs = 0.75))
A2 <- data_tvs1 %>% filter(code_tvs %in% data_moy$code_tvs)

fixed <- feols(log_morts ~ densite + log_pop + taille_menage + med_niveau_vie + densite_infirmiers + NB_D401 + NB_D106 +
                 NB_D101 + NB_D102 + NB_D103 + NB_D104 + NB_D105 + CSprof_interm + CScadres + CSemployes + CSouvriers + CSartisans_commercants +
                 CSagriculteurs + CSretraites + Fpop_tot + `15_29` + `30_44` + `45_59` + `60_74` + plus75 + as.factor(annee)
               | code_tvs, data=A2)
summary(fixed)
