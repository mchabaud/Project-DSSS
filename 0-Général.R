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
library(imputeTS)
library(corrplot)
library(plm)


#### Répertoires ####

mortalite_path <- "~/ENSAE/3A/Projet_DSSS/Données/Mortalité"
APL_path <- "~/ENSAE/3A/Projet_DSSS/Données/APL_densité_médicale/APL"
densite_path <- "~/ENSAE/3A/Projet_DSSS/Données/APL_densité_médicale/Densité médicale"
covariable_path <- "~/ENSAE/3A/Projet_DSSS/Données/Covariables"