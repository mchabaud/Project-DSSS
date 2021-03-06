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


#### R�pertoires ####

mortalite_path <- "~/ENSAE/3A/Projet_DSSS/Donn�es/Mortalit�"
APL_path <- "~/ENSAE/3A/Projet_DSSS/Donn�es/APL_densit�_m�dicale/APL"
densite_path <- "~/ENSAE/3A/Projet_DSSS/Donn�es/APL_densit�_m�dicale/Densit� m�dicale"
covariable_path <- "~/ENSAE/3A/Projet_DSSS/Donn�es/Covariables"