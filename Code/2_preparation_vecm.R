#Pour lire une table excel 
library(readxl)
library(dplyr)
# Pour les tests de racine unite:
#install.packages("urca")
library(urca)
library(fUnitRoots)
# Pour utiliser la fonction d'autocorrelation inverse:
#library(FitAR)
# Pour traiter des SARIMA
library(astsa)
library(aTSA)
# Pour incorporer les tableaux en Latex
library(stargazer)
# Pour tester la significativite des coefficients
library(lmtest)
# Pour tester la significativite des coefficients
library(readr)
# Pour traiter des series temporelles
library(zoo)
#Fournit des outils pour l'analyse univariée des ts
library(forecast)
#Permet de plot des ellipses
library(ellipse)
library(tseries)
library(bootUR)
library(sjPlot)
library(imputeTS)
library(TSstudio)
library(forecast)
library(dynlm)
library(lmtest) 
library(sandwich)
library(ggeffects)
library(stargazer)
library(ecm)
conflict_prefer("pp.test", "aTSA")
conflict_prefer("select", "dplyr")

setwd("C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/wage_equation")

#On extrait l'annee et le mois pour chaque date :
chomage_BIT_estim <- chomage_BIT |>
  arrange(DATE) |>
  mutate(annee=as.numeric(format(DATE, format = "%Y"))) |>
  mutate(mois=as.numeric(format(DATE, format = "%m")))   |>
  mutate(chomage_BIT = chomage_BIT/100) |>
  mutate(log_chom_BIT = log(chomage_BIT)) |>
  mutate(retard1_log_chom_BIT = lag(log_chom_BIT, 1)) |>
  mutate(retard1_chom_BIT = lag(chomage_BIT, 1)) |>
  mutate(quarter = mois -2) |>
  mutate(yearly_chom_bit = chomage_BIT/lag(chomage_BIT, 4)-1) |>
  select(-mois)

cntva <- cntva |>
  arrange(DATE) |>
  mutate(trim_deflateur_va = (deflateur_va/lag(deflateur_va, 1))-1)  |>
  mutate(yearly_deflateur_va = (deflateur_va/lag(deflateur_va, 1))-1) 

IPC_estim <- IPC |>
  mutate(annee=as.numeric(format(DATE, format = "%Y"))) |>
  mutate(mois=as.numeric(format(DATE, format = "%m")))   |>
  filter(mois == "3"|mois == "6"|mois == "9"|mois == "12") |>
  mutate(quarter = mois -2) |>
  select(DATE, cum_inf_2015, annee, quarter) |>
  mutate(log_ipc = log(cum_inf_2015)) |>
  arrange(DATE) |>
  mutate(tx_yearly_ipc = cum_inf_2015/lag(cum_inf_2015, 4)-1) |>
  mutate(tx_trim_ipc = cum_inf_2015/lag(cum_inf_2015, 1)-1) |>
  rename(cum_ipc = cum_inf_2015) |>
  mutate(cum_ipc = cum_ipc/cum_ipc[DATE == "2015-03-01"]*100)

cnt_wage_estim <- cnt_wage_estimation |>
  mutate(annee=as.numeric(format(DATE, format = "%Y"))) |>
  mutate(mois=as.numeric(format(DATE, format = "%m")))   |>
  mutate(quarter = mois -2) |>
  select(-mois) |>
  arrange(DATE) |>
  mutate(log_prod_tete_marng = log(prod_tete_marng)) |>
  mutate(log_prod_travail_marng = log(prod_travail_marng)) |>
  mutate(yearly_lag_log_prod = (lag(log_prod_tete_marng, 1)/lag(log_prod_tete_marng, 5))-1) |>
  arrange(DATE) |>
  mutate(yearly_salh_marng = (salh_marng/lag(salh_marng, 4))-1) |>
  mutate(yearly_prod_travail_marng = (prod_travail_marng/lag(prod_travail_marng, 4))-1) |>
  mutate(yearly_log_prod_travail_marng = (log(prod_travail_marng)/lag(log(prod_travail_marng), 4)-1)) |>
  mutate(log_prod = log(prod_tete_marng))
  

smb_smpt_wage_estimation <- SMB_SMPT |>
  mutate(annee=as.numeric(format(DATE, format = "%Y"))) |>
  mutate(mois=as.numeric(format(DATE, format = "%m")))   |>
  mutate(SHBO = ifelse(DATE == "2020-03-01", SHBO[DATE == "2019-12-01"], SHBO)) |>
  mutate(SHBOE = ifelse(DATE == "2020-03-01", SHBOE[DATE == "2019-12-01"], SHBOE)) |>
  mutate(SMBS = ifelse(DATE == "2020-03-01", SMBS[DATE == "2019-12-01"], SMBS)) |>
  mutate(quarter = mois -2) |>
  select(DATE, annee, quarter, SHBO, SHBOE, SMBS, trim_smbs, trim_shboe, trim_shbo) |>
  arrange(DATE) |>
  mutate(log_shbo = log(SHBO)) |>
  mutate(log_smbs = log(SMBS))

param_smic_estim <- left_join(param_SMIC, IPC_estim) |>
  filter(!is.na(quarter)) |>
  mutate(smic_reel = cum_smic_2015/cum_ipc*100) |>
  arrange(DATE) |>
  mutate(tx_smic_reel = smic_reel/lag(smic_reel, 4)-1) |>
  mutate(tx_smic_nom = cum_smic_2015/lag(cum_smic_2015, 4)-1) |>
  mutate(tx_smic_gmr_nom = cum_smic_mensuel_gmr_2015/lag(cum_smic_mensuel_gmr_2015, 4)-1) |>
  select(DATE, cum_smic_2015, smic_reel, tx_smic_reel, tx_smic_nom, tx_smic_gmr_nom)

rm(wage_estimation)
wage_estimation <- left_join(chomage_BIT_estim,IPC_estim, by = c("DATE", "annee", "quarter"))
wage_estimation <- left_join(wage_estimation, smb_smpt_wage_estimation, by = c("DATE", "annee", "quarter"))
wage_estimation <- left_join(wage_estimation, cnt_wage_estim, by = c("DATE", "annee", "quarter"))
wage_estimation <- left_join(wage_estimation, param_smic_estim, by = c("DATE"))
wage_estimation <- left_join(wage_estimation, rdb_cnt, by = "DATE")
wage_estimation <- left_join(wage_estimation, cntva, by = "DATE")
wage_estimation <- mutate(wage_estimation, year = as.numeric(substr(DATE, 1, 4))) 
wage_estimation <- left_join(wage_estimation, syndicat, by = "year")
wage_estimation <- left_join(wage_estimation, TUC, by = "DATE")
wage_estimation <- left_join(wage_estimation,coup_pouce_smic, by = "DATE")
wage_estimation <- left_join(wage_estimation,deflateur_import, by = "DATE")
wage_estimation <- left_join(wage_estimation,deflateur_conso_tot, by = "DATE")


###Rajout des indicatrices 
wage_estimation_long <- wage_estimation |>
  mutate(ind_t1 = ifelse(quarter ==1, 1, 0)) |>
  mutate(ind_82 = ifelse(DATE < as.Date('1982-03-01'), 1, 0)) |>
  mutate(post_ind_82 = ifelse(DATE >= as.Date('1983-03-01'), 1, 0)) |>
  mutate(dum814 = ifelse(annee == 1981 & quarter == 10, 1, 0)) |>
  mutate(dum821 = ifelse(annee == 1982 & quarter == 1, 1, 0)) |>
  mutate(dum822 = ifelse(annee == 1982 & quarter == 4, 1, 0)) |>
  mutate(dum823 = ifelse(annee == 1982 & quarter == 7, 1, 0)) |>
  mutate(dum824 = ifelse(annee == 1982 & quarter == 10, 1, 0)) |>
  mutate(dum932 = ifelse(annee == 1993 & quarter == 4, 1, 0)) |>
  mutate(dum984 = ifelse(annee == 1998 & quarter == 10, 1, 0)) |>
  mutate(dum084 = ifelse(annee == 2008 & quarter == 10, 1, 0)) |>
  mutate(dum172 = ifelse(annee == 2017 & quarter == 4, 1, 0)) |>
  mutate(dum98_02 = ifelse(annee>=1998 & annee <2002,1, 0)) |>
  mutate(dum001 = ifelse(annee == 2000 & quarter ==1, 1, 0)) |>
  mutate(dum021 = ifelse(annee == 2002 & quarter == 1, 1, 0)) |>
  arrange(DATE) |>
  mutate(retard1_taux_ipc = lag(tx_yearly_ipc, 1)) |>
  mutate(retard2_taux_ipc = lag(tx_yearly_ipc, 2))  |>
  mutate(retard3_taux_ipc = lag(tx_yearly_ipc, 3)) |>
  mutate(trim_shbo = ifelse(DATE == as.Date('2022-06-01'), trim_shboe, trim_shbo)) |>
  mutate(yearly_TUC = TUC/lag(TUC, 4)-1) |>
  mutate(trim_TUC = TUC/lag(TUC, 1)-1) |>
  mutate(log_TUC = log(TUC)) |>
  mutate(annee = as.character(annee)) |>
  arrange(DATE) |>
  mutate(yearly_deflateur_va = (deflateur_va/lag(deflateur_va,4)-1)) |>
  mutate(trim_deflateur_va = (deflateur_va/lag(deflateur_va,1)-1)) |>
  mutate(log_deflateur_va = log(deflateur_va)) |>
  mutate(wedge_conso_va = deflateur_conso_finale_men - deflateur_va)
#filter(annee !='2020' & annee != '2021' & annee != '2022') #|>
#filter(DATE <= as.Date('1998-12-01'))

# #IPC
# ipc <- ts(wage_estimation_long$cum_ipc,start=c(1975,1), end=c(2022, 2), frequency=4)
# tx_ipc<-ts(wage_estimation_long$tx_trim_ipc,start=c(1975,1), end=c(2022, 2), frequency=4)
# diff1_tx_ipc <- diff(tx_ipc,1)
# 
# retard1_taux_ipc<-ts(wage_estimation_long$retard1_taux_ipc,start=c(1975,1), end=c(2022, 2), frequency=4)
# diff1_ret1_tx_ipc <- diff(retard1_taux_ipc, 1)
# 
# retard2_taux_ipc<-ts(wage_estimation_long$retard2_taux_ipc,start=c(1975,1), end=c(2022, 2), frequency=4)
# diff1_ret2_tx_ipc <- diff(retard2_taux_ipc, 1)
# 
# retard3_taux_ipc<-ts(wage_estimation_long$retard3_taux_ipc,start=c(1975,1), end=c(2022, 2), frequency=4)
# diff1_ret3_tx_ipc <- diff(retard3_taux_ipc, 1)
# 
# #Smic réel
# smic_reel <- ts(wage_estimation_long$smic_reel,start=c(1975,1), end=c(2022, 2), frequency=4)
# 
# tx_smic_reel <- ts(wage_estimation_long$tx_smic_reel,start=c(1975,1), end=c(2022, 2), frequency=4)
# diff_tx_smic_reel <- diff(tx_smic_reel, 1)
# 
# #Chomage BIT
# chom_bit <- ts(wage_estimation_long$chomage_BIT, start=c(1975,1), end=c(2022, 2), frequency=4)
# log_chom_bit <- ts(wage_estimation_long$log_chom_BIT, start=c(1975,1), end=c(2022, 2), frequency=4)
# diff_log_chom_bit <- diff(log_chom_bit, 1)
# 
# #SHBO
# shbo <- ts(wage_estimation_long$SHBO, start=c(1975,1), end=c(2022, 2), frequency=4)
# trim_shbo <- ts(wage_estimation_long$trim_shbo, start=c(1975,1), end=c(2022, 2), frequency=4)
# trim_shbo_test <- ts(wage_estimation_long$trim_shbo, start=c(1975,1), end=c(2019, 4), frequency=4)
# diff_trim_shbo <- diff(trim_shbo, 1)
# 
# #SMBS
# smbs <- ts(wage_estimation_long$SMBS, start=c(1975,1), end=c(2022, 2), frequency=4)
# trim_smbs <- ts(wage_estimation_long$trim_smbs, start=c(1985,1), end=c(2022, 2), frequency=4)
# trim_smbs_test <- ts(wage_estimation_long$trim_smbs, start=c(1985,1), end=c(2019, 4), frequency=4)
# 
# ###SMPT
# 
# smpt <- ts(wage_estimation_long$smpt_cnt_marng, start=c(1975,1), end=c(2022, 2), frequency=4)
# trim_smpt <- ts(wage_estimation_long$trim_smpt, start=c(1975,1), end=c(2022, 2), frequency=4)
# 
# ###Déflateur de la conso des ménages
# def_conso_men <- ts(wage_estimation_long$deflateur_conso_finale_men, start=c(1975,1), end=c(2022, 2), frequency=4)
# tx_trim_def_conso_men <- ts(wage_estimation_long$trim_def_conso_men, start=c(1975,1), end=c(2022, 2), frequency=4)
# retard1_tx_def_conso_men  <- ts(wage_estimation_long$retard1_tx_def_conso_men, start=c(1975,1), end=c(2022, 2), frequency=4)
# retard2_tx_def_conso_men <- ts(wage_estimation_long$retard2_tx_def_conso_men, start=c(1975,1), end=c(2022, 2), frequency=4)
# retard3_tx_def_conso_men <- ts(wage_estimation_long$retard3_tx_def_conso_men, start=c(1975,1), end=c(2022, 2), frequency=4)
# 
# ###Productivité par tête du marchand non agricole
# prod_marng <- ts(wage_estimation_long$prod_tete_marng, start=c(1975,1), end=c(2022, 2), frequency=4)
# trim_lag_log_prod <- ts(wage_estimation_long$trim_lag_log_prod, start=c(1975,1), end=c(2022, 2), frequency=4)
# log_prod <- ts(wage_estimation_long$log_prod, start=c(1975,1), end=c(2022, 2), frequency=4)
# ###Déflateur de la VA
# deflateur_va <- ts(wage_estimation_long$deflateur_va, start=c(1975,1), end=c(2022, 2), frequency=4)
# trim_deflateur_va <- ts(wage_estimation_long$trim_deflateur_va, start=c(1975,1), end=c(2022, 2), frequency=4)
# log_deflateur_va <- ts(wage_estimation_long$log_deflateur_va, start=c(1975,1), end=c(2022, 2), frequency=4)
# diff_deflateur_va <- diff(deflateur_va, 1)
# wage_estim_tuc <- filter(wage_estimation_long, substr(DATE, 1, 4)>=1977)
# tuc <- ts(wage_estim_tuc$TUC, start=c(1977,1), end=c(2022, 2), frequency=4)
##Labelisation
var_label(wage_estimation_long$trim_shbo) <- "Croissance trimestrielle SHBO"
var_label(wage_estimation_long$trim_smbs) <- "Croissance trimestrielle SMBS"
var_label(wage_estimation_long$yearly_salh_marng) <- "G.A. du salaire horaire marchand non agricole"
var_label(wage_estimation_long$yearly_deflateur_va) <- "G.A. du déflateur VA"
var_label(wage_estimation_long$yearly_def_conso_men) <- "G.A. du déflateur de la consommation"
var_label(wage_estimation_long$yearly_lag_log_prod) <- "G.A. de la productivité (retard 1 trim)"
var_label(wage_estimation_long$log_prod) <- "Niveau de la prod. (T-1)"
var_label(wage_estimation_long$tx_yearly_ipc) <- "G.A. de l'IPC"
var_label(wage_estimation_long$ind_82) <- "Ind. année<1982"
var_label(wage_estimation_long$retard1_taux_ipc) <- "Retard IPC 1 trimestre"
var_label(wage_estimation_long$retard2_taux_ipc)  <- "Retard IPC 2 trimestres"
var_label(wage_estimation_long$retard3_taux_ipc) <- "Retard IPC 3 trimestres"
var_label(wage_estimation_long$tx_smic_reel)  <- "G.A. du SMIC réel"
var_label(wage_estimation_long$tx_smic_nom) <- "G.A. du SMIC nominal"
var_label(wage_estimation_long$yearly_prod_travail_marng)  <- "G.A. de la productivité horaire marchande"
var_label(wage_estimation_long$log_chom_BIT) <- "Log du chômage-BIT"
var_label(wage_estimation_long$retard1_log_chom_BIT)  <- "Retard du log-chômage BIT"
var_label(wage_estimation_long$ind_t1)  <- "Ind. T1"
var_label(wage_estimation_long$dum814)  <- "Ind. 1981-T4"
var_label(wage_estimation_long$dum822) <- "Ind. 1982-T2"
var_label(wage_estimation_long$dum823) <- "Ind. 1982-T3"
var_label(wage_estimation_long$dum824) <-  "Ind. 1982-T4"
var_label(wage_estimation_long$dum932) <-"Ind. 1993-T2"
var_label(wage_estimation_long$dum984) <-"Ind. 1998-T4"
var_label(wage_estimation_long$dum98_02) <- "Ind. 1998-2002"
var_label(wage_estimation_long$dum001) <- "Ind. 2001-T1"
var_label(wage_estimation_long$dum021) <- "Ind. 2002-T1"
var_label(wage_estimation_long$dum084) <- "Ind. 2008-T4"
var_label(wage_estimation_long$dum172) <- "Ind. 2017-T2"


