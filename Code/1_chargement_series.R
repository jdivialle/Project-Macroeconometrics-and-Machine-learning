#Séries utiles pour l'estimation de l'équation de salaire 
rm(list=ls())
library(insee)
library(doremifasol)
library(eurostat)
library(OECD)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl) 
library(tsibble)
library(tibble)
library(janitor)
library(tsbox)
library(knitr)
library(tempdisagg)
library(labelled)
library(stringr)
library(Hmisc)
library(lubridate)
library(conflicted)
library(base)
library(tidyr)
library(bootUR)
library(ecm)
library(sjPlot)
conflict_prefer("rename", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("replace", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("relocate", "dplyr")
conflict_prefer("lead", "dplyr")

#Chemin d'accès 
setwd("C:/Users/gasto/Documents/GitHub/projet-ml-macro")

#Liste générale 
cols_to_select <- c('DATE','OBS_VALUE')

##Appel des bases collectées dans le fichier 0_bases_volumineuses
load(file = file.path("data/Data_FRANCE", "IPC_OCDE.RData"))
load(file = file.path("data/Data_FRANCE", "cnt.RData"))

#Données Insee a partir de 1990 pour l'inflation 
IPC <- get_insee_idbank('001759970')
IPC <- IPC[cols_to_select]
IPC <- rename(IPC, "IPC" = "OBS_VALUE")
IPC<- IPC[order(as.Date(IPC$DATE, format="%d/%m/%Y")),]
IPC <- mutate(IPC, cum_inf_1990 = IPC/IPC[which(DATE == "1990-01-01")]*100)

#prévision d'inflation jusque décembre 2022
file_prev_inf <- "data/Data_FRANCE/prev_inf_22.xlsx"
prev_inf_22 <-read_excel(file_prev_inf) 
prev_inf_22$DATE <- as.Date(prev_inf_22$DATE)
IPC <- full_join(IPC, prev_inf_22)
IPC<- IPC[order(as.Date(IPC$DATE, format="%d/%m/%Y"), decreasing = TRUE),]
i <-1
while(i <=4){
  IPC[i, "cum_inf_1990"] <- IPC[i+12, "cum_inf_1990"]*(1+(IPC[i, "annual_ipc"]/100)) 
  i <- i+1
}


#Rajout du glissement annuel (a partir de 1991)
ipc_gliss_annuel <- get_insee_idbank('001761313')
ipc_gliss_annuel <- ipc_gliss_annuel[cols_to_select]
ipc_gliss_annuel <- rename(ipc_gliss_annuel, "anual_ipc" = "OBS_VALUE")
IPC <- left_join(IPC, ipc_gliss_annuel) |>
  mutate(annual_ipc = ifelse(!is.na(anual_ipc), anual_ipc, annual_ipc)) |>
  select(-anual_ipc)
#fusion inflation INSEE (post 1990)-OCDE (pré 1990)
IPC2 <- rbind(IPC_OCDE, IPC)
IPC2<- IPC2[order(as.Date(IPC2$DATE, format="%d/%m/%Y")),]
IPC2 <- mutate(IPC2, cum_inf_2015 = cum_inf_1990/cum_inf_1990[DATE == '2015-01-01']*100)
IPC2<- mutate(IPC2, annual_ipc = (cum_inf_1990 - Lag(cum_inf_1990, +12))/Lag(cum_inf_1990, +12))
IPC2<- mutate(IPC2, month_ipc = (cum_inf_1990 - Lag(cum_inf_1990, +1))/Lag(cum_inf_1990, +1))
IPC2 <- filter(IPC2, DATE >= as.Date("1970-01-01"))
rm(IPC_OCDE, ipc_gliss_annuel, IPC)
IPC <- IPC2 
rm(IPC2)

IPC <- mutate(IPC, tcam_ipc = ifelse(substr(DATE, 6, 7) == "01", (cum_inf_2015 + lead(cum_inf_2015, 1)+ lead(cum_inf_2015, 2)+ lead(cum_inf_2015, 3)+ lead(cum_inf_2015, 4)+ lead(cum_inf_2015, 5)+ lead(cum_inf_2015, 6)+ lead(cum_inf_2015, 7)+ lead(cum_inf_2015, 8)+ lead(cum_inf_2015, 9)+ lead(cum_inf_2015, 10)+ lead(cum_inf_2015, 11))/(lag(cum_inf_2015, 1)+ lag(cum_inf_2015, 2)+ lag(cum_inf_2015, 3)+ lag(cum_inf_2015, 4)+ lag(cum_inf_2015, 5)+ lag(cum_inf_2015, 6)+ lag(cum_inf_2015, 7)+ lag(cum_inf_2015, 8)+ lag(cum_inf_2015, 9)+ lag(cum_inf_2015, 10)+ lag(cum_inf_2015, 11)+ lag(cum_inf_2015, 12))-1, NA))
IPC <- fill(IPC, tcam_ipc)

#Chomage au sens du BIT
chomage_BIT <- 	get_insee_idbank('001688527')
chomage_BIT<- select(chomage_BIT, TIME_PERIOD, OBS_VALUE)
chomage_BIT <- rename(chomage_BIT, c("DATE" = "TIME_PERIOD", "chomage_BIT" = "OBS_VALUE"))
chomage_BIT$DATE <- str_replace(chomage_BIT$DATE,"Q1", "03")
chomage_BIT$DATE <- str_replace(chomage_BIT$DATE,"Q2", "06")
chomage_BIT$DATE <- str_replace(chomage_BIT$DATE,"Q3", "09")
chomage_BIT$DATE <- str_replace(chomage_BIT$DATE,"Q4", "12")
chomage_BIT$DATE<-as.Date(paste(chomage_BIT$DATE,1,sep="-"), format = "%Y-%m-%d")

#TUC
TUC <- get_insee_idbank('001586737')
TUC<- select(TUC, TIME_PERIOD, OBS_VALUE)
TUC <- rename(TUC, c("DATE" = "TIME_PERIOD", "TUC" = "OBS_VALUE"))
TUC$DATE <- str_replace(TUC$DATE,"Q1", "03")
TUC$DATE <- str_replace(TUC$DATE,"Q2", "06")
TUC$DATE <- str_replace(TUC$DATE,"Q3", "09")
TUC$DATE <- str_replace(TUC$DATE,"Q4", "12")
TUC$DATE<-as.Date(paste(TUC$DATE,1,sep="-"), format = "%Y-%m-%d")

######### COMPTES NATIONAUX TRIMESTRIELS
scale_2019t4 <- function(x, na.rm = FALSE) x/x[cntbis[,1] == "2019-12-01"]*100
#SMNA-CNT : ensemble principalement marchand non agricole
#DI-CNT : industrie ensemble 
#A17-FZ : construction 
cntbis <- cnt |>
  filter(OPERATION == "B1" | OPERATION == "EMP" | OPERATION == "EMPNS" |  OPERATION == "DHES" |  OPERATION == "DHET" |  OPERATION == "VHTS" |  OPERATION == "VHTT" |  OPERATION == "D1") |>
  filter(CNA_PRODUIT == "DIM-CNT" | CNA_PRODUIT == "A17-FZ" | CNA_PRODUIT == "DSM-CNT" | CNA_PRODUIT =="DSN-CNT" | CNA_PRODUIT == "SMNA-CNT" | CNA_PRODUIT == "D-CNT") |>
  mutate(op_pr_val = paste(OPERATION, CNA_PRODUIT, VALORISATION, UNIT_MEASURE, sep = "-")) |>
  select(DATE, TIME_PERIOD, OBS_VALUE, OPERATION, CNA_PRODUIT, VALORISATION, UNIT_MEASURE, op_pr_val) |>
  spread(key = "op_pr_val",
         value = "OBS_VALUE") 
cntbis$DATE <- str_replace(cntbis$DATE,"-01-", "-03-")
cntbis$DATE <- str_replace(cntbis$DATE,"-04-", "-06-")
cntbis$DATE <- str_replace(cntbis$DATE,"-07-", "-09-")
cntbis$DATE <- str_replace(cntbis$DATE,"-10-", "-12-")
cntbis$DATE <-as.Date(paste(cntbis$DATE,1,sep="-"))
cntbis <- mutate_at(cntbis,  vars(7:72), list(~ recode(., `NULL` = 0))) |>
  mutate_at(vars(7:72), as.numeric) |>
  mutate_at(vars(7:72), ~replace(., is.na(.), 0)) |>
  group_by(DATE) |>
  summarise_at(vars(6:71),              # Specify column
               list(name = sum)) |>
  rename("va_man"= "B1-DIM-CNT-L-EUROS_name", "va_term" = "B1-DSM-CNT-L-EUROS_name", 
         "va_ternm" = "B1-DSN-CNT-L-EUROS_name", "wage_man"="D1-DIM-CNT-SO-EUROS_name", 
         "wage_term"="D1-DSM-CNT-SO-EUROS_name", "wage_ternm"= "D1-DSN-CNT-SO-EUROS_name", 
         "eff_travail_salman"="DHES-DIM-CNT-SO-HEURES_name", "eff_travail_salterm"= "DHES-DSM-CNT-SO-HEURES_name", 
         "eff_travail_salternm" ="DHES-DSN-CNT-SO-HEURES_name", "eff_travail_globman"= "DHET-DIM-CNT-SO-HEURES_name", 
         "eff_travail_globterm"="DHET-DSM-CNT-SO-HEURES_name", "eff_travail_globternm"= "DHET-DSN-CNT-SO-HEURES_name", 
         "emp_etp_globman"="EMP-DIM-CNT-SO-ETP_name", "emp_ind_globman"= "EMP-DIM-CNT-SO-INDIVIDUS_name", 
         "emp_glob_etp_term"="EMP-DSM-CNT-SO-ETP_name", "emp_glob_ind_term"="EMP-DSM-CNT-SO-INDIVIDUS_name", 
         "emp_glob_etp_ternm"="EMP-DSN-CNT-SO-ETP_name", "emp_glob_ind_ternm"="EMP-DSN-CNT-SO-INDIVIDUS_name", 
         "empns_etp_man" ="EMPNS-DIM-CNT-SO-ETP_name", "empns_ind_man" ="EMPNS-DIM-CNT-SO-INDIVIDUS_name", 
         "empns_etp_term"="EMPNS-DSM-CNT-SO-ETP_name", "empns_ind_term"="EMPNS-DSM-CNT-SO-INDIVIDUS_name",
         "empns_etp_ternm"="EMPNS-DSN-CNT-SO-ETP_name", "empns_ind_ternm"="EMPNS-DSN-CNT-SO-INDIVIDUS_name", 
         "vol_travail_sal_man"="VHTS-DIM-CNT-SO-HEURES_name", "vol_travail_sal_term"="VHTS-DSM-CNT-SO-HEURES_name", 
         "vol_travail_sal_ternm"="VHTS-DSN-CNT-SO-HEURES_name", "vol_travail_globman"="VHTT-DIM-CNT-SO-HEURES_name", 
         "vol_travail_glob_term"="VHTT-DSM-CNT-SO-HEURES_name", "vol_travail_glob_ternm" ="VHTT-DSN-CNT-SO-HEURES_name", 
         "va_marng"= "B1-SMNA-CNT-L-EUROS_name",
         "wage_marng"="D1-SMNA-CNT-SO-EUROS_name",
         "eff_travail_salmarng"="DHES-SMNA-CNT-SO-HEURES_name",
         "eff_travail_globmarng"="DHET-SMNA-CNT-SO-HEURES_name",
         "emp_ind_globmarng"= "EMP-SMNA-CNT-SO-INDIVIDUS_name",
         "emp_etp_globmarng"="EMP-SMNA-CNT-SO-ETP_name",
         "empns_etp_marng" ="EMPNS-SMNA-CNT-SO-ETP_name", 
         "empns_ind_marng" ="EMPNS-SMNA-CNT-SO-INDIVIDUS_name",
         "vol_travail_sal_marng"="VHTS-SMNA-CNT-SO-HEURES_name",
         "vol_travail_glob_marng"="VHTT-SMNA-CNT-SO-HEURES_name",
         "va_ag"= "B1-A17-FZ-L-EUROS_name",
         "wage_ag"="D1-A17-FZ-SO-EUROS_name",
         "eff_travail_salag"="DHES-A17-FZ-SO-HEURES_name",
         "eff_travail_globag"="DHET-A17-FZ-SO-HEURES_name",
         "emp_ind_globag"= "EMP-A17-FZ-SO-INDIVIDUS_name",
         "emp_etp_globag"="EMP-A17-FZ-SO-ETP_name",
         "empns_etp_ag" ="EMPNS-A17-FZ-SO-ETP_name", 
         "empns_ind_ag" ="EMPNS-A17-FZ-SO-INDIVIDUS_name",
         "vol_travail_sal_ag"="VHTS-A17-FZ-SO-HEURES_name",
         "vol_travail_glob_ag"="VHTT-A17-FZ-SO-HEURES_name", 
         "va_tot"= "B1-D-CNT-L-EUROS_name",
         "wage_tot"="D1-D-CNT-SO-EUROS_name",
         "eff_travail_saltot"="DHES-D-CNT-SO-HEURES_name",
         "eff_travail_globtot"="DHET-D-CNT-SO-HEURES_name",
         "emp_ind_globtot"= "EMP-D-CNT-SO-INDIVIDUS_name",
         "emp_etp_globtot"="EMP-D-CNT-SO-ETP_name",
         "empns_etp_tot" ="EMPNS-D-CNT-SO-ETP_name", 
         "empns_ind_tot" ="EMPNS-D-CNT-SO-INDIVIDUS_name",
         "vol_travail_sal_tot"="VHTS-D-CNT-SO-HEURES_name",
         "vol_travail_glob_tot"="VHTT-D-CNT-SO-HEURES_name",) |>
  select(-"B1-DIM-CNT-V-EUROS_name", -"B1-A17-FZ-V-EUROS_name", -"B1-DSN-CNT-V-EUROS_name",
         -"B1-DSM-CNT-V-EUROS_name", -"B1-SMNA-CNT-V-EUROS_name", -"B1-D-CNT-V-EUROS_name")

cntbis <- mutate_at(cntbis, vars(2:61), scale_2019t4) |>
  arrange(DATE) |>
  mutate(tcam_va_marng = ifelse(substr(DATE, 6, 7) == "03", (va_marng+lead(va_marng, 1)+lead(va_marng, 2)+lead(va_marng, 3))/(lag(va_marng, 4)+lag(va_marng, 3)+lag(va_marng, 2)+lag(va_marng, 1))-1, NA)) |>
  mutate(tcam_va_tot = ifelse(substr(DATE, 6, 7) == "03", (va_tot+lead(va_tot, 1)+lead(va_tot, 2)+lead(va_tot, 3))/(lag(va_tot, 4)+lag(va_tot, 3)+lag(va_tot, 2)+lag(va_tot, 1))-1, NA))
cntbis <-  fill(cntbis, tcam_va_marng, tcam_va_tot) 
cntbis <-  mutate(cntbis, tcam_va_marng = ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_va_marng))
cntbis <-  mutate(cntbis, tcam_va_tot = ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_va_tot))

cntbis <- cntbis |>
  mutate(prod_tete_ag = va_ag/emp_ind_globag*100) |>
  mutate(prod_tete_man = va_man/emp_ind_globman*100) |>
  mutate(prod_tete_term = va_term/emp_glob_ind_term*100) |>
  mutate(prod_tete_ternm = va_ternm/emp_glob_ind_ternm*100) |>
  mutate(prod_tete_marng = va_marng/emp_ind_globmarng*100) |>
  mutate(prod_tete_tot = va_tot/emp_ind_globtot*100) |>
  mutate(smpt_cnt_tot = wage_tot/emp_ind_globtot*100) |>
  mutate(smpt_cnt_marng = wage_marng/emp_ind_globmarng*100) |>
  mutate(salh_marng = wage_marng/vol_travail_glob_marng*100) |>
  mutate(prod_travail_ag = va_ag/vol_travail_glob_ag*100) |>
  mutate(prod_travail_man = va_man/vol_travail_globman*100) |>
  mutate(prod_travail_term = va_term/vol_travail_glob_term*100) |>
  mutate(prod_travail_ternm = va_ternm/vol_travail_glob_ternm*100) |>
  mutate(prod_travail_marng = va_marng/vol_travail_glob_marng*100) |>
  mutate(prod_travail_tot = va_tot/vol_travail_glob_tot*100) |>
  mutate(tcam_prod_travail_tot = ifelse(substr(DATE, 6, 7) == "03", (prod_travail_tot+lead(prod_travail_tot, 1)+lead(prod_travail_tot, 2)+lead(prod_travail_tot, 3))/(lag(prod_travail_tot, 4)+lag(prod_travail_tot, 3)+lag(prod_travail_tot, 2)+lag(prod_travail_tot, 1))-1, NA)) |>
  mutate(tcam_prod_tete_tot = ifelse(substr(DATE, 6, 7) == "03", (prod_tete_tot+lead(prod_tete_tot, 1)+lead(prod_tete_tot, 2)+lead(prod_tete_tot, 3))/(lag(prod_tete_tot, 4)+lag(prod_tete_tot, 3)+lag(prod_tete_tot, 2)+lag(prod_tete_tot, 1))-1, NA))


cntbis <-  fill(cntbis, tcam_prod_travail_tot, tcam_prod_tete_tot) 
cntbis <-  mutate(cntbis, tcam_prod_travail_tot= ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_prod_travail_tot))
cntbis <-  mutate(cntbis, tcam_prod_tete_tot = ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_prod_tete_tot))

cnt_wage_estimation <- select(cntbis, DATE, prod_travail_marng, prod_tete_marng, smpt_cnt_marng, salh_marng) |>
  mutate(trim_smpt = (smpt_cnt_marng/lag(smpt_cnt_marng, 1))-1) |>
  mutate(lag_trim_smpt = lag(trim_smpt, 1)) 

### Série des salaires 
file_SMPT <- "data/Data_FRANCE/SMB_SMPT.xlsx"
SMB_SMPT <- read_excel(file_SMPT, 
                       sheet = "Feuil3")
SMB_SMPT <- as.data.frame(SMB_SMPT)
SMB_SMPT <- select(SMB_SMPT, 1:5)
SMB_SMPT$DATE<-as.Date(paste(SMB_SMPT$DATE,1,sep="-"), format = "%Y-%m-%d")
SMB_SMPT <- rename(SMB_SMPT, c("SMPT" = "smpt", "SHBO"="SHBO base harmonisée","SMBS"="SMBS base harmonisée")) |>
  select(DATE, SHBO, SHBOE, SMBS, SMPT) |>
  arrange(DATE) |>
  mutate(trim_shbo = SHBO/lag(SHBO, +1)-1) |>
  mutate(trim_shboe = SHBOE/lag(SHBOE, +1)-1) |>
  mutate(trim_smpt = SMPT/lag(SMPT, +1)-1) |>
  mutate(tcam_smpt = ifelse(substr(DATE, 6, 7) == "03", (SMPT+lead(SMPT, 1)+lead(SMPT, 2)+lead(SMPT, 3))/(lag(SMPT, 4)+lag(SMPT, 3)+lag(SMPT, 2)+lag(SMPT, 1))-1, NA)) |>
  mutate(trim_smbs = SMBS/lag(SMBS, +1)-1) 

SMB_SMPT <- SMB_SMPT  |>
  mutate(SHBO = ifelse(DATE == "2020-03-01", (lag(SHBO, 1)+lead(SHBO, 1))/2, SHBO)) |>
  mutate(SHBOE = ifelse(DATE == "2020-03-01", (lag(SHBOE, 1)+lead(SHBOE, 1))/2, SHBOE)) |>
  mutate(SMBS = ifelse(DATE == "2020-03-01", (lag(SMBS, 1)+lead(SMBS, 1))/2, SMBS)) |>
  mutate(tcam_shbo = ifelse(substr(DATE, 6, 7) == "03", (SHBO+lead(SHBO, 1)+lead(SHBO, 2)+lead(SHBO, 3))/(lag(SHBO, 4)+lag(SHBO, 3)+lag(SHBO, 2)+lag(SHBO, 1))-1, NA)) |>
  mutate(tcam_shboe = ifelse(substr(DATE, 6, 7) == "03", (SHBOE+lead(SHBOE, 1)+lead(SHBOE, 2)+lead(SHBOE, 3))/(lag(SHBOE, 4)+lag(SHBOE, 3)+lag(SHBOE, 2)+lag(SHBOE, 1))-1, NA)) |>
  mutate(tcam_smbs = ifelse(substr(DATE, 6, 7) == "03", (SMBS+lead(SMBS, 1)+lead(SMBS, 2)+lead(SMBS, 3))/(lag(SMBS, 4)+lag(SMBS, 3)+lag(SMBS, 2)+lag(SMBS, 1))-1, NA))  |>
  mutate(SHBO = ifelse(DATE == "2020-03-01", NA, SHBO)) |>
  mutate(SHBOE = ifelse(DATE == "2020-03-01", NA, SHBOE)) |>
  mutate(SMBS = ifelse(DATE == "2020-03-01", NA, SMBS)) 

SMPT_cnt <- cntbis |>
  select(DATE, smpt_cnt_tot, smpt_cnt_marng) |>
  mutate(trim_smpt = (smpt_cnt_marng/lag(smpt_cnt_marng, 1))-1) |>
  mutate(lag_trim_smpt = lag(trim_smpt, 1)) 
SMB_SMPT <- left_join(SMB_SMPT, SMPT_cnt, by ="DATE")

######## Paramètres SMIC 
param_SMIC <- get_insee_idbank("000879877")
param_SMIC <- param_SMIC[cols_to_select]
param_SMIC <- rename(param_SMIC, "smic_h" = "OBS_VALUE")
param_SMIC <- mutate(param_SMIC, smic_h = smic_h/151.67)
param_SMIC <- filter(param_SMIC, DATE > as.Date("2019-01-01"))
param_SMIC$DATE <- as.Date(paste(param_SMIC$DATE,1,sep="-"), format = "%Y-%m-%d") 


#Appariemment pré 2019
file_smic_ipp <- "data/Data_FRANCE/smic.xlsx"
param_smic_ipp <- read_excel(file_smic_ipp, skip = 1)
param_smic_ipp <- rename(param_smic_ipp, "DATE" = "...1", "smic_h" = "Smic brut (horaire)")
param_smic_ipp <- select(param_smic_ipp, DATE, smic_h)
param_smic_ipp$DATE <- as.Date(paste(param_smic_ipp$DATE,1,sep="-"), format = "%Y-%m-%d")

seq_date <- seq(as.Date("1970-01-01"), as.Date("2019-01-01"), by = "month")
seq_date <- as.data.frame(seq_date)
seq_date <- rename(seq_date, "DATE" = "seq_date")

param_smic_ipp <- left_join(seq_date, param_smic_ipp)
param_SMIC <- rbind(param_SMIC, param_smic_ipp)
param_SMIC<- param_SMIC[order(as.Date(param_SMIC$DATE, format="%d/%m/%Y")),]
param_SMIC <- mutate(param_SMIC, smic_h = ifelse(DATE <as.Date("2002-01-01"), smic_h/6.55957, smic_h))

param_SMIC <- fill(param_SMIC, smic_h)
param_SMIC <- mutate(param_SMIC, value_2015 = ifelse(DATE == "2015-01-01", smic_h, 0))
param_SMIC <- mutate(param_SMIC, value_2015 = value_2015[which.max(value_2015)])
param_SMIC <- mutate(param_SMIC, cum_smic_2015 = smic_h/value_2015*100)
param_SMIC <- select(param_SMIC, -value_2015)

rm(seq_date, param_smic_ipp)

###Output gap 
file_outputgap <- "data/Data_FRANCE/Output_gap_france_imf.csv"
output_gap <- read.csv(file = file_outputgap)
output_gap <- mutate(output_gap, year = substr(Date, 1, 4))
output_gap <- select(output_gap, year, Value)
output_gap <- rename(output_gap, "Output_gap" = "Value")

#Taux de syndicalisation 
file_syndic <- "data/Data_FRANCE/Serie_taux_syndicalisation_France_1949_2019_.xlsx"
syndicat<- read_excel(file_syndic, sheet = "Série longue syndicalisation", skip = 1, range = "A2:B73")
syndicat <- rename(syndicat, c("year" = "Année", "tx_syndic" = "Taux de Syndicalisation"))
a <- as.vector(syndicat$year)
b <- as.vector(syndicat$tx_syndic)
year=seq(1949,2019,1)
tx_syndic=approxExtrap(a, b, xout = year, method="linear", ties="ordered")$y
syndicat_final <- data.frame(year,tx_syndic)
syndicat <- syndicat_final
rm(syndicat_final, a, b, tx_syndic, year)

###Taux d'?change
exch_rate <- get_insee_idbank("010002053")
exch_rate <- exch_rate[cols_to_select]
exch_rate <- rename(exch_rate, "exch_rate" = "OBS_VALUE")

###Taux d'intérêt nominal fixé par la BCE
file_bce_rate<- "data/Data_FRANCE/taux_bce.csv"
BCE_rate <- read.csv(file = file_bce_rate, sep = ",")
BCE_rate <- rename(BCE_rate,  c("DATE"= "X", "depot_rate" = "ECB.Deposit.facility...date.of.changes..raw.data.", "lending_rate" ="ECB.Marginal.lending.facility...date.of.changes..raw.data.", "director_rate"="ECB.Main.refinancing.operations...fixed.rate.tenders..fixed.rate...date.of.changes."))
BCE_rate <- mutate(BCE_rate, month = substr(DATE, 6,7))
BCE_rate <- mutate(BCE_rate, day = substr(DATE, 9,10))
BCE_rate$DATE <- paste0(BCE_rate$DATE, "-01")
BCE_rate$DATE <- as.Date(paste(BCE_rate$DATE,1,sep="-"), format = "%Y-%m-%d")
BCE_rate <- filter(BCE_rate, month == "03" | month == "06" | month == "09" | month == "12") |>
  filter(day == "01")


###Déflateur conso des ménages 
file_rdb <- "data/Data_FRANCE/deflateur_conso_menages_t2_2002.xlsx"
rdb_cnt <- read_excel(file_rdb) |>
  arrange(DATE) |>
  mutate(yearly_def_conso_men = (deflateur_conso_finale_men/lag(deflateur_conso_finale_men, 4))-1) |>
  mutate(trim_def_conso_men = (deflateur_conso_finale_men/lag(deflateur_conso_finale_men, 1))-1) |>
  mutate(retard1_tx_def_conso_men = (lag(deflateur_conso_finale_men, 1)/lag(deflateur_conso_finale_men, 2))-1) |>
  mutate(retard2_tx_def_conso_men = (lag(deflateur_conso_finale_men, 2)/lag(deflateur_conso_finale_men, 3))-1) |>
  mutate(retard3_tx_def_conso_men = (lag(deflateur_conso_finale_men, 3)/lag(deflateur_conso_finale_men, 4))-1) 


###Déflateur des prix de VA
cntva <- cnt |>
  filter(OPERATION == "B1" & CNA_PRODUIT == "D-CNT") |>
  filter(VALORISATION == "L" | VALORISATION == "V") |>
  select(DATE, OBS_VALUE, VALORISATION) |>
  spread(key = "VALORISATION",
         value = "OBS_VALUE") |>
  rename(vol = L, val = V) |>
  mutate(deflateur_va = val/vol*100) |>
  select(-vol, -val) 
cntva$DATE <- str_replace(cntva$DATE,"-01-01", "-03-01")
cntva$DATE <- str_replace(cntva$DATE,"-04-01", "-06-01")
cntva$DATE <- str_replace(cntva$DATE,"-07-01", "-09-01")
cntva$DATE <- str_replace(cntva$DATE,"-10-01", "-12-01")
cntva$DATE<-as.Date(paste(cntva$DATE,1,sep="-"), format = "%Y-%m-%d")

#GMR 
gmr2000<- "data/Data_FRANCE/gmr.xlsx"
gmr <-read_excel(gmr2000) 
gmr$DATE<-as.Date(paste(gmr$DATE,1,sep="-"), format = "%Y-%m-%d")
param_SMIC <- left_join(param_SMIC, gmr, by ="DATE") 
param_SMIC <-   arrange(param_SMIC, DATE) |>
  select(-gmr2) |>
  fill(gmr2h) |>
  mutate(gmr2h = ifelse(DATE > as.Date('2005-06-01') | DATE < as.Date('2000-01-01'), NA, gmr2h)) |>
  mutate(smic_gmr_h = ifelse(DATE <= as.Date('2005-06-01') & DATE >= as.Date('2000-01-01'), gmr2h, smic_h)) |>
  mutate(smic_mensuel_gmr = ifelse(DATE<as.Date('2000-01-01'), (smic_gmr_h*39*52)/12, (smic_gmr_h*35*52)/12)) |>
  mutate(cum_smic_mensuel_gmr_2015 = smic_mensuel_gmr/smic_mensuel_gmr[DATE == '2015-01-01']*100) |>
  arrange(DATE) |>
  filter(substr(DATE, 6, 7) == "03" | substr(DATE, 6, 7) =="06" | substr(DATE, 6, 7) =="09" | substr(DATE, 6, 7) =="12") |>
  mutate(trim_smic_gmr = (cum_smic_mensuel_gmr_2015/lag(cum_smic_mensuel_gmr_2015, 1))-1)

##Coup pouce SMIC
coup_pouce_smic <- SMB_SMPT |>
  select(DATE, trim_shbo, trim_shboe)
coup_pouce_smic2 <- left_join(coup_pouce_smic,param_SMIC,  by ="DATE") |>
  select(DATE, trim_shbo, trim_shboe, trim_smic_gmr) 
coup_pouce_smic2 <- mutate(coup_pouce_smic2, trim_shbo_shboe = ifelse(DATE >=as.Date('2013-01-01'), trim_shboe, trim_shbo)) |>
  select(-trim_shbo, -trim_shboe) |>
  mutate(half_pa_shbo = 0.5*trim_shbo_shboe) 
coup_pouce_smic3 <- left_join(coup_pouce_smic2, IPC, by = "DATE") |>
  select(DATE, trim_shbo_shboe,  trim_smic_gmr, cum_inf_2015) |>
  arrange(DATE) |>
  mutate(trim_ipc = (cum_inf_2015/lag(cum_inf_2015, 1))-1) |>
  mutate(half_pa_shbo = 0.5*(trim_shbo_shboe-trim_ipc)) |>
  mutate(half_pa_shbo = ifelse(half_pa_shbo<0, 0, half_pa_shbo)) |>
  mutate(trim_coup_pouce_smic = trim_smic_gmr-trim_ipc-half_pa_shbo) |>
  mutate(trim_coup_pouce_smic = ifelse(trim_coup_pouce_smic<0, 0, half_pa_shbo)) |>
  mutate(trim_coup_pouce_smic = round(trim_coup_pouce_smic,4)) |>
  select(-cum_inf_2015, -trim_ipc, -half_pa_shbo)
coup_pouce_smic <- coup_pouce_smic3
rm(coup_pouce_smic2, coup_pouce_smic3)

#Taux d'investissement des SF
file_tx_inv<- "data/Data_FRANCE/tx_investissement.xls"
cnt_fbcf <- read_excel(file_tx_inv, sheet = "Niveaux") |>
  rename ('DATE' = '...1') |>
  select(DATE,tx_investissement_ent, tx_inv_glob) 
cnt_fbcf$DATE <- str_replace(cnt_fbcf$DATE,"T1", "-03-01")
cnt_fbcf$DATE <- str_replace(cnt_fbcf$DATE,"T2", "-06-01")
cnt_fbcf$DATE <- str_replace(cnt_fbcf$DATE,"T3", "-09-01")
cnt_fbcf$DATE <- str_replace(cnt_fbcf$DATE,"T4", "-12-01")
cnt_fbcf$DATE <- as.Date(cnt_fbcf$DATE)
cnt_fbcf <-  arrange(cnt_fbcf, DATE) |>
  mutate(yearly_tx_inv_ent = (tx_investissement_ent/lag(tx_investissement_ent,4)-1))

###Déflateur du prix des imports
file_import <- "data/Data_FRANCE/deflateur_import.xlsx" 
deflateur_import <- read_excel(file_import) |>
  select(DATE, deflateur_import, deflateur_import_energie, deflateur_import_he)
deflateur_import$DATE <- str_replace(deflateur_import$DATE,"T1", "-03-01")
deflateur_import$DATE <- str_replace(deflateur_import$DATE,"T2", "-06-01")
deflateur_import$DATE <- str_replace(deflateur_import$DATE,"T3", "-09-01")
deflateur_import$DATE <- str_replace(deflateur_import$DATE,"T4", "-12-01")
deflateur_import$DATE <- as.Date(deflateur_import$DATE)
deflateur_import <-  arrange(deflateur_import, DATE) |>
  mutate(yearly_deflateur_import = (deflateur_import/lag(deflateur_import,4)-1)) |>
  mutate(yearly_deflateur_import_energie = (deflateur_import_energie/lag(deflateur_import_energie, 4)-1)) |>
  mutate(yearly_deflateur_import_he = (deflateur_import_he/lag(deflateur_import_he, 4)-1)) 

###Prix du brent
brent_euro<-get_insee_idbank('010002078') |>
  select(DATE, OBS_VALUE) |>
  rename("brent_euro" = "OBS_VALUE") |>
  arrange(DATE) |>
  mutate(gliss_brent_euro = brent_euro/lag(brent_euro, 12)-1)

###Déflateur de la conso totale
file_conso_tot <- "data/Data_FRANCE/deflateur_conso_tot.xlsx" 
deflateur_conso_tot <- read_excel(file_conso_tot) |>
  mutate(DATE = as.Date(DATE))

