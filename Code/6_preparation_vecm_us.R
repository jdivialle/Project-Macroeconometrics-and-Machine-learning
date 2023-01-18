####On importe le CSV base US
file_base_us <- "data/Data_USA/base_usa.csv"
base_usa <-read.csv(file = file_base_us, sep = ",")

file_base_us_prod <- "data/Data_USA/prod_travail_us.xls"
base_prod_usa <-read_excel(file_base_us_prod) |>
  mutate(DATE = as.character(DATE))

base_usa <- left_join(base_usa, base_prod_usa, by = "DATE")
base_usa <- base_usa |>
  mutate(DATE = as.Date(DATE)) |>
  filter(DATE>=as.Date('1995-01-01') & DATE <= as.Date('2019-12-01')) |>
  rename(chom_bit = unemp_rate, log_chom_bit = log_unemp_rate, deflateur_va = prix_VA, deflateur_conso = deflat_prix_conso, tuc = capacite_prod, deflateur_import = deflat_import, smpt = SMPT) |>
  mutate(deflateur_va = deflateur_va*100) |>
  mutate(smpt = smpt/smpt[DATE == "2012-01-01"]*100) 

###ECM DGT US 
ecm_dgt_us <- select(base_usa, DATE, smpt, chom_bit, deflateur_conso, prod_travail)
save(ecm_dgt_us, file = file.path("data/Data_USA", "ecm_dgt_us.RData"))

###On fait des tests de racines unitaires (séries dans la relation de LT intégrées d'ordre 1)
chom_us_ts <- ts(base_usa$chom_bit, start=c(1995,1), end=c(2019, 4), frequency=4)
va_us_ts <- ts(base_usa$deflateur_va, start=c(1995,1), end=c(2019, 4), frequency=4)
ipc_us_ts<- ts(base_usa$deflateur_conso, start=c(1995,1), end=c(2019, 4), frequency=4)
tuc_us_ts <- ts(base_usa$tuc, start=c(1995,1), end=c(2019, 4), frequency=4)
import_us_ts  <- ts(base_usa$deflateur_import, start=c(1995,1), end=c(2019, 4), frequency=4)
smpt_us_ts <- ts(base_usa$smpt, start=c(1995,1), end=c(2019, 4), frequency=4)

###Test de racines unitaires : tout est d'ordre 1 sauf l'indicatrice
adf.test(chom_us_ts)
adf.test(diff(chom_us_ts,1))

adf.test(va_us_ts)
adf.test(diff(va_us_ts,1))

adf.test(ipc_us_ts)
adf.test(diff(ipc_us_ts,1))

adf.test(tuc_us_ts)
adf.test(diff(tuc_us_ts,1))

adf.test(smpt_us_ts)
adf.test(diff(smpt_us_ts,1)) 

adf.test(import_us_ts)
adf.test(diff(import_us_ts,1))

order_integration(smpt_us_ts, max_order = 3, method = "adf", level = 0.05,
                  plot_orders = TRUE)
order_integration(ipc_us_ts, max_order = 3, method = "adf", level = 0.05,
                  plot_orders = TRUE)


###Toutes les séries sont I(1)  => pas besoin de différencier pour le VECM 

vecm_us <- select(base_usa, DATE, chom_bit, log_chom_bit, deflateur_conso, deflateur_va, tuc, deflateur_import, smpt)
library(haven)
write_dta(
  vecm_us,
  file.path("data/VECM/vecm_usa_data.dta"),
  version = 14,
  label = attr(data, "label"))


vecm_taux_us <- select(base_usa, DATE, chom_bit, log_chom_bit, taux_deflat_prix_conso, taux_prix_VA, tuc, taux_deflat_import, taux_SMPT) |>
  rename(smpt = taux_SMPT, deflateur_conso=taux_deflat_prix_conso, deflateur_va = taux_prix_VA, deflateur_import = taux_deflat_import) |>
  select(DATE, chom_bit, log_chom_bit, deflateur_conso, deflateur_va, tuc, deflateur_import, smpt)

smpt_taux_us_ts <- ts(vecm_taux_us$smpt, start=c(1995,1), end=c(2019, 4), frequency=4)
ipc_taux_us_ts<- ts(vecm_taux_us$deflateur_conso, start=c(1995,1), end=c(2019, 4), frequency=4)

order_integration(smpt_taux_us_ts, max_order = 3, method = "adf", level = 0.05,
                  plot_orders = TRUE)
order_integration(ipc_taux_us_ts, max_order = 3, method = "adf", level = 0.05,
                  plot_orders = TRUE)

library(haven)
write_dta(
  vecm_us,
  file.path("data/VECM/vecm_taux_usa_data.dta"),
  version = 14,
  label = attr(data, "label"))



