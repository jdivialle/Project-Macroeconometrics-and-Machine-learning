                                                             
setwd("C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/wage_equation")

#On restreint sur la fenetre 1975-2019
wage_estimation_long2 <- filter(wage_estimation_long, DATE !=as.Date("1975-03-01") & DATE <=as.Date("2019-12-01"))

#On selectionne les variables pertinentes
wage_estimation_75_19 <- wage_estimation_long2 |>
  select(DATE, yearly_salh_marng, tx_yearly_ipc, yearly_def_conso_men, chomage_BIT,yearly_prod_travail_marng, tx_smic_reel, tx_smic_nom, tx_smic_gmr_nom) |>
  mutate(dum82 = ifelse(DATE <=as.Date('1982-06-01'), 1,0))

#On rajoute la durée effective de travail dans le secteur marchand non agricole
duree_eff_marng <- cntbis |>
  select(DATE, eff_travail_globmarng)|>
  arrange(DATE) |>
  #mutate(yearly_eff_travail_marng = (eff_travail_globmarng/lag(eff_travail_globmarng, 4))-1) |>
  filter( DATE >as.Date("1975-03-01") & DATE <=as.Date("2019-12-01"))

wage_estimation_75_19 <- wage_estimation_long2 |>
  select(DATE, yearly_salh_marng, tx_yearly_ipc, yearly_def_conso_men, chomage_BIT,yearly_prod_travail_marng, tx_smic_reel, tx_smic_nom, tx_smic_gmr_nom) 
wage_estimation_75_19 <- left_join(wage_estimation_75_19,duree_eff_marng, by = "DATE")

####Passage sur STATA => enregistrement en format stata compatible
library(haven)
write_dta(
  wage_estimation_75_19,
  file.path("ecm_ofce.dta"),
  version = 14,
  label = attr(data, "label"))

