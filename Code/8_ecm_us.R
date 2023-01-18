load(file = file.path("data/Data_USA", "ecm_dgt_us.RData"))
###Productivité par tête (secteur marchand non agricole)
##Construction des variables pour estimer l'équation ECM du modèle Opale
ecm_dgt_us <- ecm_dgt_us |>
  arrange(DATE) |>
  filter(DATE <=as.Date('2022-06-01')) |>
  mutate(log_smpt = log(smpt)) |>
  mutate(log_prod_tete = log(prod_travail)) |>
  mutate(log_prod_travail = log(prod_travail)) |>
  mutate(moy_mobile_prod_tete = (lag(log_prod_tete,1)+lag(log_prod_tete,2)+lag(log_prod_tete,3)+lag(log_prod_tete,4))/4) |> 
  mutate(lag_moy_mobile_prod_tete = lag(moy_mobile_prod_tete, 1)) |>
  mutate(moy_mobile_prod_travail = (lag(log_prod_travail,1)+lag(log_prod_travail ,2)+lag(log_prod_travail,3)+lag(log_prod_travail,4))/4) |>
  mutate(lag_moy_mobile_prod_travail = lag(moy_mobile_prod_travail, 1)) 

#On charge l'IPC CVS en niveau (on le réindexe en base 100 en décembre 2019)
ipc_cvs <- select(ecm_dgt_us, DATE, deflateur_conso) |>
  rename(ipc_cvs = deflateur_conso) |>
  arrange(DATE) |>
  mutate(DATE = as.Date(DATE)) |>
  filter(DATE <=as.Date('2022-06-01') & substr(DATE, 6, 7) == "01" |substr(DATE, 6, 7) == "04"|substr(DATE, 6, 7) == "07"|substr(DATE, 6, 7) == "10") |>
  mutate(ipc_cvs = ipc_cvs/ipc_cvs[DATE=='2019-10-01']*100) |>
  mutate(log_ipc_cvs = log(ipc_cvs))

ecm_dgt_us <- left_join(ecm_dgt_us, ipc_cvs, by = "DATE")
ecm_dgt_us <- ecm_dgt_us |>
  relocate(smpt, log_smpt, ipc_cvs, log_ipc_cvs, chom_bit, .after = prod_travail)

#On estime sur la fenêtre T1-1995 à T4-2018
ecm_dgt_us <- ecm_dgt_us |>
  arrange(DATE) |>
  mutate(lag_log_smpt = lag(log_smpt,1)) |>
  mutate(lag_log_ipc_cvs = lag(log_ipc_cvs, 1)) |>
  mutate(ind_t1 = ifelse(substr(DATE, 6, 7) == "01", 1, 0)) |>
  mutate(ind_t299_t400 = ifelse(DATE >=as.Date('1999-07-01') & DATE <= as.Date('2000-10-01'), 1, 0)) |>
  mutate(ind_2008t4_2010t4 = ifelse(DATE >=as.Date('2008-12-01') & DATE <= as.Date('2010-12-01'), 1, 0)) |>
  mutate(y_long_term = lag_log_smpt - lag_log_ipc_cvs - moy_mobile_prod_travail) |>
  filter(DATE >= as.Date('1995-01-01') & DATE <= as.Date('2018-12-01'))

###On sélectionne les seules variables pertinentes
ecm_dgt_us_2<- select(ecm_dgt_us, DATE, log_smpt, log_ipc_cvs, lag_log_smpt , lag_log_ipc_cvs, chom_bit, log_prod_tete, moy_mobile_prod_tete, ind_2008t4_2010t4, ind_t299_t400, ind_t1, y_long_term)

###On fait des tests de racines unitaires (séries dans la relation de LT intégrées d'ordre 1)
y_lt_ts <- ts(ecm_dgt_us_2$y_long_term, start=c(1995,1), end=c(2018, 4), frequency=4)
ind_ts <- ts(ecm_dgt_us_2$ind_2008t4_2010t4, start=c(1995,1), end=c(2018, 4), frequency=4)
smpt_ts <- ts(ecm_dgt_us_2$log_smpt, start=c(1995,1), end=c(2018, 4), frequency=4)
ipc_ts <- ts(ecm_dgt_us_2$log_ipc_cvs, start=c(1995,1), end=c(2018, 4), frequency=4)
prod_ts <- ts(ecm_dgt_us$moy_mobile_prod_tete, start=c(1995,1), end=c(2018, 4), frequency=4)

###Test de racines unitaires : tout est d'ordre 1 sauf l'indicatrice
adf.test(y_lt_ts)
adf.test(diff(y_lt_ts,1))

adf.test(ind_ts)


adf.test(smpt_ts)
adf.test(diff(y_lt_ts,1))

adf.test(ipc_ts)
adf.test(diff(y_lt_ts,1))

adf.test(prod_ts)
adf.test(diff(prod_ts, 1))

###On estime la relation de LT de l'ECM 
lt_lm_relation <- lm(log_smpt~lag_log_ipc_cvs + moy_mobile_prod_travail+ind_2008t4_2010t4, data = ecm_dgt_us)
lt_lm_relation <- lm(y_long_term~ind_t299_t400+ind_2008t4_2010t4, data = ecm_dgt_us_2)
tab_model(lt_lm_relation)

###On extrait le résidu puis on l'incorpore dans la base 2 avec un retard 
error_lt_relation <- residuals(lt_lm_relation)
error_lt_relation<- as.data.frame(error_lt_relation) 

error_lt_ts <- ts(error_lt_relation$error_lt_relation, start=c(1995,1), end=c(2018, 4), frequency=4)
order_integration(error_lt_ts, max_order = 3, method = "adf", level = 0.05,
                  plot_orders = TRUE)

error_lt_relation$time <- seq(1, 96)
error_lt_relation <- arrange(error_lt_relation, time) |>
  mutate(error_lt_relation, error_lag_lt= lag(error_lt_relation, 1)) |>
  filter(time !=1)
ecm_dgt_us_2$time <- seq(1,96) 
ecm_dgt_us_2 <- ecm_dgt_us_2 |>
  arrange(DATE) |>
  mutate(dif_ipc = log_ipc_cvs/lag(log_ipc_cvs, 1)) |>
  mutate(dif_lag_ipc = lag_log_ipc_cvs-lag(lag_log_ipc_cvs, 1)) |>
  mutate(dif_prod = log_prod_tete - lag(log_prod_tete,1)) |>
  mutate(dif_smpt = log_smpt - lag(log_smpt, 1)) |>
  mutate(dif_chom = chom_bit/100 - lag(chom_bit, 1)/100)

base_estim_ecm <- left_join(error_lt_relation, ecm_dgt_us_2, by ="time")

###On estime l'ECM complet (en différence + en incorportant la relation de LT via l'erreur retardée d'une période de la relation estimée précédemment)
ecm_estimation <- lm(dif_smpt~dif_ipc+dif_lag_ipc+dif_chom + dif_prod+ ind_t299_t400+ ind_2008t4_2010t4+error_lag_lt, data = base_estim_ecm)
tab_model(ecm_estimation)

#On extrait la prédiction de dif smpt (1e difference du log du SMPT)
predict_smpt<- predict(ecm_estimation)
predict_smpt<- as.data.frame(predict_smpt)
predict_smpt$time <-seq(2,96)
base_estim_ecm <- left_join(base_estim_ecm, predict_smpt, by ="time")
base_time1 <- select(ecm_dgt_us_2, time, log_smpt) |>
  filter(time ==1) |>
  mutate(time = 2) |>
  rename(base_smpt_time1 = log_smpt)
base_estim_ecm <- left_join(base_estim_ecm, base_time1, by = "time")
base_estim_ecm <- base_estim_ecm |>
  mutate(predict_level_smpt = ifelse(time ==2,base_smpt_time1 + predict_smpt, NA))

#On créé une boucle pour retrouver la valeur prédidte de SMPT (valeur de départ du SMPT + cumul des dif smpt)
i <-2
while(i <=nrow(base_estim_ecm)){
  base_estim_ecm[i, "predict_level_smpt"] <- base_estim_ecm[i-1, "predict_level_smpt"]+base_estim_ecm[i,"predict_smpt"]
  i <- i+1
}

#On repasse à l'exponentielle et on présente les résultats en glissement annuel et en variation trimestrielle
base_estim_ecm <- base_estim_ecm |>
  mutate(predict_level_smpt =exp(predict_level_smpt)) |>
  mutate(real_level_smpt = exp(log_smpt)) |>
  mutate(gliss_year_real_smpt = (real_level_smpt/lag(real_level_smpt, 4)-1)*100) |>
  mutate(gliss_year_pred_smpt = (predict_level_smpt/lag(predict_level_smpt, 4)-1)*100) 

###graphiques
#En niveau 
level_ecm_us<- ggplot(aes(x = DATE), data = base_estim_ecm) + geom_line(aes(y = predict_level_smpt, color = "Prediction")) +
  geom_point(aes(y = predict_level_smpt, color = "Prediction"))+
  geom_line(aes(y = real_level_smpt, color = "Real"))+
  geom_point(aes(y = real_level_smpt, color = "Real")) + theme_bw() + labs(x = "Year", y = "Index based 100 (2019-Q4)", title = "Average level of monthly wages per capita (real and prediction)") +
  scale_color_manual(values = c("Prediction"="red", 
                                "Real"= "blue")) + 
  theme(legend.position="bottom") 

pdf('graphs/level_ecm_fr.pdf')
level_ecm_us
dev.off()
#En glissement annuel 
gliss_ecm_us <- ggplot(aes(x = DATE), data = base_estim_ecm) + geom_line(aes(y = gliss_year_pred_smpt, color = "Prediction")) +
  geom_point(aes(y = gliss_year_pred_smpt, color = "Prediction"))+
  geom_line(aes(y = gliss_year_real_smpt, color = "Real"))+
  geom_point(aes(y = gliss_year_real_smpt, color = "Real")) + theme_bw() + labs(x = "Year", y = "Year-to-year growth rate (%)", title = "Average monthly wages per capita (real and prediction)") + 
  scale_color_manual(values = c("Prediction"="red", 
                                "Real"= "blue")) + 
  theme(legend.position="bottom") 

pdf('graphs/gliss_ecm_fr.pdf')
gliss_ecm_us
dev.off()

###table 

library(kableExtra)
library(modelsummary)

mod_smpt <-list(
  "(ECM du SMPT)" = ecm_estimation
)


cm_smpt <- c('(Intercept)'='Intercept',
             'dif_ipc' = 'PCI',
             'dif_lag_ipc'='1 Lagged PCI',
             'retard1_tx_def_conso_men'='Unemployment',
             'dif_prod'='Productivity',
             'ind_2008t4_2010t4'='2008Q4-2010Q4 period',
             'error_lag_lt'='LT relation')
cap_smpt <- "ECM prediction of Average monthly wage per capita (French SMPT)"


modelsummary(mod_smpt,  
             statistic = '{std.error} ({p.value})', 
             output = "latex",
             coef_map = cm_smpt, stars = TRUE, 
             title = cap_smpt, gof_omit = 'IC') |>
  add_header_above(c(" ", "Dependent variable : SMPT" = 1))


# ###passage sous stata
# ###passage sous stata
# library(haven)
# write_dta(
#   ecm_dgt_us_estimation,
#   file.path("ecm_ofce.dta"),
#   version = 14,
#   label = attr(data, "label"))
# 
# ecm_dgt_us_prediction <- ecm_dgt_us_2[ecm_dgt_us_2$DATE>'2012-12-01' & ecm_dgt_us_2$DATE<='2019-12-01',]
# xtr <- ecm_dgt_us_estimation[c('log_ipc_cvs', 'lag_log_ipc_cvs', 'log_prod_tete')]
# xeq <- ecm_dgt_us_estimation[c('y_long_term', 'moy_mobile_prod_tete', 'ind_2008t4_2010t4')]
# 
# model1 <- ecm(ecm_dgt_us_estimation$log_smpt, xeq, xtr,  lags = 1, weights = NULL, includeIntercept=TRUE)
# tab_model(model1)
# summary(model1)
# 
# ecm_dgt_us_prediction$model1Pred <- ecmpredict(model1, ecm_dgt_us_prediction, ecm_dgt_us_prediction$log_smpt[1])
# ecm_dgt_us_prediction <- mutate(ecm_dgt_us_prediction, pred_smpt = exp(model1Pred)) |>
#   mutate(real_smpt = exp(log_smpt)) |>
#   arrange(DATE) |>
#   mutate(gliss_annuel_real_smpt = (real_smpt/lag(real_smpt, 4)-1)*100) |>
#   mutate(gliss_annuel_pred_smpt = (pred_smpt/lag(pred_smpt, 4)-1)*100) 
# 
# 
# ggplot(aes(x = DATE), data = ecm_dgt_us_prediction) + geom_line(aes(y = real_smpt), color = "red") + 
#   geom_point(aes(y = real_smpt), color = "red") +
#   geom_line(aes(y = pred_smpt), color = "blue") + 
#   geom_point(aes(y = pred_smpt), color = "blue") + theme_bw() 
# 
# ggplot(aes(x = DATE), data = ecm_dgt_us_prediction) + geom_line(aes(y = gliss_annuel_real_smpt), color = "red") + 
#   geom_point(aes(y = gliss_annuel_real_smpt), color = "red") +
#   geom_line(aes(y = gliss_annuel_pred_smpt), color = "blue") + 
#   geom_point(aes(y = gliss_annuel_pred_smpt), color = "blue") + theme_bw()