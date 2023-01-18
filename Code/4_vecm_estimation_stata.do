clear
use "C:\Users\gasto\Documents\GitHub\projet-ml-macro\data\VECM\vecm_heyer.dta"
// youtube https://www.youtube.com/watch?v=syP7JiUkMng&ab_channel=CrunchEconometrix Unrestricted var only if no cointegration
gsort DATE 
gen time  = _n
tsset time
//drop DATE 
//drop if time>87
// Nb de retards par le critere AIC / BIC du VAR select sur un VAR I(1) : c'est pq chom et TUC en niveau alors que smpt et ipc en taux 
varsoc yearly_smpt
varsoc yearly_def_conso_tot 
varsoc yearly_def_conso_men
varsoc yearly_deflateur_va
varsoc yearly_deflateur_import
varsoc log_chom_bit
varsoc log_tuc
gen log_tuc2 = log(TUC)

// Puis trouver les relations de cointégration 
// tin(88,175) veut dire estimation sur la période 1998-2019
vecrank yearly_smpt yearly_def_conso_tot  yearly_deflateur_va yearly_deflateur_import log_chom_bit log_tuc if tin(88,175), trend(constant) lags(5) 
vecrank yearly_smpt yearly_def_conso_tot  yearly_deflateur_va yearly_deflateur_import log_chom_bit log_tuc if tin(88,175), trend(rconstant) lags(5) 

// Définition des contraintes

constraint define 1 [_ce1]yearly_smpt = 1
constraint define 2 [_ce1]yearly_deflateur_va = 0
constraint define 3 [_ce1]yearly_deflateur_import  = 0
constraint define 4 [_ce2]yearly_def_conso_tot = 0
constraint define 5 [_ce2]yearly_deflateur_import   = 0
constraint define 6 [_ce2]yearly_deflateur_va    = 1
constraint define 7 [_ce3]yearly_def_conso_tot     = 1
constraint define 8 [_ce3]log_chom_bit     = 0
constraint define 9 [_ce3]log_tuc    = 0
constraint define 10 [_ce3]yearly_smpt    = 0
constraint define 11 [_ce1]log_tuc= 0 
constraint define 12 [_ce2]log_chom_bit = 0
constraint define 13 [_ce3]_cons = 0 
constraint define 14 [_ce3]yearly_deflateur_va + [_ce3]yearly_deflateur_import = -1
constraint define 15 [_ce2]yearly_smpt = -1

// Test de contraintes supplémentaires mais elles sont pas acceptées
constraint define 21 [D_log_tuc]L._ce1  = 0
constraint define 22 [D_log_chom_bit]L._ce1   = 0
constraint define 23 [D_log_tuc]L._ce2  = 0
constraint define 24 [D_log_chom_bit]L._ce2   = 0
constraint define 25 [D_log_tuc]L._ce3  = 0
constraint define 26 [D_log_chom_bit]L._ce3  = 0
constraint define 27 [D_yearly_deflateur_import]L._ce1   = 0
constraint define 28 [D_yearly_deflateur_import]L._ce2  = 0
constraint define 29 [D_yearly_deflateur_import]L._ce3  = 0

constraint define 20 [D_yearly_deflateur_va]L._ce1 = 0
constraint define 21 [D_yearly_deflateur_import]L._ce1  = 0
constraint define 22 [D_yearly_def_conso_tot]L._ce2   = 0
constraint define 23 [D_yearly_deflateur_import]L._ce2   = 0
constraint define 24 [D_log_chom_bit]L._ce3   = 0
constraint define 25 [D_log_tuc]L._ce3  = 0
constraint define 26 [D_yearly_smpt]L._ce3  = 0
constraint define 27 [D_log_tuc]L._ce1= 0 
constraint define 28 [D_log_chom_bit]L._ce2= 0
constraint define 29 [D_yearly_smpt]L._ce2 = 0

// Premiere estimation du VECM avec les contraintes 1 à 13 (on reproduit le modele de l'article)
vec yearly_smpt yearly_def_conso_tot  yearly_deflateur_va yearly_deflateur_import log_chom_bit log_tuc if tin(88,175), trend(rconstant) rank(3) lags(5) bconstraints(1/13)
// en rajoutant la contrainte 14 et 15 : on force la décomposition unitaire du prix de conso = alpha*prix imports + beta*prix VA et indexation unitaire des prix par rapport aux salaires
vec yearly_smpt yearly_def_conso_tot yearly_deflateur_va yearly_deflateur_import log_chom_bit log_tuc if tin(88,175), trend(rconstant) rank(3) lags(5) bconstraints(1/15)  noetable

//On prédit les résidus de chaque équation sur la période 1998-2019
predict residuals_wage if e(sample), residuals equation(#1) 
predict residuals_def_conso if e(sample), residuals equation(#2)
predict residuals_pva if e(sample), residuals equation(#3)

// On conserve que la période 1998-2019
keep if time>=88
gen dum_98_02 = 1 if time>=88 & time<=104
replace dum_98_02 = 0 if time>104
// LM regression de la relation de court terme des salaires (1e equation du système)
reg d.yearly_smpt l.d.yearly_def_conso_tot l.d.yearly_smpt l(1/3).d.yearly_deflateur_va d.yearly_deflateur_import l.residuals_wage

// IV Regression de l'équation de salaire 
ivreg d.yearly_smpt l.d.yearly_smpt l(1/3).d.yearly_deflateur_va d.yearly_deflateur_import l.residuals_wage  (d.yearly_def_conso_tot = d.l(1/3).yearly_def_conso_tot d.log_chom_bit d.log_tuc l(1/3).d.yearly_smpt l(1/3).d.yearly_deflateur_va l(1/3).d.yearly_deflateur_import dum_98_02)
// On prédit la relation de CT des salaires 
predict ct_pred_smpt
// Plot de la prédiction de l'évolution des salaires (relation de CT) VS observé
twoway connected ct_pred_smpt d.yearly_smpt DATE, ytitle("Différence de croissance du SMPT") xtitle("Année") graphregion(lcolor(white) fcolor(white)) title("Equation structurelle de court terme des salaires") xlabel(,labsize(vsmall))

gen res_wage = residuals_wage[_n-1]
gen res_conso = residuals_def_conso[_n-1]
gen res_pva = residuals_pva[_n-1]
drop residuals_wage residuals_pva residuals_def_conso
save "vecm_heyer_check.dta", replace
