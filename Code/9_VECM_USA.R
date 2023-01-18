setwd("C:/Desktop/GitHub/projet-ml-macro/Data_USA")

Unemp_rate = read.csv("Unemployment Rate.csv")
colnames(Unemp_rate)[2] = "unemp_rate"

Prix_VA = read.csv("Price per unit of real gross value added of nonfinancial corporate business.csv")
colnames(Prix_VA)[2] = "prix_VA"

Base = merge(Unemp_rate, Prix_VA, by="DATE")

Deflat_prix_conso = read.csv("Personal consumption expenditures (implicit price deflator).csv")
colnames(Deflat_prix_conso)[2] = "deflat_prix_conso"
Base = merge(Base, Deflat_prix_conso, by="DATE")

Capacite_prod = read.csv("Capacity Utilization Total Index.csv")
colnames(Capacite_prod)[2] = "capacite_prod"
Base = merge(Base, Capacite_prod, by="DATE")

Deflat_import = read.csv("Imports of goods and services (implicit price deflator).csv")
colnames(Deflat_import)[2] = "deflat_import"
Base = merge(Base, Deflat_import, by="DATE")


Salaire_total = read.csv("United States - Wages, total economy - Quarterly.csv")
colnames(Salaire_total)[2] = "salaire_total"
Salaire_total = Salaire_total[(Salaire_total$period>"1970") & (Salaire_total$period<"2022-Q4"),]
colnames(Salaire_total)[1] = "DATE"
Salaire_total$DATE = Base$DATE

All_employees = read.csv("All Employees, Total Nonfarm.csv")
colnames(All_employees)[2] = "all_employees"
All_employees$all_employees = 1000*All_employees$all_employees

nbre_trimestre = nrow(All_employees)/3
DATE = NULL
nb_employes = NULL
for (i in 1:nbre_trimestre)
{
  DATE = c(DATE, All_employees$DATE[3*i-2])
  nb_employes = c(nb_employes, (All_employees$all_employees[3*i-2] + All_employees$all_employees[3*i-1] + All_employees$all_employees[3*i])/3)
}
Nombre_employes = data.frame(cbind(DATE, nb_employes))
Nombre_employes = Nombre_employes[Nombre_employes$DATE %in% Salaire_total$DATE,]
Salaire_total$SMPT = Salaire_total$salaire_total/as.numeric(Nombre_employes$nb_employes)

SMPT = select(Salaire_total, DATE, SMPT)


Base = merge(Base, SMPT, by="DATE")


Base$log_unemp_rate = log(Base$unemp_rate)
Base$log_capacite_prod = log(Base$capacite_prod)
Base$taux_prix_VA = NULL
Base$taux_deflat_prix_conso = NULL
Base$taux_deflat_import = NULL
Base$taux_SMPT = NULL
for (i in 1:nrow(Base))
{
  if (i<5)
  {
    Base$taux_prix_VA[i] = NA
    Base$taux_deflat_prix_conso[i] = NA
    Base$taux_deflat_import[i] = NA
    Base$taux_SMPT[i] = NA
  }
  else
  {
    Base$taux_prix_VA[i] = ((Base$prix_VA[i]/Base$prix_VA[i-4])-1)*100
    Base$taux_deflat_prix_conso[i] = ((Base$deflat_prix_conso[i]/Base$deflat_prix_conso[i-4])-1)*100
    Base$taux_deflat_import[i] = ((Base$deflat_import[i]/Base$deflat_import[i-4])-1)*100
    Base$taux_SMPT[i] = ((Base$SMPT[i]/Base$SMPT[i-4])-1)*100
  }
}

write.csv(Base, "C:/Desktop/GitHub/projet-ml-macro\\Base_USA.csv")

