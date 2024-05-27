#Intalation des Packages necessaires 
install.packages("dplyr")
install.packages("VIM")
install.packages("naniar")
install.packages("tidyr")
install.packages("readr")
install.packages("tableone")
install.packages("patchwork")
install.packages("mctest")
install.packages("geepack")
install.packages("lme4")
install.packages("doBy")
install.packages("dplyr")
install.packages("miceadds")
install.packages("emmeans")
install.packages("ggplot2")

#Chargement des packages
library(VIM)
library (mice)
library(naniar)
library(tidyr)
library(readr)
library(tableone)
library(geepack)
library(patchwork)
library(geepack)
library(lme4)
library(mctest)
library(miceadds)
library(emmeans)
library(doBy)
library(ggplot2)
library(gridExtra)
#Repertoire
setwd("C:\\Users\\kossi\\OneDrive - Université Laval\\Examen_Retrospective")

#Importation de la base
dt = read.csv(("Support.csv"),na.strings=c("."))

#Presentation de la base
str(dt)
summary(dt)
# Convertir les variables nécessaires en facteurs
dt$SEX = factor(dt$SEX, levels = c(0, 1))
dt$PHASE = factor(dt$PHASE, levels = c(1, 2))
dt$CPR1 = factor(dt$CPR1, levels = c(1, 2, 3, "D"))
dt$CPR2 = factor(dt$CPR2, levels = c(1, 2, 3, "D"))

# Statistique après conversion
str(dt)
summary(dt)

#Statistiques descriptives
mean(apply(dt,1,anyNA))*100
#70.38

# Pourcentage de valeurs manquantes par variable
missing = colMeans(is.na(dt)) * 100
missing_data = data.frame(
  Variable = names(missing),
  Percentage_Missing = missing
)
missing_data 

#Statistiques descriptives avant imputation

cov_names=c("AGE", "SEX","SURV2MD1", "DEP1","DEP2",    
            "ADL1", "ADL2","CPR1" ,"CPR2","QOL1" , "QOL2" )
print(CreateTableOne(vars = cov_names, strata = "PHASE", data = dt, includeNA = TRUE), test = FALSE, smd = TRUE);

print(CreateTableOne(vars = cov_names, data = dt, includeNA = TRUE), test = FALSE, smd = TRUE);

#Patron des données manquantes
dev.new()
aggr(dt, numbers = TRUE, prop = c(TRUE, FALSE))

#Postulas sur les données manquantes
attach(dt)
dev.new()
par(mfrow = c(2,5))
boxplot(DEP1 ~ is.na(CPR1), main = "DEP1",dt)
boxplot(ADL1 ~ is.na(CPR1), main = "ADL1",dt)
boxplot(QOL1 ~ is.na(CPR1), main = "QOL1",dt)
boxplot(AGE ~ is.na(CPR1), main = "AGE",dt)
boxplot(SURV2MD1 ~ is.na(CPR1), main = "SURV2MD1",dt)
boxplot(DEP2 ~ is.na(CPR2), main = "DEP2",dt)
boxplot(ADL2 ~ is.na(CPR2), main = "ADL2",dt)
boxplot(QOL2 ~ is.na(CPR2), main = "QOL2",dt)
boxplot(AGE ~ is.na(CPR2), main = "AGE",dt)
boxplot(SURV2MD1 ~ is.na(CPR2), main = "SURV2MD1",dt)

# t.tests
t.test(dt$DEP1[is.na(dt$CPR1)], dt$DEP1[!is.na(dt$CPR1)]) 
t.test(dt$ADL1[is.na(dt$CPR1)], dt$ADL1[!is.na(dt$CPR1)]) 
t.test(dt$QOL1[is.na(dt$CPR1)], dt$QOL1[!is.na(dt$CPR1)])
t.test(dt$DEP1[is.na(dt$ADL1)], dt$DEP1[!is.na(dt$ADL1)]) 
t.test(dt$DEP1[is.na(dt$QOL1)], dt$DEP1[!is.na(dt$QOL1)]) 
t.test(dt$ADL1[is.na(dt$QOL1)], dt$ADL1[!is.na(dt$QOL1)]) 
t.test(dt$DEP2[is.na(dt$CPR2)], dt$DEP2[!is.na(dt$CPR2)]) 
t.test(dt$ADL2[is.na(dt$CPR2)], dt$ADL2[!is.na(dt$CPR2)]) 
t.test(dt$QOL2[is.na(dt$CPR2)], dt$QOL2[!is.na(dt$CPR2)]) 
t.test(dt$DEP2[is.na(dt$ADL2)], dt$DEP2[!is.na(dt$ADL2)]) 
t.test(dt$DEP2[is.na(dt$QOL2)], dt$DEP2[!is.na(dt$QOL2)]) 
t.test(dt$ADL2[is.na(dt$QOL2)], dt$ADL2[!is.na(dt$QOL2)]) 

# Créer un data frame avec les résultats des tests t
result_t_test = data.frame(
  Test = c("DEP1 vs. non-NA(CPR1)", "ADL1 vs. non-NA(CPR1)", "QOL1 vs. non-NA(CPR1)",
           "DEP1 vs. non-NA(ADL1)", "DEP1 vs. non-NA(QOL1)", "ADL1 vs. non-NA(QOL1)",
           "DEP2 vs. non-NA(CPR2)", "ADL2 vs. non-NA(CPR2)", "QOL2 vs. non-NA(CPR2)",
           "DEP2 vs. non-NA(ADL2)", "DEP2 vs. non-NA(QOL2)", "ADL2 vs. non-NA(QOL2)"),
  t_value = c(3, 2, 0.9, 3, 2, 0.7, 0.4, 2, 2, 0.3, 0.7, 3),
  df = c(1435, 85, 45, 1344, 1583, 148, 31, 54, 44, 3, 29, 41),
  p_value = c(0.003, 0.06, 0.4, 0.007, 0.03, 0.5, 0.7, 0.05, 0.1, 0.8, 0.5, 0.003),
  mean_diff = c(0.783 - 0.676, 1.43 - 1.05, 3.64 - 3.48,
                0.778 - 0.679, 0.758 - 0.683, 1.17 - 1.06,
                0.677 - 0.604, 1.78 - 1.19, 3.39 - 3.13,
                0.719 - 0.605, 0.721 - 0.604, 2.37 - 1.18),
  conf_interval = c("[0.0365, 0.1783]", "[-0.0105, 0.7587]", "[-0.199, 0.514]",
                    "[0.0265, 0.1701]", "[0.00628, 0.14399]", "[-0.182, 0.404]",
                    "[-0.270, 0.417]", "[-0.00861, 1.18697]", "[-0.0666, 0.5833]",
                    "[-1.16, 1.39]", "[-0.244, 0.478]", "[0.414, 1.951]")
)
result_t_test

#Test Little de MCAR
out = mcar_test(dt) 
out

# Corrélation
#A l'admission
with(dt,prop.table(table(is.na(DEP1), CPR1), margin = 2))
with(dt,prop.table(table(is.na(ADL1), CPR1), margin = 2))
with(dt,prop.table(table(is.na(QOL1), CPR1), margin = 2))
with(dt,prop.table(table(is.na(DEP1), QOL1), margin = 2))
with(dt,prop.table(table(is.na(ADL1), QOL1), margin = 2))
with(dt,prop.table(table(is.na(DEP1), ADL1), margin = 2))
# Après deux mois
with(dt,prop.table(table(is.na(DEP2), CPR2), margin = 2))
with(dt,prop.table(table(is.na(ADL2), CPR2), margin = 2))
with(dt,prop.table(table(is.na(QOL2), CPR2), margin = 2))
with(dt,prop.table(table(is.na(DEP2), QOL2), margin = 2))
with(dt,prop.table(table(is.na(ADL2), QOL2), margin = 2))
with(dt,prop.table(table(is.na(DEP2), ADL2), margin = 2))


#Imputation multiple
dt_imp = mice(data = dt, m = 71, method = c("","","","","pmm","pmm","pmm","pmm","pmm","","polyreg","polyreg","pmm","pmm")
, seed = 15203,maxit=5)
imp_complet= complete(dt_imp,"long")
summary(imp_complet)

#diagnostic ( tendence des variable)
dev.new()
#plot(imp_complet)

#Transformation de la base de données en format long
dt_long = imp_complet %>%
  pivot_longer(
    cols = starts_with(c("CPR", "DEP", "ADL", "QOL")),
    names_to = c(".value", "TEMPS"),
    names_pattern = "(.*)(\\d+)"
  )

#Verification de NA
mean(apply(dt_long, 1, anyNA))*100

#Statistique descriptive après imputation et supression des modalité 3 et D 
dt_long_filt =dt_long %>%
  filter(!(CPR %in% c("3", "D")))

#Transformation de CPR et TEMPS
dt_long_filt$CPR =  as.numeric(dt_long_filt$CPR)
dt_long_filt$TEMPS =  as.numeric(dt_long_filt$TEMPS)
dt_long_filt$CPR = ifelse(dt_long_filt$CPR == 1, 1, 0)


#Tri par id 
dt_long_filt = dt_long_filt[order(dt_long_filt$PSEUDOID),]
dt_long_filt$CPR =factor(dt_long_filt$CPR)

#Statistiques descriptives sur la base imputée
summary(dt_long_filt)
cov_names=c("AGE", "SEX","SURV2MD1", "TEMPS","CPR",    
            "DEP", "ADL","QOL" )
print(CreateTableOne(vars = cov_names, data = dt_long_filt, includeNA = TRUE), test = FALSE, smd = TRUE);

# Define the list of continuous variables

p1=ggplot(dt_long_filt, aes(x = !!sym("AGE"))) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogramme", "AGE"), x = "AGE", y = "Frequency")

p2=ggplot(dt_long_filt, aes(x = !!sym("SURV2MD1"))) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = paste("Histogramme", "SURV2MD1"), x = "SURV2MD1", y = "Frequency")

p3=ggplot(dt_long_filt, aes(x = !!sym("ADL"))) +
  geom_histogram(binwidth = 1, fill = "magenta", color = "black", alpha = 0.7) +
  labs(title = paste("Histogramme", "ADL"), x = "ADL", y = "Frequency")

p4=ggplot(dt_long_filt, aes(x = !!sym("QOL"))) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black", alpha = 0.7) +
  labs(title = paste("Histogramme", "QOL"), x = "QOL", y = "Frequency")

#Variable dich

p5=ggplot(dt_long_filt, aes(x = !!sym("SEX"))) +
  geom_bar(fill = "black", color = "black", alpha = 0.7) +
  labs(title = paste("Bar Plot ", "SEX"), x = "SEX", y = "Count")

p6=ggplot(dt_long_filt, aes(x = !!sym("CPR"))) +
  geom_bar(fill = "yellow", color = "yellow", alpha = 0.7) +
  labs(title = paste("Bar Plot ", "CPR"), x = "CPR", y = "Count")

p7=ggplot(dt_long_filt, aes(x = !!sym("TEMPS"))) +
  geom_bar(fill = "pink", color = "pink", alpha = 0.7) +
  labs(title = paste("Bar Plot ", "TEMPS"), x = "TEMPS", y = "Count")

p8=ggplot(dt_long_filt, aes(x = !!sym("PHASE"))) +
  geom_bar(fill = "purple", color = "purple", alpha = 0.7) +
  labs(title = paste("Bar Plot de", "PHASE"), x = "PHASE", y = "Count")

dev.new()
grid.arrange(p5, p6, p7, p8, ncol = 2, nrow = 2)


### Selection de la structure de la matrice de correlation de travail
dt_long_filt$CPR_numeric = ifelse(dt_long_filt$CPR == "1", 1, 0)

#Determination des QIC 
matrice_exch = c()
matrice_ind = c()
matrice_ar1 = c()
QIC_exch= c()
QIC_ind = c()
QIC_ar1= c()

for (i in 1:71){
  dat = subset(dt_long_filt,.imp==i)
  matrice_exch= geeglm(CPR_numeric~DEP  + factor(TEMPS) +DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=dat, family=binomial,scale.fix=T, id=PSEUDOID, corstr= "exchangeable")
  QIC_exch =QIC(matrice_exch)
  matrice_ind= geeglm(CPR_numeric~DEP  + factor(TEMPS) +DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=dat, family=binomial,scale.fix=T, id=PSEUDOID, corstr= "independence")
  QIC_ind =QIC(matrice_ind)
  matrice_ar1= geeglm(CPR_numeric~DEP + factor(TEMPS) + DEP*factor(TEMPS)+ AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=dat, family=binomial,scale.fix=T, id=PSEUDOID, corstr= "ar1")
  QIC_ar1 =QIC(matrice_ar1)
  }

#Minimum des QIC
QIC_dat = data.frame(QIC_exch,QIC_ind,QIC_ar1)
QIC = c("QIC_exch", "QIC_ind", "QIC_ar1")
sapply(QIC_dat["QIC", ], min)
min(QIC_dat["QIC", QIC])
#Note : Le minimun est 11283.56 donc indépendance 

#Test d'interaction

#Modele sans interaction
Model_sans_interaction = list()
model1=  c()
for (i in 1:71){
  base = subset(dt_long_filt,.imp==i)
  model1 = geeglm(CPR_numeric~DEP + factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=base, family=binomial,scale.fix=T, id=PSEUDOID, corstr= "independence")
  Model_sans_interaction[[i]] = model1
}

#modele Avec intéraction
Model_interaction_temps = list()
model2  = c()
for (i in 1:71){
  base_inter = subset(dt_long_filt,.imp==i)
  model2 = geeglm(CPR_numeric~DEP + DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=base_inter, family=binomial,scale.fix=T, id=PSEUDOID, corstr= "independence")
  Model_interaction_temps[[i]] = model2 
}
# Comparaison des deux modèles
anova_results = list()
for (i in 1:71) {
  anova_result = anova(Model_sans_interaction[[i]], Model_interaction_temps[[i]], test = "Chisq")
  anova_results[[i]] = anova_result
}

#Pourcentage de modèle avec p< 0.05
all_p_values = sapply(anova_results, function(result) result$`P(>|Chi|)`)
resultats_significatifs = ifelse(all_p_values < 0.05, 1, 0)
modele_significatifs_interaction_temps = Model_interaction_temps[resultats_significatifs == 1]
modele_significatifs_sans_interaction = Model_sans_interaction[resultats_significatifs == 1]
mean(all_p_values< 0.05)*100
length(modele_significatifs_interaction_temps)


#Test de type III
J_tests = lapply(Model_interaction_temps, joint_tests)
J_stat=list()
for(i in 1:71){
J_stat[i] = J_tests[i][[1]]$p.value[9]}
J_stat

# Modele retenu avec contrast d'indépendance : 
#logit(CPR)= DEP + Time + DEP*Time + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL

#Tests des hypotheses sur 5 imputations Aléatoires 
samp = sample(1:71,5,replace=FALSE,set.seed(15203))
Model_hypothes = list()
base= c()
Base=list()
for (i in 1:5){
  base = subset(dt_long_filt,.imp==i)
  Model_hypothes[[i]] = geeglm(CPR_numeric~DEP + factor(TEMPS) + DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=base, family=binomial,scale.fix=T, id=PSEUDOID, corstr= "independence")
  Base[[i]]=base
}

#Selection d'une base d'imputation parmi les 5 pour les hypothèses 
model_hypoth =Model_hypothes[[5]] 
dt_hypoth=Base[[5]]

#Vérification des hypothèses pour le modèle avec interaction
## Relation residuelle
# Graphique des résidus
resid_pearson = resid(model_hypoth, type = "pearson")
dev.new()
par(mfrow=c(2,3))
plot(x = dt_hypoth$ADL, y = model_hypoth$resid)
lines(lowess(y = model_hypoth$resid, x = dt_hypoth$ADL), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple")

plot(x = dt_hypoth$AGE, y = model_hypoth$resid);
lines(lowess(y = model_hypoth$resid, x =dt_hypoth$AGE), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple");

plot(x = dt_hypoth$QOL, y = model_hypoth$resid);
lines(lowess(y = model_hypoth$resid, x =  dt_hypoth$QOL), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple")

plot(x = dt_hypoth$SURV2MD1, y = model_hypoth$resid);
lines(lowess(y = model_hypoth$resid, x = dt_hypoth$SURV2MD1), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple")

plot(x = dt_hypoth$CPR, y = model_hypoth$resid)
abline(h = 0, lwd = 2, col = "purple")

plot(fitted(model_hypoth), resid_pearson, ylab = "Residu pearson", xlab = "valeurs prédites")
abline(h = 0, col = "red", lty = 2)

# Résidus de score 
resid_score = resid(model_hypoth, type = "response")
dev.new()
par(mfrow=c(1,2))
qqnorm(resid_pearson)
qqline(resid_score)
plot(resid_pearson, pch = 20, col = "blue", main = "", 
     xlab = "Observations", ylab = "Résidus de score")
abline(h = 0, col = "red", lty = 2) 


#Vérifiaction de multicolinéarité
imcdiag(model_hypoth)
#Tous les VIF sont inférieur à 10 donc pas de multicolinarité

## Donnees abérantes
dev.new()
plot(resid_pearson)
text(x = resid_pearson, labels = row.names(resid_pearson), pos = 3, cex = 0.8)

#Estimation
modelF = c()
modelFinal =list()
for (i in 1:71) {
  dat = subset(dt_long_filt, .imp == i, )
  modelF = geeglm(CPR_numeric~DEP+ factor(TEMPS) +DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=dat, family=binomial(link = "logit"),scale.fix=T, id=PSEUDOID, corstr= "independence")
  modelFinal[[i]] = modelF
}


# Estimation et IC
Pool_modelFinal= pool(modelFinal)
Result_Pool_modelFinal = summary(Pool_modelFinal)
Result_Pool_modelFinal

RC = signif(exp(cbind(Result_Pool_modelFinal$estimate,Result_Pool_modelFinal$estimate-qnorm(0.975)*Result_Pool_modelFinal$std.error,Result_Pool_modelFinal$estimate+qnorm(0.975)*Result_Pool_modelFinal$std.error)),2)
dimnames(RC)[[1]] = Result_Pool_modelFinal$term
RC

#Analyse de la variance
Anova_tests = lapply(modelFinal, anova)
Anova_tests


### Analyse de sensibilité du Modele GEE sans interaction ####
### sur la base imputée avec MICE  #####

modelF_inter = c()
modelFinal_inter =list()
for (i in 1:71) {
  dat_inter = subset(dt_long_filt, .imp == i, )
  modelF_inter = geeglm(CPR_numeric~DEP+ factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL, data=dat_inter, family=binomial(link = "logit"),scale.fix=T, id=PSEUDOID, corstr= "independence")
  modelFinal_inter[[i]] = modelF_inter
}


# Estimation et IC
Pool_modelFinal_inter= pool(modelFinal_inter)
Result_Pool_modelFinal_inter = summary(Pool_modelFinal_inter)
Result_Pool_modelFinal_inter

RC_inter = signif(exp(cbind(Result_Pool_modelFinal_inter$estimate,Result_Pool_modelFinal_inter$estimate-qnorm(0.975)*Result_Pool_modelFinal_inter$std.error,Result_Pool_modelFinal_inter$estimate+qnorm(0.975)*Result_Pool_modelFinal_inter$std.error)),2)
dimnames(RC_inter)[[1]] = Result_Pool_modelFinal_inter$term
RC_inter

########Analyse de sensibilité sur l'ensemble de données complet##########
#Exclusion de toute les données manquantes
dt_complet =  na.omit(dt)
dt_complet_long = dt_complet %>%
  pivot_longer(
    cols = starts_with(c("CPR", "DEP", "ADL", "QOL")),
    names_to = c(".value", "TEMPS"),
    names_pattern = "(.*)(\\d+)"
  )
str(dt_complet_long)
dt_complet_long_filt =dt_complet_long %>%
  filter(!(CPR %in% c("3", "D")))

#Transformation de CPR et TEMPS
dt_complet_long_filt$CPR =  as.numeric(dt_complet_long_filt$CPR)
dt_complet_long_filt$TEMPS =  as.numeric(dt_complet_long_filt$TEMPS)
dt_complet_long_filt$CPR = ifelse(dt_complet_long_filt$CPR == 1, 1, 0)

#Tri par id 
dt_complet_long_filt = dt_complet_long_filt[order(dt_complet_long_filt$PSEUDOID),]
dt_complet_long_filt$CPR =factor(dt_complet_long_filt$CPR)

#Statistiques descriptives 
summary(dt_complet_long_filt)
cov_names=c("AGE", "SEX","SURV2MD1", "TEMPS","CPR",    
            "DEP", "ADL","QOL" )
print(CreateTableOne(vars = cov_names, data = dt_complet_long_filt, includeNA = TRUE), test = FALSE, smd = TRUE);


### Selection de la structure de la matrice de correlation de travail
dt_complet_long_filt$CPR_numeric = ifelse(dt_complet_long_filt$CPR == "1", 1, 0)

#selection de structure de corrélation

model_complet_long_ex = geeglm(CPR_numeric~DEP + factor(TEMPS) + DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL ,id=PSEUDOID,family=binomial(link = "logit"), corstr = 'exch',scale.fix=T,data=dt_complet_long_filt)
model_complet_long_ar = geeglm(CPR_numeric~ DEP + factor(TEMPS) + DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL ,id=PSEUDOID,family=binomial(link = "logit"), corstr = 'ar1',scale.fix=T,data=dt_complet_long_filt)
model_complet_long_id = geeglm(CPR_numeric~DEP + factor(TEMPS) + DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL ,id=PSEUDOID,family=binomial(link = "logit"), corstr = 'independence',scale.fix=T,data=dt_complet_long_filt)

QIC(model_complet_long_ex)
QIC(model_complet_long_ar)
QIC(model_complet_long_id)

#Test d'interaction 

model_complet_long_inter = geeglm(CPR_numeric~DEP+ DEP*factor(TEMPS) + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL ,id=PSEUDOID,family=binomial(link = "logit"), corstr = 'independence',scale.fix=T,data=dt_complet_long_filt)
model_complet_long_sans_inter = geeglm(CPR_numeric~DEP + factor(TEMPS)  + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL ,id=PSEUDOID,family=binomial(link = "logit"), corstr = 'independence',scale.fix=T,data=dt_complet_long_filt)

anova(model_complet_long_inter,model_complet_long_sans_inter )

joint_tests(model_complet_long_inter)
anova(model_complet_long_inter)

#Interaction non significatif

#hypothese
dev.new()
par(mfrow=c(1,5))
plot(x = dt_complet_long_filt$DEP, y = model_complet_long_sans_inter$resid);
lines(lowess(y = model_complet_long_sans_inter$resid, x = dt_complet_long_filt$DEP), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple");


plot(x = dt_complet_long_filt$SURV2MD1, y = model_complet_long_sans_inter$resid);
lines(lowess(y = model_complet_long_sans_inter$resid, x = dt_complet_long_filt$SURV2MD1), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple");


plot(x = dt_complet_long_filt$ADL, y = model_complet_long_sans_inter$resid);
lines(lowess(y = model_complet_long_sans_inter$resid, x = dt_complet_long_filt$ADL), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple");


plot(x = dt_complet_long_filt$AGE, y = model_complet_long_sans_inter$resid);
lines(lowess(y = model_complet_long_sans_inter$resid, x = dt_complet_long_filt$AGE), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple");



plot(x = dt_complet_long_filt$QOL, y = model_complet_long_sans_inter$resid);
lines(lowess(y = model_complet_long_sans_inter$resid, x = dt_complet_long_filt$QOL), col = "brown", lwd = 3);
abline(h = 0, lwd = 2, col = "purple");

## Donnees influentes
plot(model_complet_long_sans_inter$resid) 
#multicolinéarité
imcdiag(model_complet_long_sans_inter)
#Aucun changement à apporter 

#Estimation et IC
model_complet_F = geeglm(CPR_numeric~DEP + factor(TEMPS)  + AGE + SEX + SURV2MD1 + PHASE + ADL + QOL ,id=PSEUDOID,family=binomial(link = "logit"), corstr = 'independence',scale.fix=T,data=dt_complet_long_filt)

est= exp( esticon(model_complet_F,L=diag(9)))
cbind(est$estimate,est$lwr,est$upr)
