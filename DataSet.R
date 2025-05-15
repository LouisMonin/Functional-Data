#Implémentation du jeu de données dans RStudio
install.packages("fda")
library(fda)
data(MontrealTemp) 

#Affection des données en fonctions des mois 
Janvier <- t(MontrealTemp[,1:31])
Janvier

Fevrier <- t(MontrealTemp[,32:59])
Fevrier

Mars <- t(MontrealTemp[,60:90])
Mars

Avril <- t(MontrealTemp[,91:120])
Avril

Mai <- t(MontrealTemp[,121:151])
Mai

Juin <- t(MontrealTemp[,152:181])
Juin

Juillet <- t(MontrealTemp[,182:212])
Juillet

Aout <- t(MontrealTemp[,212:243])
Aout

Septembre <- t(MontrealTemp[,244:273])
Septembre

Octobre <- t(MontrealTemp[,274:304])
Octobre

Novembre <- t(MontrealTemp[,305:334])
Novembre

Decembre <- t(MontrealTemp[,334:365])
Decembre

#Réalisation des moyennes de températures en degré Celsius sur chaque mois
mean(Janvier)#-10.53055
mean(Fevrier)#-9.062605
mean(Mars)#-2.627324
mean(Avril)#5.652941
mean(Mai)#12.95949
mean(Juin)#18.0148
mean(Juillet)#20.83472
mean(Aout)#19.46112
mean(Septembre)#14.57931
mean(Octobre)#8.332827
mean(Novembre)#1.699412
mean(Decembre)#-6.524632

#Réalisation de la moyenn de température en degré Celsius de 1961 à 1994
moyenne_totale <- (mean(Janvier)+mean(Fevrier)+mean(Mars)+mean(Avril)+mean(Mai)+mean(Juin)+mean(Juillet)+mean(Aout)+mean(Septembre)+mean(Octobre)+mean(Novembre)+mean(Decembre))/12
moyenne_totale#6.065793

#Ranger par ordre croissant les valeurs des moyennes de températures de chaque mois
-10.53055
-9.062605
-6.524632
-2.627324
1.699412
5.652941
8.332827
12.95949
14.57931
18.0148
19.46112
20.83472

#Calcul du mode qui est la valeur la plus fréquente d'une série de données
sort(table(MontrealTemp),decreasing=TRUE)#Le mode de l'échantillon est 18.9°C

#Calcul de la variance sur les moyennes des températures des différents mois.

xi<- c(-10.53055,-9.062605,-6.524632,-2.627324,1.699412, 5.652941, 8.332827, 12.95949, 14.57931, 18.0148,19.46112, 20.83472)
mean(xi^2)-mean(xi)^2
variance <- sqrt(mean(xi^2)-mean(xi)^2)
variance#10.89001

#nettoyage des doonées
#pour trouver les valeurs manquantes :
is.na(MontrealTemp) 
#Le resultat montre que y'a pas de valeur manquante

#ACP
install.packages("fda")
library(fda)
install.packages("FactoMineR")
library(FactoMineR) #pour afficher des graphiques

data(MontrealTemp) 

res.pca <- PCA(MontrealTemp, graph = TRUE)
res.pca
#affichage les valeurs propres
res.pca$eig
#affichage de la contribution de chaque variable à chaque axe
res.pca$var$contrib

#Regression linéaires
 
Jours <- 1:365 #vecteurs pour les jours de l'année
Temperature_1961 <- MontrealTemp[1,]
Temperature_1962 <- MontrealTemp[2,]
Temperature_1970 <- MontrealTemp[10,]
Temperature_1980 <- MontrealTemp[20,]
# Faire la régression linéaire entre la variable température et la variable Date
modele1 <- lm(Temperature_1961 ~ Jours, data = as.data.frame(MontrealTemp))
modele2 <- lm(Temperature_1962 ~ Jours, data = as.data.frame(MontrealTemp))
modele3 <- lm(Temperature_1970 ~ Jours, data = as.data.frame(MontrealTemp))
modele4 <- lm(Temperature_1980 ~ Jours, data = as.data.frame(MontrealTemp))

# Afficher les résultats
summary(modele1)
summary(modele2)
summary(modele3)
summary(modele4)

plot(modele1, main = "regression linéaire1961")
plot(modele2, main = "regression linéaire1962")
plot(modele3, main = "regression linéaire1970")
plot(modele4, main = "regression linéaire1980")

# Pour les moyennes 
Moyenne <- c(-10.53055,-9.062605,-6.524632,-2.627324,1.699412,5.652941,8.332827,12.95949,14.57931,18.0148,19.46112,20.83472)
Mois <- 1:12
modele_moyenne <- lm(Moyenne ~ Mois, data = as.data.frame(MontrealTemp))
summary(modele_moyenne)

plot(modele_moyenne, main = "regression linéaire des moyennes mensuelles")

