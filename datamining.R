install.packages('ggplot2')
install.packages('RColorBrewer')
install.packages('gridExtra')
install.packages('corrplot')
library(tidyverse)
library(GGally)
library(plyr)
library(knitr)
library(pROC)
library(DMwR)
library(caret)
library(rpart)
library(xgboost)
library(gbm)
library(nnet)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(corrplot)
install.packages("glmnet")
library(glmnet)
glmnet.control()
install.packages('performance')
library(performance)
install.packages('prediction')
library(performance)
library(dplyr)
library(gridBase)
library(gtable)
library(vcd)
library(Hmisc)
library(Amelia)
library(ROCR)
library(randomForest)




df = read.table("UCI_Credit_Card.csv",header=T, sep=",")
glimpse(df)
summary(df)
prop.table(table(df$default.payment.next.month)) #Combien de 1 et de 0 
sapply(df, function(x) {sum(is.na(x))})

##Preparation donnees

df$ID <- NULL
colnames(df)[24] <- "DEFAULT"
df$DEFAULT <- as.factor(ifelse(df$DEFAULT == 1, "Oui", "Non"))
df$SEX <- as.factor(ifelse(df$SEX == 1, "Homme", "Femme"))
df$MARRIAGE <- as.factor(ifelse(df$MARRIAGE == 1, "Mariage",
                                ifelse(df$MARRIAGE == 2, "Celibataire", "Autres")))
df$EDUCATION <- as.factor(ifelse(df$EDUCATION == 1, "EtudesSup",
                                 ifelse(df$EDUCATION == 2, "Universite", 
                                        ifelse(df$EDUCATION == 3, "Lycee",
                                               ifelse(df$EDUCATION == 4, "Autres", "Inconnu")))))



df$PAY <- ""

for (i in 1:nrow(df)) {
  if ((df[i,6] + df[i,7] +df[i,8]+df[i,9] +df[i,10]+df[i,11]) <= 0){
    df[i,25] <- "Oui"  
  }
  else {
    df[i,25] <- "Non"         
  }
}

df$PAY_0 <- as.factor(df$PAY_0)
df$PAY_2 <- as.factor(df$PAY_2)
df$PAY_3 <- as.factor(df$PAY_3)
df$PAY_4 <- as.factor(df$PAY_4)
df$PAY_5 <- as.factor(df$PAY_5)
df$PAY_6 <- as.factor(df$PAY_6)

df$PAY <-factor(df$PAY)


#Suppression variable PAY

df = subset(df, select = -c(PAY_0,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6))


## Donnees plot graph
summary(df)
base <- read.table("UCI_Credit_Card.csv",header=T, sep=",")

dim(base)
str(base)
summary(base)

#Creation de nouvelles variables

base$Pay <- ""
base$genre <- ""
base$education <- ""
base$etatmarital <- ""
#
for (i in 1:nrow(base)) {
  if ((base[i,7] + base[i,8] +base[i,9]+base[i,10] +base[i,11]+base[i,12]) <= 0){
    base[i,26] <- "OUI"  
  }
  else {
    base[i,26] <- "NON"         
  }
}

for (i in 1:nrow(base)) {
  if (base[i,3] == 1) {
    base[i,27] <- "Homme"  
  }
  else {
    base[i,27] <- "Femme"         
  }
}

for (i in 1:nrow(base)) {
  if (base[i,4] == 1) {
    base[i,28] <- "EtudesSup"
  } else if (base [i,4] == 2) {
    base[i,28] <- "Universite" 
  } else if (base [i,4] == 3) {
    base[i,28] <- "Lycee" 
  } else {
    base[i,28] <- "Inconnu" 
  }
}

for (i in 1:nrow(base)) {
  if(base[i,5] == 1) {
    base[i,29] <- "Mariage"
  } else if (base[i,5] == 2) {
    base[i,29] <- "Celibataire"
  } else {
    base[i,29] <- "Autres"
  }
}

#Decoupe de variables

base$Age.categories<-cut(base$AGE,c(10,20,30,40,50,60,70))

#Mise en facteurs

base$Pay <-factor(base$Pay)
base$SEX <-factor(base$SEX)
base$EDUCATION <- factor(base$EDUCATION)
base$MARRIAGE <- factor(base$MARRIAGE)
base$AGEf <- factor(base$AGE)
base$default.payment.next.month<-factor(base$default.payment.next.month)
base$genre <- factor(base$genre)
base$education <- factor(base$education)
base$etatmarital <- factor(base$etatmarital)

#Suppression variable

base = subset(base, select = -c(PAY_0,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6))

dim(base)
str(base)
summary(base)


### Montant credit par genre, niveau d'education et paiement

# Montant credit par genre et niveau d'education
d1 <- ggplot(base, aes(factor(genre), (LIMIT_BAL/1000), fill=education)) + 
  geom_boxplot() +
  xlab("Genre") + 
  ylab("MontantCredit") + 
  scale_fill_brewer(palette = "Accent")

# Monatnt credit par niveau d'education et genre
d2 <- ggplot(base, aes(factor(education), (LIMIT_BAL/1000), fill=genre)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("MontantCredit") + 
  scale_fill_brewer(palette = "Paired")

# Montant credit par niveau d'education et travail
d3 <-ggplot(base, aes(factor(education), (LIMIT_BAL/1000), fill=Pay)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("MontantCredit") 

grid.arrange(d1, d2, d3)

#Interpretation: On peut voir que le genre n'a pas d'effet dans le montant credit, alors que le niveau d'education a un effet positif ainsi que le travail qui est important.



### Relation entre statut marital et montant credit par genre

ggplot(base, aes(factor(etatmarital), (LIMIT_BAL/1000), fill=genre)) + 
  geom_boxplot() +
  xlab("Etat marital") + 
  ylab("Montant Credit") + 
  coord_cartesian(ylim = c(0,350)) +
  scale_fill_brewer(palette = "Paired")

#Interpretation: On voit qu'il n'y a pas de changement pour les femmes selon leur statut marital pour le montant de credit. Mais il y a un changement pour les hommes maries notamment avec un hausse du montant notamment pour les depenses.


### Histogramme de montant de credit et de defaut de paiement

ggplot(aes(x = base$LIMIT_BAL/1000), data = base) +
  geom_histogram(aes(fill = base$default.payment.next.month)) +
  xlab("Montant Credit") +
  ylab("Nombre d'individus") +
  scale_fill_discrete(name="Defaut de paiement",
                      breaks=c(0, 1),
                      labels=c("Non", "Oui")) +
  xlim(c(0,750)) +
  facet_wrap(~education)


#Interpretation: Le montant de credit accorde et le nombre d'individus qui ont fait defaut de paiement est globalement identique pour les personnes en etudes sup et a l'universite. Le nombre d'individus ayant fait defaut est aussi semblable pour les diplomes de lycee.


### Relation entre l'education et le defaut de paiement.

d1 <- ggplot(base, aes(x=default.payment.next.month)) + 
  geom_histogram(stat="count",color='red',fill='orange') +
  xlab("Defaut de paiement") + ylab("Nombre clients") + 
  facet_wrap(~education)

d2 <- ggplot(base, aes(x=default.payment.next.month),aes(y=stat_count(gender))) + 
  geom_bar(aes(fill=factor(base$education))) +
  xlab("Defaut de paiement")+ylab("Nombre clients") + 
  facet_wrap(~genre)+
  scale_fill_discrete(name="Education")

grid.arrange(d1, d2, ncol=1)


#Interpretation:   Pas d'effet reel de l'education sur le defaut de  paiement


### Montant credit accorde par groupe d'age et d'education

ggplot(data = subset(base,!is.na(Age.categories)), aes(factor(education), (LIMIT_BAL/1000), fill=Age.categories)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("Montant Credit") + 
  coord_cartesian(ylim = c(0,500)) +
  scale_fill_brewer(palette = "Accent")

#Interpretation: L'influence du niveau d'education sur le montant de credit est croissante avec l'age quand on compare toutes les moyennes des tranches d'ages selon leur niveau d'education.


### Changement nom variable

base$SEP2005<-base$BILL_AMT1
base$AUG2005<-base$BILL_AMT2
base$JUL2005<-base$BILL_AMT3
base$JUN2005<-base$BILL_AMT4
base$MAY2005<-base$BILL_AMT5
base$APR2005<-base$BILL_AMT6

### Depenses par mois

apr <- ggplot(aes(x=AGE,y=APR2005/1000),data=base) +
  xlab("Age") + 
  ylab("Factures en avril") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="orange") + geom_smooth(stat='summary', fun=mean)

may <- ggplot(aes(x=AGE,y=MAY2005/1000),data=base) +
  xlab("Age") + 
  ylab("Factures en mai") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="blue") + geom_smooth(stat='summary', fun=mean)

jun <- ggplot(aes(x=AGE,y=JUN2005/1000),data=base) +
  xlab("Age") + 
  ylab("Factures en juin") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="green") + geom_smooth(stat='summary', fun=mean)

jul <- ggplot(aes(x=AGE,y=JUL2005/1000),data=base) +
  xlab("Age") + 
  ylab("Factures en juillet") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="orange") + geom_smooth(stat='summary', fun=mean)

aug <- ggplot(aes(x=AGE,y=AUG2005/1000),data=base) +
  xlab("Age") + 
  ylab("Factures en aout") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="blue") + geom_smooth(stat='summary', fun=mean)

sep <- ggplot(aes(x=AGE,y=SEP2005/1000),data=base) +
  xlab("Age") + 
  ylab("Factures en septembre") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="green") + geom_smooth(stat='summary', fun=mean)

grid.arrange(apr,may,jun,jul,aug,sep,ncol=3)


#Interpretation: On ne voit pas de differenciation dans les depenses selon l'age entre les differents mois, et en moyenne la facture par age est la meme pour tout les mois.

### Correlation entre montant credit, factures et paiements

M <- cor(subset(base, select = c(LIMIT_BAL,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)))
corrplot(M, method="number")

#Interpretation: On voit en regardant les correlations, qu'il y a une faible correlation entre le montant de credit, les factures et les paiements. On remarque logiquement une forte corrélation entre les montants de factures eux memes car cela reflete les montants cumules

###Probabilites de montant de credit par age

ggplot(aes(x=AGE,y=LIMIT_BAL/1000),data=subset(base,!is.na(Age.categories)))+
  xlab("Age") + 
  ylab("Montant Credit") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+
  scale_color_brewer(palette = "Pastel1")+
  geom_jitter(alpha=0.5, position = position_jitter(h=0), aes(color=Age.categories)) +
  geom_smooth(stat='summary', fun=mean) +
  geom_smooth(stat='summary', fun=quantile, fun.args = list(probs = 0.1), color = 'black', linetype=2) +
  geom_smooth(stat='summary', fun=quantile, fun.args = list(probs = 0.5), color = 'red', linetype=2) +
  geom_smooth(stat='summary', fun=quantile, fun.args = list(probs = 0.9), color = 'black', linetype=2)


#Interpretation: On trace ici la limite de montant de credit par tranche d'age et on remarque aisément les limites de 500 000 et 200 000 ce qui est 95% et 50%: 
#  95% ont des montants de credits egaux ou inferieurs a 500 000 et 50% a 200 000.
#  La ligne bleue etant la moyenne du montant des credits aux clients, l'ecart entre la ligne bleue et rouge montre que la banque donne un plus gros montant que les 50% de probabilite.
# On peut aussi interpreter les probabilites comme etant l'intervalle de confiance et l'exposition de la banque: La banque peut donc se permettre d'avoir des defaut de 500 000 ou plus a seulement 5% si il y a du défaut. 




#Graph de distribution
graph1 <- ggplot(data=base, aes(x=genre,fill=genre)) + geom_bar() +
  labs(title = "Distribution par genre", x ="Genre",fill = "Genre") +
  scale_fill_manual(values=c("#56B4E9", "#FF9999")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

graph2 <- ggplot(data=base, aes(x=education,fill=education)) + geom_bar() +
  labs(title = "Distribution par niveau d'education", x ="Niveau d'education",fill = "Niveau d'education") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

graph3 <- ggplot(data=base, aes(x=etatmarital,fill=etatmarital)) + geom_bar() +
  labs(title = "Distribution par etat marital", x ="Etat marital",fill = "Etat marital") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

grid.arrange(graph1,graph2,graph3,ncol=2)




graph4 <- ggplot(data=base, aes(x=education,fill=genre)) + geom_bar(position='dodge') +
  labs(title = "Distribution par niveau d'education et genre", x ="Niveau d'education",fill = "Genre") +
  scale_fill_manual(values=c("#56B4E9", "#FF9999")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

graph5 <- ggplot(data=base, aes(x=etatmarital,fill=genre)) + geom_bar(position='dodge') +
  labs(title = "Distribution par etat marital et genre", x ="Etat marital",fill = "Genre") +
  scale_fill_manual(values=c("#56B4E9", "#FF9999")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

graph6 <- ggplot(data=base, aes(x=education,fill=genre)) + geom_bar(position='dodge') +
  labs(title = "Distribution par niveau d'education et etat marital", x ="Niveau d'education",fill = "Etat marital") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

grid.arrange(graph4, graph5, graph6,ncol=2)


#################


#Quel groupe d'age fait le plus de défaut
ggplot(data = base, aes(x = AGE)) + 
  geom_histogram(bins = 50, fill = "purple", col = "blue", alpha = 0.3) + 
  scale_x_continuous(breaks = seq(min(0), max(90), by = 5), na.value = TRUE)


#Graph clients par niveau d'education
ggplot(base, aes(x=education, fill = education)) + 
  geom_bar() +
  labs(title="Clients par niveau d'education", 
       
       x="Niveau d'education",
       fill="Niveau d'education")


# Courbe de densite clients par niveau d'education
ggplot(base, aes(x=education, fill = education)) + 
  geom_density(aes(fill=factor(education)), alpha=0.8) + 
  labs(title="Clients par niveau d'education", 
       x="Education",
       fill="Education")




# Graph clients qui font defauts

ggplot(base, aes(x=default.payment.next.month, fill = default.payment.next.month)) + 
  geom_bar() + 
  labs(title="Defauts des clients",
       x="Defaut de paiement",
       fill="Defaut de paiement")

# Courbe de densite 

ggplot(base, aes(x=default.payment.next.month, fill = default.payment.next.month)) +
  geom_density(aes(fill=factor(default.payment.next.month)), alpha=1.0) + 
  labs(title="Defauts des clients", 
       x="Defaut de paiement",
       fill="Defaut de paiement")


# Graph pour montrer relations entre les variables
# Client par genre 
ggplot(base, aes(x=genre, fill = genre)) + 
  geom_bar() + 
  labs(title="Clients par genre",
       x="Genre",
       fill="Genre")

# Courbe de densite
ggplot(base, aes(x=genre, fill = genre)) + 
  geom_density(aes(fill=factor(genre)), alpha=0.8) + 
  labs(title="Clients par genre",
       x="Genre",
       fill="Genre")


# Graph clients par etat marital
ggplot(base, aes(x=etatmarital, fill = etatmarital)) + 
  geom_bar() +
  labs(title="Clients par statut marital",
       x="Statut marital",
       fill="Statut marital")

# Courbe de densite clients par statut marital
ggplot(base, aes(x=etatmarital, fill = etatmarital)) +
  geom_density(aes(fill=factor(etatmarital)), alpha=0.8) + 
  labs(title="Clients par statut marital",
       x="Statut marital",
       fill="Statut marital")



# Quel groupe d'age a le plus de credit et de montant
theme_set(theme_bw())  
g <- ggplot(data = base, aes(x = AGE, y = LIMIT_BAL))
g + geom_count(col="blue", show.legend=F) +
  labs(title="Age / Montant Credit")


# Diagramme en violon
# Niveau d'education / Montant Credit
ggplot(data = base,aes(x = education, y = LIMIT_BAL,
                       fill = education,
                       color = education))  + 
  geom_violin() +
  labs(title="Niveau d'education / Montant Credit") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



# Etat marital / Montant Credit
ggplot(data = base,aes(x = etatmarital, y = LIMIT_BAL,
                       fill = etatmarital,
                       color = etatmarital
)) + geom_violin() +
  labs(title="Etat marital / Montant Credit")


# Genre / Montant Credit
ggplot(data = base,aes(x = genre, y = LIMIT_BAL,
                       fill = genre,
                       color = genre
))  + geom_violin() +
  labs(title="Genre / Montant Credit")



# Defaut / Montant Credit
ggplot(data = base,aes(x = default.payment.next.month, y = LIMIT_BAL,
                       fill = default.payment.next.month,
                       color = default.payment.next.month )) +
  geom_violin() +
  labs(title="Defaut / Montant Credit")




# Graph pour determiner si les tendances se superposent
#Interpretation: On voit avec les courbes de densite que les defauts se chevauchent pour le montant de credit, les cas qui ne se superposent pas on un pouvoir discriminant plus grand.

featurePlot(x = base[, c(1)],                       
            y = base$default.payment.next.month,    
            plot = "density",                         
            auto.key = T)





#V de Cramer
# On determine avec le V de Cramer quelles sont les variables les plus liées a la variable a expliquer.

base$ID <- NULL
par(mfrow = c(1, 1))

# calcul du V de Cramer 
cramer  <- matrix(NA,ncol(base),3)
effectif <- dim(base)[1]
for (i in (1:ncol(base))) 
{   cramer[i,1] <- names(base[i])
cramer[i,2] <- sqrt(chisq.test(table(base[,i],base$default.payment.next.month))$statistic/effectif)
cramer[i,3] <- chisq.test(table(base[,i],base$default.payment.next.month))$p.value
}
colnames(cramer) <- c("variable","V de Cramer","p-value chi2")

# affichage des variables par V de Cramer decroissants
vcramer <- cramer [order(cramer[,2], decreasing=T),]
vcramer

# graphique
old <- par(no.readonly = TRUE)
par(mar = c(8, 4, 4, 0))
#par(mar = c(9, 4, 4, 0))

barplot(as.numeric(vcramer[-1,2]),col=gray(0:nrow(vcramer)/nrow(vcramer)),
        names.arg=vcramer[-1,1], ylab='V de Cramer', ylim=c(0,1),cex.names = 0.8, las=3)
par(old)




#############################################
############### MODELISATION#################
#############################################



intraining <- createDataPartition(y=df$DEFAULT, p=0.7, list=F)
train.batch <- df[intraining,]
test.batch <- df[-intraining,]



# echantillons d'apprentissage et de validation
entrainement  <- train.batch
validation  <- test.batch
table(entrainement$DEFAULT)/nrow(entrainement)
table(validation$DEFAULT)/nrow(validation)


# =========================================================================================================
# REGRESSION LOGISTIQUE
# =========================================================================================================




# l'ensemble des predicteurs
predicteurs <- -grep('df|DEFAULT', names(entrainement)) # position variable a expliquer
predicteurs

# formule
formule <- as.formula(paste("DEFAULT ~ ",paste(names(entrainement[,predicteurs]),collapse="+")))
formule

# selection de modele ascendant
logit <- glm(DEFAULT~1, data=entrainement, family=binomial(link = "logit"))
summary(logit)

# recherche maximale
selection <- step(logit, direction="forward", trace=TRUE, k = 2, scope=list(upper=formule), na.omit = TRUE)
selection
summary(selection)

# le modele retenu par la methode ascendante :
# PAY + PAY_AMT1 + LIMIT_BAL + MARRIAGE + EDUCATION + PAY_AMT2 + SEX + PAY_AMT4 + AGE + PAY_AMT5 + PAY_AMT3

# application du modele a un jeu de donnees
entrainement.ascbic <- predict(selection, newdata=entrainement, type="response")
validation.ascbic <- predict(selection, newdata=validation, type="response")

# aire sous la courbe ROC
library(pROC)
auc(entrainement$DEFAULT,entrainement.ascbic)
auc(validation$DEFAULT,validation.ascbic)

# selection de modele descendant
logit <- glm(DEFAULT~., data=entrainement, family=binomial(link = "logit"))
selection <- step(logit, direction="backward", trace=TRUE, k = log(nrow(entrainement)))
summary(selection)

# le modele retenu par la methode descendante :
# LIMIT_BAL + SEX + EDUCATION + MARRIAGE + BILL_AMT1 + BILL_AMT2 + PAY_AMT1 + PAY_AMT2 + PAY

# selection avec AIC : k = 2 
# variante avec BIC au lieu de AIC : k = log(nrow(entrainement))


# ---------------------------------------------------------------------------------------------------------
# Modele final pour la regression
# ---------------------------------------------------------------------------------------------------------

par(mfrow = c(1, 1))

logit = glm(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + BILL_AMT1 + BILL_AMT2 + PAY_AMT1 + PAY_AMT2 + PAY, data=entrainement, family=binomial(link = "logit"))
summary(logit)
summary(logit)$coefficients
pred.logit <- predict(logit, newdata=validation, type="response")
head(pred.logit)

# aire sous la courbe ROC

auc(validation$DEFAULT,pred.logit)
roc <- plot.roc(validation$DEFAULT, pred.logit, main="", percent=TRUE, ci=TRUE)
roc.se <- ci.se(roc, specificities=seq(0, 100, 5))
plot(roc.se, type="shape", col="grey")


# ---------------------------------------------------------------------------------------------------------
# Grille de score ~#########
# ---------------------------------------------------------------------------------------------------------

logit$xlevels
nom = gsub("[0-9]", "", names(unlist(logit$xlevels))) # on garde seulement le nom des classes
nom

VARIABLE=c("", gsub("[0-9]","", names(unlist(logit$xlevels))) ) # on supprime les chiffres
VARIABLE
MODALITE=c("", unlist(logit$xlevels)) # on extrait les modalites
MODALITE

names=data.frame(VARIABLE,MODALITE,NOMVAR=c("(Intercept)", 
                                            paste(VARIABLE,MODALITE,sep="")[-1])) # on concatene MODALITE et VARIABLE
# names$NOMVAR = rownames(names)

regression=data.frame(NOMVAR=names(coefficients(logit)), COEF=as.numeric(coefficients(logit))) # on veut chaque coefficient avec la MODALITE et la VARIABLE
regression

param = merge(names,regression,all.x=TRUE)[-1] # on enleve la 1ere colonne NOMVAR
param

param$COEF[is.na(param$COEF)] <- 0 # on remplace les valeurs manquantes dans les coefficients par 0
param

# calcul du poids total pour normalisation
mini=aggregate(data.frame(min = param$COEF), by = list(VARIABLE = param$VARIABLE), min)
maxi=aggregate(data.frame(max = param$COEF), by = list(VARIABLE = param$VARIABLE), max)

total=merge(mini,maxi)
total$diff = total$max - total$min
total
poids_total = sum(total$diff)
poids_total

# calcul des poids par modalite
grille = merge(param,mini,all.x=TRUE)
grille$delta = grille$COEF - grille$min
grille$POIDS = round((100*grille$delta) / poids_total)
grille[order(grille$VARIABLE,grille$MODALITE)[which(VARIABLE!="")], c("VARIABLE","MODALITE","POIDS")]


# ---------------------------------------------------------------------------------------------------------
# Seuil de score
# ---------------------------------------------------------------------------------------------------------

# application du modele a l'ensemble des donnees
pred.logit <- predict(logit, newdata=df, type="response")

# taux score
quantile(pred.logit)
q <- unique(quantile(pred.logit, seq(0, 1, by=0.03)))
qscore <- cut(pred.logit, q)
tab <- table(qscore, df$DEFAULT)
ti <- prop.table(tab,1)[,2] # affichage % en ligne
old <- par(no.readonly = TRUE)
par(mar = c(7, 4, 2, 0))
barplot(as.numeric(ti), col=gray(0:length(ti)/length(ti)),
        names.arg=names(ti), ylab='Taux de défaut', ylim=c(0,0.8), cex.names = 0.8, las=3)
abline(v=c(28.82),col="red")
par(old)

# application des seuils de score
library(car)
confusion <- recode(pred.logit,"lo:0.26='0'; 0.26:hi='1'")
tab <- table(confusion,df$DEFAULT)
prop.table(tab,1)
tab

a = tab[1,1] # coordonees des vrai -
b = tab[1,2] # coordonees des faux -
c = tab[2,1] # coordonees des faux +
d = tab[2,2] # coordonees des vrai +

tot = a+b+c+d

#pourcentage de bon classement sur lechantillon total
bc = (a+d)/(tot)*100
bc

#Score derreur sur l'echantillon total
se = a/(a+c)*100
se

#Score predit sur l'echantillon total
sp = d/(b+d)*100
sp



# ---------------------------------------------------------------------------------------------------------
#REGRESSION PEENALISEE RIDGE
# ---------------------------------------------------------------------------------------------------------


#install.packages("glmnet")
library(glmnet)
glmnet.control() # parametres par defaut

# le vecteur de predicteurs x de glmnet doit etre numerique => toutes
# les variables qui sont des facteurs sont remplacees par les indicatrices
# de leurs modalites (sauf la modalite de reference)

x <- model.matrix( ~ . -1 , data=entrainement[,-which(names(entrainement)=="DEFAULT")])
y <- entrainement[,"DEFAULT"]

# validation croisee
set.seed(235)
#glmnet.control(fdev = 0)
system.time(cvfit <- cv.glmnet(x, y, alpha=0, family = "binomial", type="auc", nlambda=100))
print(cvfit)
length(cvfit$lambda)
cvfit$lambda # valeurs de penalisation
cvfit$lambda[1] # plus petit lambda annulant tous les coefficients
# a noter que ce lambda devrait etre infini pour alpha = 0 (ridge)
# et que pour avoir un lambda fini, on remplace alpha = 0 par alpha = 0.001
cvfit$lambda[length(cvfit$lambda)] # lambda precedent divise par presque 10000
cvfit$lambda.min # lambda donnant le plus petit taux derreur (cross-valide)
log(cvfit$lambda.min)
which(cvfit$lambda==cvfit$lambda.min) # rang de lambda donnant l'AUC maximale
cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]
# representation graphique de l'erreur (= AUC) en fonction de la penalisation
plot(cvfit)
abline(h=cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)], col='blue', lty=2)
abline(v=log(cvfit$lambda.min), col='blue', lty=2)

# calcul de la regression pour une plage de valeurs de lambda
fit <- glmnet(x, y, alpha=0, family = "binomial", lambda=seq(cvfit$lambda[1],
                                                             cvfit$lambda[length(cvfit$lambda)], length=10000), standardize = T)
# affichage des coefficients
plot(fit)
plot(fit, xvar="lambda", label="T")

# prediction sur une plage de lambda sur la base d'apprentissage
# sans precision de "s", toute la sequence de lambda utilisee en apprentissage est reprise
ypred <- predict(fit, newx=x, type="response")
length(fit$lambda)
install.packages("ROCR")
library(ROCR) # presque 5 fois plus rapide
roc <- function(x) { performance(prediction(ypred[,x],y),"auc")@y.values[[1]] }
vauc <- Vectorize(roc)(1:length(fit$lambda))
# affichage de l'AUC
plot(vauc~log(fit$lambda), col='blue', lty=2, cex=0.5, pch=16)
abline(h=vauc[which.max(vauc)], col='black', lty=2)
abline(v=log(fit$lambda[which.max(vauc)]), col='blue', lty=2)

# prediction sur une plage de lambda sur la base de VALIDATION
xt <- model.matrix( ~ . -1, data=validation[,-which(names(validation)=="DEFAULT")])
yt <- as.numeric(validation[,"DEFAULT"])
ytpred <- predict(fit, newx=xt, type="response")
roc <- function(x) { performance(prediction(ytpred[,x],yt),"auc")@y.values[[1]] }
vauc <- Vectorize(roc)(1:length(fit$lambda))
# valeurs de l'AUC en validation
vauc[10000] # AUC du modele le moins penalise
which.max(vauc) # rang de la penalisation donnant la plus forte AUC
fit$lambda[which.max(vauc)] # penalisation donnant la plus forte AUC
vauc[which.max(vauc)] 
# affichage de l'AUC
plot(vauc~log(fit$lambda), col='blue', lty=2, cex=0.5, pch=16)
abline(h=vauc[which.max(vauc)], col='black', lty=2)
abline(v=log(fit$lambda[which.max(vauc)]), col='blue', lty=2)




# ---------------------------------------------------------------------------------------------------------
# Seuil de score
# ---------------------------------------------------------------------------------------------------------

# application du modele a l'ensemble des donnees
x <- model.matrix( ~ . -1, data=df[,-which(names(df)=="DEFAULT")])
ytpred <- predict(fit, newx=x, type="response")[,10000]
View(ytpred)
# taux de defaut par decile du score
quantile(ytpred)
q <- unique(quantile(pred.logit, seq(0, 1, by=0.03)))
qscore <- cut(ytpred, q)
tab <- table(qscore, df$DEFAULT)
ti <- prop.table(tab,1)[,2] # affichage % en ligne
old <- par(no.readonly = TRUE)
par(mar = c(7, 4, 2, 0))
barplot(as.numeric(ti), col=gray(0:length(ti)/length(ti)),
        names.arg=names(ti), ylab='Taux de défaut', ylim=c(0,0.8), cex.names = 0.8, las=3)
abline(v=c(28.82),col="red")
par(old)

# application des seuils de score
library(car)
confusion <- recode(ytpred,"lo:0.268='0'; 0.268:hi='1'")
tab <- table(confusion,df$DEFAULT)
prop.table(tab,1)
tab

a = tab[1,1] # coordonees des vrai -
b = tab[1,2] # coordonees des faux -
c = tab[2,1] # coordonees des faux +
d = tab[2,2] # coordonees des vrai +

tot = a+b+c+d

#pourcentage de bon classement sur lechantillon total
bc = (a+d)/(tot)*100
bc

#Score derreur sur l'Ã©chantillon total
se = a/(a+c)*100
se

#Score predit sur l'Ã©chantillon total
sp = d/(b+d)*100
sp





# ---------------------------------------------------------------------------------------------------------
# BAGGING
# ---------------------------------------------------------------------------------------------------------

#install.packages("ipred")
library(ipred)
library(rpart)

?bagging()
# bagging avec arbres de profondeur maximale---
set.seed(235)


bag <- bagging(DEFAULT ~ ., data=entrainement, nbagg=200, coob=TRUE,# 200  AUC 75.8% avec average
               control= rpart.control(cp=0))                  # 200 76.25 % avec average

bag


bag1 <- bagging(DEFAULT ~ ., data=entrainement, nbagg=300, coob=TRUE,# 300  AUC 75.8% avec average
                control= rpart.control(cp=0))                  # 300 76.32 % avec average

bag1


# prediction
library(pROC)
pred.bag <- predict(bag1, type="prob", validation)
auc(validation$DEFAULT, pred.bag[,2], quiet=TRUE)

pred.bagaverage <- predict(bag1, validation, type="prob", aggregation="average")
auc(validation$DEFAULT, pred.bagaverage[,2], quiet=TRUE)



# agregation par moyenne des probabilites
pred.bg1 <- predict(bag1, validation, type="prob", aggregation=,"average")
auc(validation$DEFAULT, pred.bg1[,2], quiet=TRUE)

# agregation par vote a la majorite
pred.bg2 <- predict(bag1, validation, type="prob", aggregation="majority")
head(pred.bg2)
auc(validation$DEFAULT, pred.bg2[,2], quiet=TRUE)



# ---------------------------------------------------------------------------------------------------------
# FORET ALEATOIRE
# ---------------------------------------------------------------------------------------------------------


rand.model <- randomForest(DEFAULT~.,data=entrainement, importance=T,
                           ntree=500, keep.forest=T)
rand.model
varImpPlot(rand.model)

rand.pred <- predict(rand.model, validation, type="class")

rand.prob <- predict(rand.model, validation, type='prob')[,2]
pred4 <- prediction(rand.prob,validation$DEFAULT)

rf.perf1 <- performance(pred4,"tpr","fpr")


rf.auc <- performance(pred4,"auc")

cat(paste0(rf.auc@y.name,": ", round(rf.auc@y.values[[1]], 2)))


#--- verification des erreurs.---

# out of bag
mean(rand.model$oob.times) # nb de fois ou chaque individu est out of bag
head(rand.model$oob.times)
n <- 100000000000000
rand.model$ntree*(1-(1/n))^n

# erreur
head(rand.model$votes) # votes pour chaque individu OOB des arbres de l'???chantillon d'apprentissage
head(predict(rand.model,type='prob')) # commande identique ??? la pr???c???dente
rand.model$pred <- ifelse(rand.model$votes[,2]>=0.5,1,0)
err_rf <- sum(rand.model$pred!=entrainement$DEFAULT) / nrow(entrainement)



# taux d'erreur
head(rand.model$err.rate)
head(rand.model$test$err.rate)
tail(rand.model$err.rate)
tail(rf$test$err.rate)


# taux d'erreur OOB
err_OOB <- sum(rand.model$pred != entrainement$DEFAULT) / nrow(entrainement)
err_OOB# 23.48
table(entrainement$DEFAULT,rand.model$pred)


# taux d'erreur en apprentissage
entrainementrf <- predict(rf, entrainement, type='response')
table(entrainement$y,entrainementrf)
err_entrainement <- sum(rf$pred != entrainement$y) / nrow(data_assu2[id,])
err_entrainement


# taux d'erreur en fonction du nombre d'arbres
head(rf$err.rate)
head(rf$test$err.rate)
tail(rf$err.rate)
tail(rf$test$err.rate)

# evolution du taux d'erreur (calcule sur individus OOB de l'echantillon d'apprentissage)
plot(rand.model$err.rate[,1], type='l', ylim=c(.20,.3), xlab="nombre d'iterations", ylab='erreur')
lines(rand.model$test$err.rate[,1], type='l', lwd=2, col='red')

# importance des variables (randomForest)
rand.model$importance[,3]
rand.model$importance[,4]
varImpPlot(rand.model)
rf.imp <- rand.model$importance[,3][order(rand.model$importance[,3], decreasing=TRUE)]
rf.imp # variables par ordre decroissant d'importance
