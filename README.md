# Placebo
---
title: "Devoir 2 UE"
author: "FIANKO Kossi"
date: "17/04/2021"
output:
  html_document: default
  word_document: default
---
```{r}
# DEVOIR A FAIRE
# Importation des données nommées 
```



```{r}

```




```{r}
###LA MEILLEURE
## Faire appel à une autre methode la "GTSUMMARY" 

library(gtsummary)
library(readr)

dat <- RDMCODE %>% select(XTRTMT, RANDOMIZENUM,TRTMT)
dat %>% tbl_summary(by=XTRTMT)

tail(ASILITE)
 gh <- filter(ASILITE, ASILITE$VISID == "SCRNBASE")

asil <- select(gh,VISID,CLASS,GENDER,BIRTHDT,RACE,RELGION)
asil <- filter(asil, asil$VISID == "SCRNBASE")
#todo a modifier
asil <- slice(asil, 1:395)

# je vais joindre les 2 données dat et asil
tot <- cbind(dat,asil)
head(tot)

library(tidyr)


# Supprimer les na
#sup <- apply(tot, 1, function(x){any(is.na(x))})
#tot.1 <- tot[!row.has.na,]
#tot.filtered 
#head(tot.1)


# La selection des variables à utiliser
tot.1 <- tot %>% select("XTRTMT", "RANDOMIZENUM", "TRTMT", "VISID", "CLASS", "GENDER", 
"BIRTHDT", "RACE", "RELGION")

# Utilisation du parkage Flextable 
library(flextable)
tot.1 %>% tbl_summary(by= XTRTMT)
tot.1

#Renommination des sous variable

# le genre
tot.1$GENDER <- factor(tot.1$GENDER,
                       levels = c(0,1),
                       labels = c("Homme","Femme"))
# La race
tot.1$RACE <- factor(tot.1$RACE,
                     levels = c(1,2,4,4,5,6,7),
                     labels = c("White", "Latino","Black","Asian","American","Autre","Non connu"))
#La classe
tot.1$CLASS <- factor(tot.1$CLASS,
                      levels = c(1,2),
                      labels = c("Intake","Follow up"))
# La religion
tot.1$RELGION <- factor(tot.1$RELGION,
                        levels = c(1,2,4,4,5,6),
                        labels = c("Protestant","Catholic","Jewish","Islam","Other","None"))

#Representation  du tableOne

tot.1 %>% tbl_summary(by = XTRTMT,
                      label = list(XTRTMT ~ "Groupe de traitement",
                                   BIRTHDT ~ "Age",
                                   RANDOMIZENUM ~ "Nombre de  randomiser",
                                   TRTMT ~ "Code de traitement",
                                   VISID ~ "Visite",
                                   CLASS ~ "Classe",
                                   GENDER ~ "Genre",
                                   RACE ~ "Race",
                                   RELGION ~ "Religion"),                  # renomer les variables
                      statistic = list(all_continuous() ~ "{mean} ({sd})", # garder les conditions statistiques
                                       all_categorical() ~ "{n} / {N} ({p}%)"), # tout catégorie
                      digits = all_continuous() ~ 2,
                      missing_text = "(Manquant)") %>%  # renomer les elements restants ou manquants
  modify_header(label ~ "**Variable**") %>% # renomer le titre des variables
  modify_spanning_header(c("stat_1", "stat_2","stat_3", "stat_4") ~ "**Analyse de Traitement**") %>% # Titre suivant les colonnes
  modify_caption("**Table 1. Carasteristique du patient**") %>% bold_labels() %>% add_p()
 
```


```{r}
### Création du tableau avec tableOne
library(tableone)
library(survival)
faot <- c("XTRTMT")
vass <- c("RANDOMIZENUM", "TRTMT", "VISID", "CLASS", "GENDER", 
"BIRTHDT", "RACE", "RELGION")
tableOne_1 <- CreateTableOne(vars = vass, data = tot.1, factorVars = faot, strata = "XTRTMT")
tableOne_1
```













```{r}
# la question 2 sur le nombre de test positif durant les semaines
install.packages("lattice")

A1 <- URINETOX %>% select(SUBJID,VISID,URDRUGS,URRESULT) %>% filter(URDRUGS == 7)
A1 <- A1 %>% select(- URDRUGS) 
A1 <- A1 %>% filter(VISID == c("WEEK1V1","WEEK2V1","WEEK3V1","WEEK4V1","WEEK5V1",
                                                     "WEEK6V1","WEEK7V1","WEEK8V1"))



# Il faut que j'arrive à combiner RDMCODE à A1
cv <- RDMCODE %>% select(SUBJID,XTRTMT)
lg <- full_join(cv,A1, by = "SUBJID")


#pour le week 1
W1 <- lg %>% filter(VISID == "WEEK1V1")
T1 <- table(W1$URRESULT, W1$XTRTMT)
T1


#pour le week 2
W2 <- lg %>% filter(VISID == "WEEK2V1")
T2 <- table(W2$URRESULT, W2$XTRTMT)
T1

#pour le week 3
W3 <- lg %>% filter(VISID == "WEEK3V1")
T3 <- table(W3$URRESULT, W3$XTRTMT)
T3

#pour le week 4
W4 <- lg %>% filter(VISID == "WEEK4V1")
T4 <- table(W4$URRESULT, W4$XTRTMT)
T4

# Comment je pourrai combiner les 4 T pour faire un graphe 4 courbes

sss <- lg %>% select(- SUBJID) %>% filter(XTRTMT == "Placebo") # que pour Placebo

table(sss$VISID,sss$URRESULT)

```




```{r}
library(gtsummary)
library(flextable)

tb1 <- tbl_summary(lg, by = VISID)
                       
##label = list(lg$XTRTMT, lg$URRESULT))


#Fonction de  la representation  
library(ggplot2)
library(wesanderson)
library(tidyverse)
library(pander)

#ggplot(lg, aes(VISID,URRESULT)) + geom_abline(aes(VISID,URRESULT)) + labs(title = " Representation graphique")
  



```






```{r}
# Calcul de la proportion de test + des 8 semaines de chaque traitement

N_pos <- lg %>% filter(lg$URRESULT == 1)
table(N_pos$XTRTMT,N_pos$VISID)

# Calcul de la proportion de test - des 8 semaines de chaque traitement
N_pot <- lg %>% filter(lg$URRESULT == 0)
table(N_pot$XTRTMT,N_pot$VISID)



```


```{r}
#  OBJECTIF SECONDAIRE
### Determination du nombre d'échantillon d'urine negatif 
Urns <- select(URINETOX,URDRUGS, URRESULT)
Urnn <- filter(Urns, URDRUGS == c(1,2,3,4,5,9), URRESULT == 0)
Repons3 <- pander(table(Urnn$URDRUGS,Urnn$URRESULT))
##Graphe d'appui
hist(Urnn$URDRUGS, xlab = "Produits de test", ylab = "Effectif totale", main = "Echantillon d'urine negatif", probability = F, ylim = c())
```


```{r}
## Détermination du nombre d'alcool test negatif
# Faisons appel à la base ALBREATH

ALBREATH <- read.csv("C:/Users/LENOVO/Documents/MASTER/M2/Application pratique/A applications pratiques/CSV/ALBREATH.csv", header = T, sep = ",")

head(ALBREATH)


```




```{r}
#Non-consommation faisant objet d'abus
demog <- DEMOG %>% select(SUBJID,GENDER,MOSTPROB,SUBSTANC)

demog <- demog %>% dplyr::filter(., SUBSTANC==11)
table(demog$MOSTPROB) # le taux de la methephtamine est bien plus élévé

```

