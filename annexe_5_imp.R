#' Extraction, de l'annexe 5 du manuel des GHM
#' code CIM 10 , CMA, numéro de liste d'exclusion

library(tidyverse)
library(pdftools)

# Importer csv pour comparer ####
diag<-read.csv2("~/R/Manuel_GHM_extractions_annexes/tb_annexe_5_1_liste_cma_et_diags_excluants.csv")
#
rac<-read.csv2("~/R/Manuel_GHM_extractions_annexes/tb_annexe_5_2_liste_cma_et_rghm_excluantes.csv")
# 
# Extraction des données brutes du fichier PDF

# sous forme de large character ####
#
ex_pdf <- pdf_text(pdf ="C:/Users/4011297/Documents/R/Manuel_GHM_extractions_annexes/man_ghm_23_vol_1.pdf")
#
length(ex_pdf)# nb pages
#
# sous forme de large list ####
extra_pdf<-pdf_data(pdf = "~/R/Manuel_GHM_extractions_annexes/man_ghm_23_vol_1.pdf")
#
# Trouver les numeros de pages ####
# pour lesquelles "Annexe 5-" present: limiter aux pages utiles

pages<-tibble(num=NA)

for (i in 1:length(ex_pdf)) {
  if (str_detect(ex_pdf[[i]],"Annexe 5-")==FALSE) {
    next
  }
  pages1<-tibble(num=i)
  pages<-rbind(pages,pages1)%>%filter(!is.na(num))
    }
rm(pages1)
#
str(pages)
min(pages)
max(pages)
#
# Separer Parties 1 et 2 ####
# Trouver la page avec mention "Partie 2" qui sépare les parties 5-1 et 5-2
#
for (i in min(pages):max(pages)) {
  if (str_detect(ex_pdf[[i]],"Partie 2")==FALSE) {
    next
  }
  print(i)
  lim=max(pages)-i+1
}
#
# num pages annexe 5-1
pages1<-pages%>%
  slice_head(n=nrow(pages)-lim)
# num pages annexe 5-2
pages2<-pages%>%
  slice_tail(n=lim)
#
# Page test####
#
pg<-extra_pdf[[389]]

# Plus compliqué que annexe 4 ou annexe 5-2
# attention page 391 vers le haut: reste de la liste précédente (11) s'affiche toujours au dessus de la liste 12
# ce qui peut fait attribuer par erreur les codes à la mauvaise liste
# hypothèse : croiser l'extraction par ligne et l'extraction par coordonnées x,y ?
#
# Import Partie 1 ####
#
# d'abord, extraire les numéros de liste et leur valeur Y pour distribuer les codes
# y ont la valeur max pour la dernière ligne
pg<-extra_pdf[[391]]
#
# coordonnées x du numéro de liste: 85 ou 91 (2 chiffres)
# coordonnées x du premier code de la liste: = 106, à partir de son y et jusqu'à y du numéro de liste - c'est 

# y minimal de la page correspond au code avec x=106 et commençant par une lettre majuscule
ymin<-pg%>%
  filter(x==106,str_detect(text,"^[A-Z]"))%>%
  select(ymin=y)%>%
  arrange(ymin)
ymin_p<-min(ymin$ymin)
#
str(ymin)
# y max correspond à celui du numéro de liste indiqué dans la colonne de gauche;
# il peut être faux pour la dernière ligne de la page si la liste se prolonge à la page suivante
ymax<-pg%>%
  filter(x>73,x<92,str_detect(text,"^\\s*[0-9]*\\s*$"))%>%
  mutate(lst=as.integer(text))%>%
  select(ymax=y,lst)%>%
  arrange(ymax)
str(ymax)
range(ymax$lst)
# valeurs Y min et max pour tester par la boucle "for"
y_list<-ymin%>%
  bind_cols(.,ymax)
str(y_list)
min_list=min(y_list$lst)
#
# codes de la page avec leur valeur y
# pour la 2nde page, il ne faut pas limiter y>ymin car 2 lignes de codes de la liste de la page précédente
# si l'exclusion de minuscules ajoutée [^a-z], alors codes de type "F0" sautent
list_pg<-pg%>%
  filter(str_detect(text,"^[A-Z]"),y<795)%>% # pour exclure la dernière ligne de texte
  select(y,cod=text)
#
str(list_pg)
min(list_pg$y)
max(list_pg$y)
#
by=join_by(y >= ymin, y <= ymax)
#
full<-list_pg%>%
  left_join(.,y_list,by)%>%
  mutate(lst2=if_else(is.na(lst)&y<ymin_p,min_list-1,lst))
#
# Suite - boucle pour obtenir la correspondance de liste de codes agrégés et de numéros de listes
# fichier "full", colonnes utiles "lst2" et "cod"
#
################
#
#
# les codes peuvent être attribués selon leur Y s'il est >= ymin et <=ymax de la liste donnée

# Import Partie 2 ####
# Extraction Annexe 5-2 de rghm/liste exclu ###
# avec boucle for et selon position x
#
tab<-tibble(liste=NA,rghm=NA)
#
for (i in min(pages2):max(pages2)) {
  tab1<-extra_pdf[[i]]%>%
    filter(x==91|x==97)%>%
    select(y,liste=text)%>%
    left_join(.,extra_pdf[[i]])%>%
    filter(liste!=text)%>%
    select(liste,rghm=text)
  tab<-tab%>%
    bind_rows(.,tab1)%>%
    filter(!is.na(liste))
}
rm(tab1)
#
tbl<-tab%>%
  mutate(cim1=str_replace(cim,"\\.",""))%>%
  filter(cim!="contenu")%>%
  select(cim1,liste_exclu,page)
  
#
## verif la différence ####
# si certaines lignes ne sont pas importées

dif<-comp%>%
  anti_join(.,tbl)

# Export tableau vérifié Annexe 4 ###
#
write.csv2(tbl,file="tb1_annexe_5_1.csv",row.names = F)


