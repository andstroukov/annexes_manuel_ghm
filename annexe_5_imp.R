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
pg<-extra_pdf[[390]]

# Plus compliqué que annexe 4 ou annexe 5-2
# attention page 391 vers le haut: reste de la liste précédente (11) s'affiche toujours au dessus de la liste 12
# ce qui peut fait attribuer par erreur les codes à la mauvaise liste
# hypothèse : croiser l'extraction par ligne et l'extraction par coordonnées x,y ?
#
# Import Partie 1 ####
#

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


