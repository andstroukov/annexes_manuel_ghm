#' Extraction, de l'annexe 4 du manuel des GHM
#' code CIM 10 , CMA, numéro de liste d'exclusion

library(tidyverse)
library(pdftools)

# Importer csv pour comparer ####
comp<-read.csv2("~/R/Manuel_GHM_extractions_annexes/tb_annexe_4_liste_cma_et_num_liste_exclusion.csv")%>%
  rename(cim1=code_cim_10_cma,liste_=2)
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
# comment trouver la-dedans les pages "Annexe 4-"
# trouver les numeros de pages ####
# pour lesquelles "Annexe 4-" present: limiter aux pages utiles

pages<-tibble(num=NA)

for (i in 1:length(ex_pdf)) {
  if (str_detect(ex_pdf[[i]],"Annexe 4-")==FALSE) {
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
# Page test si erreur d'import ####
#
pg<-extra_pdf[[339]]
#
p12<-pg%>%
  filter(x>73,x<86)%>%
  select(y,cim=text)%>%
  left_join(.,pg%>%
              filter(x>156,x<169)%>%
              select(y,list=text))
#
# Extraction Annexe 4 de diag/liste exclu ####
# avec boucle for et selon position x
#
tab<-tibble(cim=NA,liste_exclu=NA,page=NA)
#
for (i in min(pages):max(pages)) {
  tab1<-extra_pdf[[i]]%>%
    filter(x>73,x<86)%>%
    select(y,cim=text)%>%
    left_join(.,extra_pdf[[i]]%>%
                filter(x>156,x<169)%>%
                select(y,liste_exclu=text))%>%
    select(cim,liste_exclu)
  tab<-tab%>%
    bind_rows(.,tab1)%>%
    filter(!is.na(cim),!is.na(liste_exclu))
}
rm(tab1)
#
tbl<-tab%>%
  mutate(cim1=str_replace(cim,"\\.",""))%>%
  filter(cim!="contenu")%>%
  select(cim1,liste_exclu)
  
#
## verif la différence ####
# si certaines lignes ne sont pas importées

dif<-comp%>%
  anti_join(.,tbl)

# Export tableau vérifié Annexe 4 ###
#
write.csv2(tbl,file="tb1_annexe_4.csv",row.names = F)


