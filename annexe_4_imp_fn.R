#' Extraction, de l'annexe 4 du manuel des GHM
#' code CIM 10 , CMA, numéro de liste d'exclusion

library(tidyverse)
library(pdftools)
# 
# Extraction des données brutes du fichier PDF

# sous forme de large character ####
#
ex_pdf <- pdf_text(pdf ="C:/Users/4011297/Documents/R/Manuel_GHM_extractions_annexes/man_ghm_2023_vol_1.pdf")
#
length(ex_pdf)# nb pages
#
# sous forme de large list ####
extra_pdf<-pdf_data(pdf = "~/R/Manuel_GHM_extractions_annexes/man_ghm_2023_vol_1.pdf")
pdf_info("~/R/Manuel_GHM_extractions_annexes/man_ghm_2023_vol_1.pdf")$pages

fn_reperage_pages<-function(num_page){
  pg<-extra_pdf[[num_page]]
  pg%>%
    select(y,text)%>%
    filter(str_detect(text,"4-"),y>790)%>%
    mutate(num=if_else(!is.na(text),num_page,NA))%>%
    filter(!is.na(num))%>%
    select(num)
}

pg<-extra_pdf[[302]]
pg%>%
  select(y,text)%>%
  filter(str_detect(text,"4-"),y>790)#%>%
  mutate(num=if_else(!is.na(text),num_page,NA))%>%
  filter(!is.na(num))%>%
  select(num)

pages2<-map_df(1:pdf_info("~/R/Manuel_GHM_extractions_annexes/man_ghm_2023_vol_1.pdf")$pages,fn_reperage_pages)
pages2<-pages2%>%
  distinct()
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
#
# Extraction Annexe 4 de diag/liste exclu ####
# selon position x
#

fn_extr_tab_pages <- function(num_page){
  extra_pdf[[num_page]]%>%
    filter(x>73,x<86)%>%
    select(y,cim=text)%>%
    left_join(.,extra_pdf[[num_page]]%>%
                filter(x>156,x<169)%>%
                select(y,liste_exclu=text))%>%
    select(cim,liste_exclu)
}
#
tbl2 <- map_df(min(pages):max(pages),fn_extr_tab_pages)%>%
    mutate(cim1=str_replace(cim,"\\.",""))%>%
    filter(cim!="contenu",!is.na(liste_exclu))%>%
    select(cim1,liste_exclu)

# Export tableau vérifié Annexe 4 ###
#
write.csv2(tbl2,file="tb1_annexe_4.csv",row.names = F)


