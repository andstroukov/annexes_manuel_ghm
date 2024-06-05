#' Extraction, de l'annexe 4 du manuel des GHM
#' code CIM 10 , CMA, numéro de liste d'exclusion

library(tidyverse)
library(pdftools)
library(tictoc)
# 
# Extraction des données brutes du fichier PDF

# sous forme de large character ####
#
tic()
# 
an<-2023
#
chemin_pdf<-paste0("~/R/Manuel_GHM_extractions_annexes/man_ghm_",an,"_vol_1.pdf")
#
extra_pdf<-pdf_data(pdf = chemin_pdf)
#
fn_reperage_pages<-function(num_page){
  pg<-extra_pdf[[num_page]]
  pg%>%
    select(y,text)%>%
    filter(str_detect(text,"4-"),y>790)%>%
    mutate(num=if_else(!is.na(text),num_page,NA))%>%
    filter(!is.na(num))%>%
    select(num)
}
# 
pages<-map_df(1:pdf_info(chemin_pdf)$pages,fn_reperage_pages)
pages<-pages%>%
  distinct()
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
tbl <- map_df(min(pages):max(pages),fn_extr_tab_pages)%>%
    mutate(cim1=str_replace(cim,"\\.",""))%>%
    filter(cim!="contenu",!is.na(liste_exclu))%>%
    select(cim1,liste_exclu)

# Export tableau Annexe 4 ###
#
write.csv2(tbl,file=paste0("tb1_annexe_4_",an,".csv"),row.names = F)
toc()


