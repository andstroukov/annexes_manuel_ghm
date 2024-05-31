#' Extraction, de l'annexe 4 du manuel des GHM (PDF)
#' Correspondance entre le code CIM 10 de la CMA et le numéro de liste d'exclusion
#'

#' @param chemin_fichier_pdf chemin où se trouve le volume 1 du manuel des GHM
#' @param annee année de publication du manuel

fn_extraction_annexe_4_mghm <- function(chemin_fichier_pdf,
                                        annee
){
  
  require(tidyverse)
  require(pdftools)
  
  message("\nExtraction des données - ce script prends une dizaine de secondes à être exécuté\n.")
  
  
  # Extraction des données brutes du fichier PDF
  
  
  
  ex_pdf <- pdf_text(pdf = chemin_fichier_pdf)
  extra_pdf<-pdf_data(pdf = chemin_fichier_pdf)
 
  for (i in 1:length(ex_pdf)) {
    if (str_detect(ex_pdf[[i]],"Annexe 4-")==FALSE) {
      next
    }
    pages1<-tibble(num=i)
    pages<-rbind(pages,pages1)%>%filter(!is.na(num))
  }
  rm(pages1)
 
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
  
  tbl<-tab%>%
    mutate(cim1=str_replace(cim,"\\.",""))%>%
    filter(cim!="contenu")%>%
    select(cim1,liste_exclu)
  
  

  
  # Suppression des lignes ne commençant pas par un code CIM 10
  tb_a_modifier <-
    tb_a_modifier %>%
    filter(str_detect(string=ligne_sans_espace_deb,pattern="^[:alpha:][:digit:][:digit:]"))
  
  tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion <-
    tb_a_modifier %>%
    separate(col = ligne_sans_espace_deb,into = c("code_cim_10","niveau_severite","num_liste_excl"),extra = "drop") %>%
    select(code_cim_10,num_liste_excl) %>%
    distinct() %>%
    rename("code_cim_10_cma"="code_cim_10","num_liste_exclusion_de_la_cma"="num_liste_excl")
  
  
  
  
  list(tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion= tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion,
       tb_cim_10_comp = tb_cim_10_comp
  )
}


# 
# 
liste_export <-
fn_extraction_annexe_4_mghm(
# 
chemin_fichier_pdf = "C:/Users/4011297/Documents/R/Manuel_GHM_extractions_annexes/man_ghm_2023_vol_1.pdf",
annee = 2023 )
# 
# 
saveRDS(liste_export$tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion,"tb_annexe_4_liste_cma_et_num_liste_exclusion.rds")
write_csv2(liste_export$tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion,"tb_annexe_4_liste_cma_et_num_liste_exclusion.csv")

saveRDS(liste_export$tb_cim_10_comp,"tb_cim_10_comp.rds")
write_csv2(liste_export$tb_cim_10_comp,"tb_cim_10_comp.csv")
# 
