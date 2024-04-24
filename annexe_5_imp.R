#' Extraction, de l'annexe 5 du manuel des GHM
#' code CIM 10 , CMA, numéro de liste d'exclusion

library(tidyverse)
library(pdftools)
library(nomensland)
# 
## Import CIM10
#
tb_cim_10_comp <-
  nomensland::get_table("cim")%>%
  filter(anseqta==2023) %>%
  select(code) %>% # sans libellé "lib_long"
  arrange(code)
#
# Lettres dans l'ordre pour retrouver les numéros des listes
tb_lettre <- tibble(lettre = LETTERS, ordre = 1:length(LETTERS))
#
# Extraction des données brutes du fichier PDF ####

# sous forme de large character
#
ex_pdf <- pdf_text(pdf ="C:/Users/4011297/Documents/R/Manuel_GHM_extractions_annexes/man_ghm_23_vol_1.pdf")
#
# sous forme de large list
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
# Separer Parties 1 et 2 de l'Annexe 5 #
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
###############################################################################
#                                     ANNEXE
#                                     5-1
###############################################################################
#
# coordonnées x du numéro de liste: 85 ou 91 (2 chiffres)
#
## liste de codes agrégés et de numéros de listes ###
# 4 colonnes: liste et code(agrégés), coordonnées x et y
#
by=join_by(y >= ymin, y <= ymax)# jointure par les coordonnées
#
## Import tableau avec coordonnées
commune<-tibble(lst2=NA,cod=NA,x=NA,y=NA)
#
for (i in min(pages1):max(pages1)) {
  pg<-extra_pdf[[i]]
  ymin<-pg%>%     # valeur "y" min du code avec coordonnée x=103 
    filter(x==106,str_detect(text,"^[A-Z]"))%>%
    select(ymin=y)%>%
    arrange(ymin)
  ymin_p<-min(ymin$ymin) # valeur pour trouver fin de liste p. préced en haut de p. suivante
  ymax<-pg%>%     # valeur "y" max au même niveau "y" que le numéro de liste (tout numérique)
    filter(x>73,x<92,str_detect(text,"^\\s*[0-9]*\\s*$"))%>%
    mutate(lst=as.integer(text))%>%
    select(ymax=y,lst)%>%
    arrange(ymax)
  y_list<-ymin%>% # intervals ymin-ymax valeurs "y" pour une liste donnée
    bind_cols(.,ymax)
  min_list=min(y_list$lst) # numéro liste minimale de la page donnée
  list_pg<-pg%>%  # extraction codes
    filter(str_detect(text,"^[A-Z]"),y<795)%>% # pour exclure la dernière ligne de texte
    select(x,y,cod=text)
  full<-list_pg%>% # ajout numéro de liste aux codes avec coordonnées de la page
    left_join(.,y_list,by)%>% # jointure par y entre ymin et ymax
    mutate(lst2=if_else(is.na(lst)&y<ymin_p,min_list-1,lst))%>% # numero liste = min-1 si abs
    select(lst2,cod,x,y)
  commune<-commune%>% # enchainement des pages
    bind_rows(.,full)%>%
    filter(lst2>0)%>%
    distinct()
}
#
## Nettoyage des codes agrégés (p ex "A3-A5") ###
#
## Recoller les intervalles "-" ###
# separés par: A. les saut de lignes
#              B. les sauts Des pages
#
# pour la même liste, il faut recoller les tirets: "A1-" et "B2" ("A1-B2") pour ne pas perdre le contenu du milieu
# pour la liste 9, se voit pour 2 codes: B34.0 - B34.4 et R68.8-R70
#  
# le code suivant se trouve par le X minimal (=103) parmis les codes
# et Y "pas + 1" à +11 ou +12 par rapport au précédent. C'est la coordonnée "Y" la plus proche:
#
# A . Le saut de ligne Même page #
#
by2=join_by(lst2,closest(y<y2)) # jointure sur (tiret + 2nd code à la même page)

# 1re partie de la jointure "-" de la même page: se termine par "-"
# y=756 ou 758, pas concerné, c'est le saut de page
c1<-commune%>%
  filter(str_detect(cod,"-$"),y<756)

# 2nde partie de jointure "-" de la même page: x=103, delta y>=12
c2<-commune%>%filter(x==103)

# jointure par "y" superieur le plus proche
c3<-c1%>%
  left_join(.,c2%>%rename(y2=y),by2)%>%
# même liste, y sup le plus proche, même page; manquent code de fin pour listes 608 et 1218 
  left_join(.,c2%>%filter(y==73,x==103)%>%select(lst2,cod2=cod))%>%
  mutate(cod=if_else(!is.na(cod.y),paste0(cod.x,cod.y),paste0(cod.x,cod2)))%>%
  select(lst2,cod,x=x.x,y) # x et y de la 1re partie "A10-" 

# B. Le saut de page ####
#
# tiret et 2nd code à la page suivante: y ==73 & x==103
# 
c3bis<-commune%>%
  filter(str_detect(cod,"-$"),y>747,x>460)%>% # si y=747 alors avant dernière ligne et erreur!
  rename(cod1=cod)%>%
  left_join(.,commune%>%filter(y==73,x==103)%>%select(lst2,cod2=cod))%>%
  mutate(cod=paste0(cod1,cod2))%>%
  select(lst2,cod,x,y)

# Commune avec liste "-" rompues et reconstituées ####
#
# supprimer les "-" à la fin: "Q40-"
commune2<-commune%>%
  bind_rows(.,c3)%>%
  bind_rows(.,c3bis)%>%
  select(lst2,cod)%>%
  distinct()%>%
  filter(!str_detect(cod,"-$"),!str_detect(cod,"NA"))
#
# Preparer listes sans doublons pour 2 fonctions:
## A. avec tiret A4-A5
#
atir<-commune2%>%
  select(lst2,cod)%>%
  filter(str_detect(cod,"-"))
#
# replacer les points/asterisc
rep_str=c("\\*"="0","\\."="")
atir$cod<-str_replace_all(atir$cod,rep_str)
#
# laisser les combines/couples/intervalles des codes uniques
at<-atir%>%
  select(cod)%>%
  distinct()
#
## fonction pour sortir la totalité des codes CIM10 de l'intervalle
# A1-A5
#
fn_ival <- function(cod){
code_depart <-
  str_split_1(cod,pattern = "-")[1]

code_fin <-
  str_split_1(cod,pattern = "-")[2]

indice_debut <-
  min(which(str_detect(string = tb_cim_10_comp$code,pattern = code_depart)))

indice_fin <-
  max(which(str_detect(string = tb_cim_10_comp$code,pattern = code_fin)))

tb_cim_10_comp$code[indice_debut:indice_fin] %>%
  paste0(collapse = " ")
}
#
# remplacer les intervalles "A1-A5" par les vecteurs de codes "A1-A2-A3-A4-A5"
at2<-at%>%
  mutate(liste_cd=map_chr(.$cod,fn_ival))
#
## B. sans tiret A40
stir<-commune2%>%
  select(lst2,cod)%>%
  filter(!str_detect(cod,"-"))
# remplacer les points
stir$cod<-str_replace_all(stir$cod,rep_str)
#
# Fonction transformation de code(père) CIM (p ex A41)
fn_transfo_unique<- function(unique){
  
  tb_cim_10_comp$code[str_detect(string = tb_cim_10_comp$code,pattern = unique)] %>%
    paste0(collapse = " ")
}
#
stir2<-stir%>%
  select(cod)%>%
  distinct()%>%
  mutate(liste_cd=map_chr(.$cod,fn_transfo_unique))
#
## versions verticale et horizontale ###
# vecteurs chr horizontaux transformés en verticaux
#
horiz<-stir2%>%
  bind_rows(.,at2)
#
commune2$cod<-str_replace_all(commune2$cod,rep_str) # rep_str cf ligne 164
#
reun<-commune2%>%
  left_join(.,horiz)%>%
  mutate(code_cim_10 = str_split(liste_cd," "))%>%
  unnest(cols = c(code_cim_10))%>%
  rename(liste_ex=lst2,cod_interv=cod,liste_cim_10=liste_cd)
#
str(reun)
#
# ecrire .csv trop long >1million238K lignes
ann_5_1<-reun%>%
  select(liste_ex,code_cim_10)%>%
  distinct()
str(ann_5_1)
#
write_csv2(ann_5_1,file = "ann_5_1.csv")
# 56.81 sec
#
##################################################################################################################
#               ANNEXE  
#               5-2
########################## ########################################################################
#
# 2 procédés: 1.avec boucles "for" et 2.avec fonction + "map"
#
# Partie commune des 2 procédés
#
library(tidyverse)
library(pdftools)
library(nomensland)
library(tictoc)
#
# Extraction des données brutes du fichier PDF ####

# sous forme de large character
#
ex_pdf <- pdf_text(pdf ="C:/Users/4011297/Documents/R/Manuel_GHM_extractions_annexes/man_ghm_23_vol_1.pdf")
#
# sous forme de large list
#
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
# Separer Parties 1 et 2 de l'Annexe 5 #
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
### Tableau des racines de GHM #
tb_rghm <-
  nomensland::get_table("ghm_rghm_regroupement") %>%
  filter(anseqta==2023) %>%
  select(racine) %>% 
  rename("code_racine"="racine")
#
pages2
#
## Fonction extraction toutes les 2 pages de l'annexe 5-2
#
fn_extract_tab_pages <- function(num_page){
  tibble(ligne=
           ex_pdf[[num_page]] %>%
           str_split("\\n") %>%
           unlist() )
}
# laisser les lignes commençant par espace(1 ou+) suivi de chiffre
#
extr_tab_pages <- 
  map_df(min(pages2):max(pages2),fn_extract_tab_pages)%>%
  filter(str_detect(ligne,"^[:space:]+(?=[:digit:])"))
#
tb_a_modifier <-
  extr_tab_pages %>%
  mutate(ligne_sans_espace_deb= str_replace_all(string  = ligne,pattern="^ *",replacement = ""))%>%
  separate(ligne_sans_espace_deb,c("num_liste_exclusion","liste_rghm_exclusion"),sep = " ",extra="merge")%>%
  mutate(liste_rghm_exclusion = str_split(liste_rghm_exclusion," "),ligne=NULL)%>%
  unnest(cols = c(liste_rghm_exclusion))
#
# fonction transformation regroupement en racine 
#
fn_tf <- function(lst){
  
  if(lst %in% tb_rghm$code_racine){res_rghm <-  lst}
  
  if(str_sub(lst,1,3) == "CMD"){
    res_rghm <-  
      tb_rghm %>%
      filter(str_sub(code_racine,1,2)==str_sub(lst,4,5)) %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  }
  if(str_detect(lst,"Racines_en_")==TRUE){
    res_rghm <-
      tb_rghm %>%
      filter(str_sub(code_racine,3,3)==str_sub(lst,12,12)) %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  }
  
  if(str_detect(lst,"Sous_CMD")==TRUE){
    res_rghm <-
      tb_rghm %>%
      filter(str_sub(code_racine,1,2)==str_sub(lst,9,10),
             str_sub(code_racine,3,3)==str_sub(lst,12,12)) %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  }
  
  res_rghm
  
}
#
tb_annexe_5_2 <-tb_a_modifier %>%
  mutate(liste_rghm_exclusion = map_chr(.$liste_rghm_exclusion,fn_tf))%>%
  mutate(liste_rghm_exclusion = str_split(liste_rghm_exclusion," ")) %>%
  unnest(cols = c(liste_rghm_exclusion)) %>%
  rename("num_liste_exclusion_de_la_cma"="num_liste_exclusion","code_rghm_excluant_la_liste_de_cma"="liste_rghm_exclusion") %>%
  distinct()
#
## Export tableau Annexe 5 _ 2 #
#
write.csv2(tb_annexe_5_2,file="tb1_annexe_5_2.csv",row.names = F)
#
########## Annexe 5-2: Autre procédé (pdf_table, boucle for) ################################################
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
# Remplacer les regroupements des racines GHM par les listes des racines GHM ####
#
## Remplacer les CMD par vecteur correspondant des racines
# "CMD26" > "26C02 26M02..."
cmd<-tab%>%
  filter(str_detect(rghm,"^CMD"))%>%
  mutate(cr=str_sub(rghm,-2))%>%
  select(cr)%>%
  distinct()%>%
  pull()
#
for (i in cmd){
  assign(paste0("CMD",i),tb_rghm%>%
           select(code_racine)%>%
           filter(str_sub(code_racine,1,2)==i)%>%
           pull())
}
#
## racines en C,K,M
#
codrac<-tab%>%
  filter(str_detect(rghm,"Racines_en_"))%>%
  mutate(cr=str_sub(rghm,-1))%>%
  select(cr)%>%
  distinct()%>%
  pull()
#
for (i in codrac){
  assign(paste0("r_",i),tb_rghm%>%
           select(code_racine)%>%
           filter(str_sub(code_racine,3,3)==i)%>%
           pull())
}
#
## Sous CMD (en C)
#
sous<-tab%>%
  filter(str_detect(rghm,"Sous_CMD"))%>%
  mutate(sr=str_sub(rghm,-4),sr2=str_replace(sr,"_",""))%>%
  select(sr2)%>%
  distinct()%>%
  pull()

for (i in sous){
  assign(paste0("s",i),tb_rghm%>%
           select(code_racine)%>%
           filter(str_sub(code_racine,1,3)==i)%>%
           pull())
}
#
## Remplacer les groupes par les listes obtenues ##
#
tab2<-tab%>%
  mutate(r2=case_when(
    str_detect(rghm,"^CMD")~str_c(CMD26,collapse = " "),
    str_detect(rghm,"Racines_en_C")~str_c(r_C,collapse = " "),
    str_detect(rghm,"Racines_en_K")~str_c(r_K,collapse = " "),
    str_detect(rghm,"Racines_en_M")~str_c(r_M,collapse = " "),
    str_detect(rghm,"Sous_CMD02")~str_c(s02C,collapse = " "),
    str_detect(rghm,"Sous_CMD21")~str_c(s21C,collapse = " "),
    str_detect(rghm,"Sous_CMD22")~str_c(s22C,collapse = " "),
    TRUE~rghm),
    racine = str_split(r2," "))%>%
  unnest(cols = c(racine))
#
ann_5_2<-tab2%>%
  select(liste_ex=liste,racine)%>%
  distinct()
#
write_csv2(ann_5_2,"ann_5_2.csv")
