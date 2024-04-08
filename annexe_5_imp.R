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
# Separer Parties 1 et 2 de l'Annexe 5 ####
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
pg<-extra_pdf[[431]]

# Plus compliqué que annexe 4 ou annexe 5-2
# attention page 391 vers le haut: reste de la liste précédente (11) s'affiche toujours au dessus de la liste 12
# ce qui peut fait attribuer par erreur les codes à la mauvaise liste
# hypothèse : croiser l'extraction par ligne et l'extraction par coordonnées x,y ?
#
# Détail import partie 1####
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
##  Codes de la page avec leur valeur y
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
## Codes diagnostics et liste d'exclusion
# ajout de numéro de la page précédente si y min de code < y min de debut de liste
full<-list_pg%>%
  left_join(.,y_list,by)%>%
  mutate(lst2=if_else(is.na(lst)&y<ymin_p,min_list-1,lst))
#
#
## liste de codes agrégés et de numéros de listes ####
# 4 colonnes: liste et code(agrégés), coordonnées x et y
#
by=join_by(y >= ymin, y <= ymax)
commune<-tibble(lst2=NA,cod=NA,x=NA,y=NA)
#
for (i in min(pages1):max(pages1)) {
  pg<-extra_pdf[[i]]
  ymin<-pg%>%
    filter(x==106,str_detect(text,"^[A-Z]"))%>%
    select(ymin=y)%>%
    arrange(ymin)
  ymin_p<-min(ymin$ymin)
  ymax<-pg%>%
    filter(x>73,x<92,str_detect(text,"^\\s*[0-9]*\\s*$"))%>%
    mutate(lst=as.integer(text))%>%
    select(ymax=y,lst)%>%
    arrange(ymax)
  y_list<-ymin%>%
    bind_cols(.,ymax)
  min_list=min(y_list$lst)
  list_pg<-pg%>%
    filter(str_detect(text,"^[A-Z]"),y<795)%>% # pour exclure la dernière ligne de texte
    select(x,y,cod=text)
  full<-list_pg%>%
    left_join(.,y_list,by)%>%
    mutate(lst2=if_else(is.na(lst)&y<ymin_p,min_list-1,lst))%>%
    select(lst2,cod,x,y)
  commune<-commune%>%
    bind_rows(.,full)%>%
    filter(lst2>0)%>%
    distinct()
}
#
## Nettoyage des codes agrégés ####
#
## Recoller les intervalles "-" ####
# separés par: A. les saut de lignes
#              B. les sauts Des pages
#
# pour la même liste, il faut recoller les tirets: "A1-" et "-B2" pour ne pas perdre le contenu du milieu
# pour la liste 9, se voit pour 2 codes: B34.0 - B34.4 et R68.8-R70
#  
# le code suivant se trouve par le X minimal (=103) parmis les codes
# et Y "pas + 1" à +11 ou +12 par rapport au précédent. C'est la coordonnée "Y" la plus proche:
#
# A . Le saut de ligne Même page ####
#
by2=join_by(lst2,closest(y<y2)) # tiret et 2nd code à la même page

# 1re partie de la jointure "-" de la même page: se termine par "-"
# y=756 ou 758, pas concerné, c'est le saut de page
c1<-commune%>%
  filter(str_detect(cod,"-$"),y<756)

# 2nde partie de jointure "-" de la même page: x=103, delta y>=12
c2<-commune%>%filter(x==103)

# jointure par y plus proche
c3<-c1%>%
  left_join(.,c2%>%rename(y2=y),by2)%>%
  left_join(.,c2%>%filter(y==73,x==103)%>%select(lst2,cod2=cod))%>%
  mutate(cod=if_else(!is.na(cod.y),paste0(cod.x,cod.y),paste0(cod.x,cod2)))%>%
  select(lst2,cod,x=x.x,y) # x et y de la 1re partie "A10-" 

# B. Le saut de page ####
#
# tiret et 2nd code à la page suivante: y ==73 & x==103
#
c3bis<-commune%>%
  filter(str_detect(cod,"-$"),y>745,x>460)%>%
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
  filter(!str_detect(cod,"-$"))
#
# Preparer listes sans doublons pour 2 fonctions:
## A. avec tiret A4-A5

atir<-commune2%>%
  select(lst2,cod)%>%
  filter(str_detect(cod,"-"))
#
# replacer les points/asterisc
rep_str=c("\\*"="0","\\."="")
atir$cod<-str_replace_all(atir$cod,rep_str)
#
# laisser les combines uniques
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
at2<-at%>%
  slice(979:979)%>% # ici, blocage car "R05-NA", problème tableau 3 bis incomplet et 3 avec "R05-NA"
  mutate(liste_cd=map_chr(.$cod,fn_ival))
#
# Problème: pour les listes 608 et 1018, les "-" à la fin n'ont pas été reperés.
# repérés pour listes 142 et 506
#
## B. sans tiret A40

## Import CIM10
library(nomensland)
#
tb_cim_10_comp <-
  nomensland::get_table("cim")%>%
  filter(anseqta==2023) %>%
  select(code,lib_long) %>%
  arrange(code)

tb_lettre <- tibble(lettre = LETTERS, ordre = 1:length(LETTERS))


################

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
#
# Function practicum ####
library(gapminder)
library(ggplot2)
library(tidyverse)
#
j_country <- "Australia" # pick, but do not hard wire, an example
(j_dat <- gapminder %>% 
    filter(country == j_country))
unique(gapminder$country)
p <- ggplot(j_dat, aes(x = year, y = lifeExp))
p + geom_point() + geom_smooth(method = "lm", se = FALSE)
j_fit <- lm(lifeExp ~ year, j_dat)
coef(j_fit)
j_fit <- lm(lifeExp ~ I(year - 1952), j_dat)
coef(j_fit)
#
le_lin_fit <- function(dat, offset = 1952) {
  the_fit <- lm(lifeExp ~ I(year - offset), dat)
  coef(the_fit)
}
le_lin_fit(j_dat)
#
le_lin_fit <- function(dat, offset = 1952) {
  the_fit <- lm(lifeExp ~ I(year - offset), dat)
  setNames(coef(the_fit), c("intercept", "slope"))
}
le_lin_fit(1954)
#
j_country <- "Zimbabwe"
j_dat <- gapminder %>% filter(country == j_country)
#
j_dat<-gapminder%>%filter(country=="Zimbabwe")
#
p <- ggplot(j_dat, aes(x = year, y = lifeExp))
#
p + geom_point() + geom_smooth(method = "lm", se = FALSE)
le_lin_fit(j_dat)
#
library(tidyverse)
# purr:: nest data
n_iris<-iris%>%
  group_by(Species)%>%
  nest()
str(n_iris)
n_iris$data[[2]]
n_iris$data[2]
n_iris$data[n_iris$Species=="versicolor"]%>%
  print(n=3)
n_iris%>%
  unnest()
# work with lists
mod_fun <- function(df)
  lm(Sepal.Length ~ ., data = df)
m_iris <- n_iris %>%
  mutate(model = map(data, mod_fun))
str(m_iris)
m_iris$model[[2]]
#
# simplify
b_fun <- function(mod)
  coefficients(mod)[[1]]
m_iris %>% transmute(Species,
                     beta = map_dbl(model, b_fun))
help(tribble)
trb<-tribble(
  ~x,  ~y,
  "a", 1:3,
  "b", 4:6
)
str(trb)
trb2<-tibble(max = c(3, 4, 5), seq = list(1:3, 1:4, 1:5))
trb3<-enframe(list('3'=1:3, '4'=1:4, '5'=1:5), 'max', 'seq')
str(trb3)
mtcars%>%head()
str(mtcars)
mtcars %>% mutate(seq = map(cyl, seq))
#
sm<-mtcars %>% group_by(cyl) %>%
  summarise(q = list(quantile(mpg)))
sm$q[[2]]
ni2<-n_iris %>% mutate(n = map(data, dim))
ni2$n[3]
#
m_iris %>% mutate(n = map2(data, model, list))
#
nk2<-m_iris %>% mutate(n = map2(data, model, list))
nk2$n[[2]][[2]]
