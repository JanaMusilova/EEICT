rm(list=ls())  # smaz�n� cel�ho environment
library(rvest) # na�ten� knihovny pro scrapping

# sta�en� DB jednotliv�ch l�k� pro pozd�j�� stahov�n� informac�m t�chto jednotliv�ch l�k� na z�klad� jejich DB
a <- 1 # pomocn� prom�nn�
DB <- c() # nadefinov�n� vektoru, kam se budou v cyklu ukl�dat jednotliv� DB
odkaz_all_drugs <- c('https://www.drugbank.ca/unearth/q?utf8=%E2%9C%93&query=schizophrenia&searcher=drugs&approved=1&vet_approved=1&nutraceutical=1&illicit=1&withdrawn=1&investigational=1&button=','https://www.drugbank.ca/unearth/q?approved=1&button=&c=_score&d=down&illicit=1&investigational=1&nutraceutical=1&page=2&query=schizophrenia&searcher=drugs&vet_approved=1&withdrawn=1','https://www.drugbank.ca/unearth/q?approved=1&button=&c=_score&d=down&illicit=1&investigational=1&nutraceutical=1&page=3&query=schizophrenia&searcher=drugs&vet_approved=1&withdrawn=1')

for (i in 1:length(odkaz_all_drugs)){ # postupn� stahov�n� DB z celkem 4 webov�ch str�nek (l�k� je 80 rozd�len�ch do 4 stran) 
all_drugs <- read_html(odkaz_all_drugs[i])
DB[a:(a+24)] <- all_drugs %>% 
   html_nodes(".btn-card")%>%
   html_text()
a = a+25}
all_drugs4 <- read_html('https://www.drugbank.ca/unearth/q?approved=1&button=&c=_score&d=down&illicit=1&investigational=1&nutraceutical=1&page=4&query=schizophrenia&searcher=drugs&vet_approved=1&withdrawn=1')
DB4 <- all_drugs4 %>% 
  html_nodes(".btn-card")%>%
  html_text() 
pom <- length(DB4) # ulo�en� posledn� str�nky l�k�
DB[76:(75+pom)] <- DB4

# vytvo�en� pomocn�ch prom�nn�ch
Interactions <- character()
Targets <- character()
pomocna <- 1
Drugs <- matrix(0,length(DB),3)
odkaz <- "https://www.drugbank.ca/drugs/"

# postupn� stahov�n� dat jednotliv�ch l�k�
for (k in 1:length(DB)){ 
Drug <- read_html(paste0(odkaz,DB[k])) #sta�en� dat z pot�ebn� www

ATC <- Drug %>% # ulo�en� prom�nn� ATC
  html_node(css="tr:nth-child(51) > td > ul > li:nth-child(1) > a") %>% 
  html_text()

Int <- Drug %>% # sta�en� dat pro vyhodnocen� podm�nky IF
  html_node(css="table#drug-interactions")
Targ <- Drug %>% 
  html_node(css="table#moa-target-table")
if (is.na(Int) == FALSE & is.na(Targ) == FALSE & is.na(ATC) == FALSE){  # ulo�� c�le a interakce jenom kdy� existuje tabulka s interakcemi, c�li a ATC data
  Name <- Drug %>% # ulo�en� n�zvu l�ku
    html_node("td, strong")%>%
    html_text()
  Group <- Drug %>% # ulo�en� skupiny, do kter� l�k pat��
    html_node("tr:nth-child(5) td")%>%
    html_text()
  Targ <- Drug %>% # ulo�en� tabulky s c�li
    html_node(css="table#moa-target-table") %>%
    html_table()
    Targ[,ncol(Targ)+1] <- Name # p�id�n� n�zvu l�ku do posledn�ho sloupce
    Targets <- rbind(Targets, Targ)
  Int <- Drug %>% # ulo�en� interaguj�c�ch l�k�
    html_node(css="table#drug-interactions") %>%
    html_table()
    Int[,ncol(Int)+1] <- Name # p�id�n� n�zvu l�ku do posledn�ho sloupce
    Interactions <- rbind(Interactions, Int)

Drugs[pomocna,1] <- Name # zaps�n� informac� o l�c�ch do tabulky Drugs
Drugs[pomocna,2] <- Group
Drugs[pomocna,3] <- ATC
pomocna <- pomocna+1 # umo�n� zapisovat nov� l�k v�dy na dal�� ��dek v tabulce Drugs
}}

names(Interactions) <- c("Interacting drug","Interaction","Group", "Drug") # p�ejmenov�n� n�zv� sloupc� v tabulk�ch
names(Drugs) <- c("Name","Group", "ATC")
names(Targets)[names(Targets)=="V8"] <- "Drug"

Drugs <- Drugs[1:(pomocna-1),] # odstran�n� p�ebyte�n�ch ��dk� v tabulce (muselo b�t nadefinov�no tolik ��dk�, kolik je celkem l�k�, proto�e p�edem nev�me, kolik z nich bude sta�eno - kolik m� hodnoty v Targets a ATC)

library(DescTools) #na�ten� knihovny pro export do Excelu a p�eveden� dat do Excelu
XLView(Drugs)
XLView(Interactions) 
XLView(Targets)