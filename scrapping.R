rm(list=ls())  # smazání celého environment
library(rvest) # naètení knihovny pro scrapping

# stažení DB jednotlivých lékù pro pozdìjší stahování informacím tìchto jednotlivých lékù na základì jejich DB
a <- 1 # pomocná promìnná
DB <- c() # nadefinování vektoru, kam se budou v cyklu ukládat jednotlivá DB
odkaz_all_drugs <- c('https://www.drugbank.ca/unearth/q?utf8=%E2%9C%93&query=schizophrenia&searcher=drugs&approved=1&vet_approved=1&nutraceutical=1&illicit=1&withdrawn=1&investigational=1&button=','https://www.drugbank.ca/unearth/q?approved=1&button=&c=_score&d=down&illicit=1&investigational=1&nutraceutical=1&page=2&query=schizophrenia&searcher=drugs&vet_approved=1&withdrawn=1','https://www.drugbank.ca/unearth/q?approved=1&button=&c=_score&d=down&illicit=1&investigational=1&nutraceutical=1&page=3&query=schizophrenia&searcher=drugs&vet_approved=1&withdrawn=1')

for (i in 1:length(odkaz_all_drugs)){ # postupné stahování DB z celkem 4 webových stránek (lékù je 80 rozdìlených do 4 stran) 
all_drugs <- read_html(odkaz_all_drugs[i])
DB[a:(a+24)] <- all_drugs %>% 
   html_nodes(".btn-card")%>%
   html_text()
a = a+25}
all_drugs4 <- read_html('https://www.drugbank.ca/unearth/q?approved=1&button=&c=_score&d=down&illicit=1&investigational=1&nutraceutical=1&page=4&query=schizophrenia&searcher=drugs&vet_approved=1&withdrawn=1')
DB4 <- all_drugs4 %>% 
  html_nodes(".btn-card")%>%
  html_text() 
pom <- length(DB4) # uložení poslední stránky lékù
DB[76:(75+pom)] <- DB4

# vytvoøení pomocných promìnných
Interactions <- character()
Targets <- character()
pomocna <- 1
Drugs <- matrix(0,length(DB),3)
odkaz <- "https://www.drugbank.ca/drugs/"

# postupné stahování dat jednotlivých lékù
for (k in 1:length(DB)){ 
Drug <- read_html(paste0(odkaz,DB[k])) #stažení dat z potøebné www

ATC <- Drug %>% # uložení promìnné ATC
  html_node(css="tr:nth-child(51) > td > ul > li:nth-child(1) > a") %>% 
  html_text()

Int <- Drug %>% # stažení dat pro vyhodnocení podmínky IF
  html_node(css="table#drug-interactions")
Targ <- Drug %>% 
  html_node(css="table#moa-target-table")
if (is.na(Int) == FALSE & is.na(Targ) == FALSE & is.na(ATC) == FALSE){  # uloží cíle a interakce jenom když existuje tabulka s interakcemi, cíli a ATC data
  Name <- Drug %>% # uložení názvu léku
    html_node("td, strong")%>%
    html_text()
  Group <- Drug %>% # uložení skupiny, do které lék patøí
    html_node("tr:nth-child(5) td")%>%
    html_text()
  Targ <- Drug %>% # uložení tabulky s cíli
    html_node(css="table#moa-target-table") %>%
    html_table()
    Targ[,ncol(Targ)+1] <- Name # pøidání názvu léku do posledního sloupce
    Targets <- rbind(Targets, Targ)
  Int <- Drug %>% # uložení interagujících lékù
    html_node(css="table#drug-interactions") %>%
    html_table()
    Int[,ncol(Int)+1] <- Name # pøidání názvu léku do posledního sloupce
    Interactions <- rbind(Interactions, Int)

Drugs[pomocna,1] <- Name # zapsání informací o lécích do tabulky Drugs
Drugs[pomocna,2] <- Group
Drugs[pomocna,3] <- ATC
pomocna <- pomocna+1 # umožní zapisovat nový lék vždy na další øádek v tabulce Drugs
}}

names(Interactions) <- c("Interacting drug","Interaction","Group", "Drug") # pøejmenování názvù sloupcù v tabulkách
names(Drugs) <- c("Name","Group", "ATC")
names(Targets)[names(Targets)=="V8"] <- "Drug"

Drugs <- Drugs[1:(pomocna-1),] # odstranìní pøebyteèných øádkù v tabulce (muselo být nadefinováno tolik øádkù, kolik je celkem lékù, protože pøedem nevíme, kolik z nich bude staženo - kolik má hodnoty v Targets a ATC)

library(DescTools) #naètení knihovny pro export do Excelu a pøevedení dat do Excelu
XLView(Drugs)
XLView(Interactions) 
XLView(Targets)