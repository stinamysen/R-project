#Vinmonopolet data
library(httr)
library(jsonlite)
library(data.table)
library(bit64)
library(docstring) #HUSK Å BRUKE DENNE TIL Å FORKLARE FEKS EN FUNKSJON
library(tidyverse)

#----------------------------------------------------------------------------------------------------------------------

#Since the data at vinmonopolet is changing everyday, we use the data.table library and the fread()-function 
 #found this from https://www.r-bloggers.com/2015/03/getting-data-from-an-online-source/
products <- fread('https://www.vinmonopolet.no/medias/sys_master/products/products/hbc/hb0/8834253127710/produkter.csv')
head(products)
is.data.frame(products)




products %>% 
  select(-HovedGTIN,-Miljosmart_emballasje, -Gluten_lav_pa, -AndreGTINs) %>% 
  filter(Alkohol!="0,00") %>% 
  head

choose_name <- function(){
  name <- readline(prompt = "Choose the name of the liquor you want (press enter if you don't want to filtrate on name: ")
  if (name %in% products$Varenavn){
    rad <- products[grep(name, products$Varenavn, value = F), ]
    tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Volum, rad$Pris, rad$Passertil01, rad$Passertil02, rad$Passertil03, rad$Vareurl)
    print(paste("We found: ", nrow(tabell), "liquor(s) matching your input."))
    return (tabell)
  }
  else {
    print("No such liquor name in Vinmonopolets storage, please try again: ")
    return(choose_name())
  }
}

choose_name()


#Må gjøres med funksjonen:
#- endre navn på kolonnene i "tabell"
#- kunne trykke enter for å hoppe videre til annen input



#Funksjon for pris
#Lager min- og makspris for å gi brukeren et interval for pris
min_price = min(fil$Pris)
max_price = max(fil$Pris)


choose_price <- function(){
  pris = readline(prompt=paste0("Choose a price between ", min_price, " and ", max_price, ": "))
  pris = as.numeric(pris)
  #if (pris <= max_price) {
  rad <- products[products$Pris <= pris, ]
  print(paste("We found: ", nrow(rad), "liquor(s) matching your input."))
  #}
  #else{
  # print("Not a valid price, please try again: ")
  #return(choose_price())
  #}
}


choose_price()

#Må gjøres med funksjonen:
#Får ikke if-statementen til å fungere hvis man skal få brukeren til å 
#velge en pris mellom min- og makspris og forsøke på nytt dersom han ikke gjør det


#Funksjon for varetype, veldig lik funksjonen for navn
choose_type <- function(){
  type <- readline(prompt = "Choose the type of the liquor you want: ")
  if (type %in% products$Varetype){
    rad <- products[grep(type, products$Varetype, value = F), ]
    tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Volum, rad$Pris, rad$Passertil01, rad$Passertil02, rad$Passertil03, rad$Vareurl)
    print(paste("We found ", nrow(tabell), " liquors"))
  } else {
    print("No such liquor name in Vinmonopolets storage, please try again: ")
    return(choose_type())
  }
}

choose_type()



full_function <- function(){
  name <- choose_name()
  if (name = ""){
    #Hvis brukeren ikke taster inn navn vil de andre funksjonene kjøres for å filtrere på
    #pris, type, smak, lukt etc. 
    choose_type()
    choose_price()
    
  }
  else{
    #else her vil være at brukeren taster inn navnet på en type alkohol
    # funksjonen må her da plukke ut 
  }
}


#Bruker taster inn navnet på alkoholen
name <- readline(prompt = "Choose the name of the liquor you want (press enter if you don't want to filtrate on name: ")
#plukker ut akkurat den typen
navn <- products[grep(name, products$Varenavn, value = F), ]
#henter ut varetypen til det brukeren tastet inn
type <- navn$Varetype
passer_til <- navn$Passertil01
#går tilbake til hovedfilen for å plukke ut de andre type alkoholene
tabell <- products[grep(type, products$Varetype, value = F), ]
#her plukker vi ut de som har lik passeril01 som det brukeren tastet inn
tabell2 <- products[grep(passer_til, products$Passertil01, value = F), ]
 
