#Vinmonopolet data
library(httr)
library(jsonlite)
library(data.table)
library(bit64)
library(docstring) #HUSK Å BRUKE DENNE TIL Å FORKLARE FEKS EN FUNKSJON
library(tidyverse)
library(dplyr) #rename
library(shiny)#filter name function
library(stringr)
library(anytime) #endre tid
#----------------------------------------------------------------------------------------------------------------------
#Since the data at vinmonopolet is changing everyday, we use the data.table library and the fread()-function 
 #found this from https://www.r-bloggers.com/2015/03/getting-data-from-an-online-source/
produkter <- fread('https://www.vinmonopolet.no/medias/sys_master/products/products/hbc/hb0/8834253127710/produkter.csv')
is.data.frame(produkter)


products <- produkter %>% 
  select(-HovedGTIN,-Miljosmart_emballasje, -Gluten_lav_pa, -AndreGTINs) %>% 
  filter(Alkohol!="0,00") %>%
  unite('Passertil', Passertil01,Passertil02,Passertil03, sep = " ", remove=F ) %>% #Legger sammen passertil kolonnene
  mutate(Datotid= anytime(Datotid)) %>% #for å få finere tid-format
  mutate(Pris= as.numeric(gsub(",",".",Pris)))# %>%  #changing the separator , to . and making numeric


choose_name <-function(){
  name <- readline(prompt = "Choose the name of the liquor you want (press enter if you don't want to filtrate on name): ")
  name <- tolower(name)
  Varenavn <- tolower(products$Varenavn) 
  rad <- products[grep(name, products$Varenavn, ignore.case = T, value = F), ]# et datasett hvor inputen og datasettet matcher
  if(name==""){
    return()
  }
  else if (nrow(rad)>=0){   #hvis rad-datasettet har et innhold, altså antall rader større enn 0
    tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
    names(tabell) <- substring(names(tabell),5) #removing the "rad." part of every colname
    print(paste("We found: ", nrow(tabell), "liquor(s) matching your input."))
    return(tabell)
  }
  
  else {
    print("No such liquor name in Vinmonopolets storage, please try again: ")
    return(choose_name())
  }
}
choose_name()



name <- readline(prompt = "Choose the name of the liquor you want (press enter if you don't want to filtrate on name): ")
#Make it case insensitive:
name <- strsplit(tolower(name), " ")
Varenavn <- tolower(products$Varenavn)
check<-(all(name %in% Varenavn))
  


#Må gjøres med funksjonen:
#- kunne trykke enter for å hoppe videre til annen input --> gjøres vell i hovedfunksjonen?


#Funksjon for pris

#Lager min- og makspris for å gi brukeren et interval for pris

min_price <- round(min(products$Pris))#rounded minimum price 
max_price <- round(max(products$Pris))#rounded maximum price


choose_price <- function(){
  pris_max <- readline(prompt=paste0("The prices range from ",min_price," to ",max_price,".", " Choose your maximum price: "))
  pris_min <- readline(prompt=paste0("Choose your minimum price: "))
  pris_max <- as.numeric(pris_max)
  pris_min <- as.numeric(pris_min)
  
  if(is.na(pris_max) && is.na(pris_min)){
    return()
  }
  else if (pris_max <= max_price && pris_min >= min_price) {
    p_max_enquo = enquo(pris_max)
    p_min_enquo = enquo(pris_min) #får den under til å funke, men aner faen ikke hvorfor - this took me 6 hours :) 
    rad <- products %>% filter(Pris <= (!!p_max_enquo) && Pris >= (!!p_min_enquo))
    tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl) #kanskje gjøre innholdet i denne dataframen til en listen
                                                                                                      #slik at man slipper å skrive dette inn for hver funksjon
    names(tabell) <- substring(names(tabell),5)
    print(paste("We found: ", nrow(rad), "liquor(s) matching your input."))
    return(tabell)
  }
  else{
    print("Not a valid price, please try again: ")
    return(choose_price())
  }
}
choose_price()


#Funksjon for varetype, veldig lik funksjonen for navn
choose_type <- function(){
  type <- readline(prompt = "Choose the type of the liquor you want: ")
  #Make it case insensitive:
  type <- tolower(type)
  Varetype <- tolower(products$Varetype)
  rad <- products[grep(type, products$Varetype,ignore.case = TRUE, value = F), ]
  if(type==""){
    return()
  }
  else if (nrow(rad)!=0){
    rad <- products[grep(type, products$Varetype,ignore.case = TRUE, value = F), ]
    tabell_type <- data.frame(rad$Varenavn, rad$Varetype, rad$Volum, rad$Pris, rad$Passertil,rad$Vareurl)
    names(tabell_type) <- substring(names(tabell_type),5)
    print(paste("We found ", nrow(tabell_type), " liquors"))
    return (tabell_type)
  } 
  else {
    print("No such liquor name in Vinmonopolets storage, please try again: ")
    return(choose_type())
  }
}
choose_type()


#Funksjon for hvilket land
choose_country <- function(){
  country <- readline(prompt = "Which country do you want the liqour to come from? ")
  #Make it case insensitive:
  country <- tolower(country)
  land <- tolower(products$Land)
  
  if(country==""){
  }
  else if (country %in% land){
    rad <- products[grep(country, products$Land, ignore.case = TRUE, value = F), ]
    tabell_country <- data.frame(rad$Varenavn, rad$Varetype, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
    names(tabell_country) <- substring(names(tabell_country),5)
    print(paste("We found ", nrow(rad), " liquors"))
    return (tabell_country)
  } 
  
  else {
    print(paste("No liquor from", country, "in Vinmonopolets storage, please try again: "))
    return(choose_country())
  }
}
choose_country()

#Funksjon for hva den skal passe til
choose_fits <- function(){
  fits <- readline(prompt = "What do you want the liqour to fit well with: ")
  
 
  #Make it case insensitive:
  fits <- tolower(as.list(scan(text=fits, what = ","))) #Småbokstaver og lager til liste
  fits <- fits[fits != "og"]  #fjerner ordene som ikke skal med:
  fits <- fits[fits != "and"] #fjerner ordene som ikke skal med:
  

  passertil <- tolower(products$Passertil)
  
  rad <- products[grep(fits[1], products$Passertil,ignore.case = TRUE, value = F), ]
  
  #if(length(fits)==0){
   # return()
  
  #bruker for loop for at det skal filtreres for alle ordene brukeren har skrevet inn 
  for (i in 1:length(fits)){
    rad <- rad[grep(fits[i], rad$Passertil,ignore.case = TRUE, value = F), ]
  }
  
  #Lager en dataramme av det ferdig filtrerte "rad"
  tabell_fits <- data.frame(rad$Varenavn, rad$Varetype, rad$Volum, rad$Pris, rad$Passertil,rad$Vareurl)
  names(tabell_fits) <- substring(names(tabell_fits),5)
  
  
  #hvis det er rader igjen etter filtreringen: 
  if (nrow(rad) != 0){
    print(paste("We found: ", nrow(rad), "liquor(s) matching your input."))
    return (tabell_fits)
  }
  else{
    print(paste("We couldn't find any liquor that is good with those dishes. Please try again: "))
    return (choose_fits())
  }
  
}



choose_fits()


filtering<-products[grep(price, products$Pris, value = F), ]
country<- choose_country()
filtering<-products[grep(country, products$Land, value = F), ]
fits<- choose_fits()
filtering<-products[grep(fits, products$Passertil, value = F), ]


full_function <- function(){
  name_input <-choose_name()
  if (is.null(name_input)){
    #Hvis brukeren ikke taster inn navn vil de andre funksjonene kjøres for å filtrere på
    #pris, type, smak, lukt etc. 
    price <- choose_price()
    if (is.null(price)){
      type<-choose_type()
      if(is.null(type)){
        country<-choose_country()
        if(is.null(country)){
          fits<-choose_fits()
          if(is.null(fits)){
            return()
          }
          else{
            filtering<-products[grep(fits, products$Pris, value = F), ]
          }
        }
        else{
          filtering<-products[grep(country, products$Pris, value = F), ]
        }
        
      }
      else{
        filtering<-products[grep(type, products$Pris, value = F), ]
      }
    }
    
      
    }
    else{
      filtering<-products[grep(price, products$Pris, value = F), ]
    }
      
    
  else{
    return(name_input)
  }

}

full_function()




?prompt_are

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



