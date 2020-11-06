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

#----------------------------------------------------------------------------------------------------------------------
#NAME FUNKSJON 
#må gjøres:
#- hvis kun rad=1- still ingen fler spørmsål
#- få vekk "read 1 items"

helt_lik<-function(name,tabell){
  name<-tolower(name)
  Varenavn <- tolower(tabell$Varenavn)
  eksakt<-as.numeric(name==Varenavn)
  if(sum(eksakt)>0){
    rad_string<-tabell %>% filter(str_detect(tolower(Varenavn),name))
    full_match <- data.frame(rad_string$Varenavn, rad_string$Varetype, rad_string$Land, rad_string$Volum, rad_string$Pris, rad_string$Passertil, rad_string$Vareurl)
    names(full_match) <- substring(names(full_match),12) 
    print(paste("Vi fant: ", nrow(full_match), "drikkevare(r) som inneholdt", name, "."))
    return(full_match)
  }
  else{
    return()
  }
  
}
helt_lik("løiten export",products)

choose_name <-function(name, tabell){
  name<-tolower(name) #for at ikke outputen "vi fant ..... drikkervarer som inneholdt..." skal bli gjentatt flere ganger hvis brukeren skriver fler enn 1 ord
  list_name <- tolower(as.list(scan(text=name, what = ","))) #Småbokstaver og lager til liste
  Varenavn <- tolower(tabell$Varenavn)
  
  rad <- tabell[grep(list_name[1], tabell$Varenavn,ignore.case = TRUE, value = F), ]
  # et datasett hvor inputen og datasettet matcher
  
  
  if (length(list_name)==0){
    return(tabell)
  }
  else {
    for (i in 1:length(list_name)){
      rad[grep(list_name[i], rad$Varenavn,ignore.case = TRUE, value = F), ]
    }
    
    
    if (nrow(rad)>0){   #hvis rad-datasettet har et innhold, altså antall rader større enn 0
      tabell_name <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
      names(tabell_name) <- substring(names(tabell_name),5) #removing the "rad." part of every colname
      print(paste("Vi fant: ", nrow(tabell_name), "drikkevare(r) som inneholdt", name, "."))
      return(tabell_name)
    }
    
    else {
      print(paste("Vinmolopolet har dessverre ingen drikkevarer med navnet ", name, ". Vær så snill og prøv igjen: "))
      name <- readline(prompt = "Velg navn på drikkevaren du ønsker å finne: ")
      return(choose_name(name, tabell))
    }
  }
}




#-------------------------------------------------------------------------------------------------------------------------------------------------------------


#PRIS FUNKSJON
#Lager min- og makspris for å gi brukeren et interval for pris
min_price <- round(min(products$Pris),1)#rounded minimum price 
max_price <- round(max(products$Pris),1)#rounded maximum price


choose_price <- function(pris_max, pris_min, tabell){
  pris_max_n <- as.numeric(pris_max)
  pris_min_n <- as.numeric(pris_min)
  
  #If one of the inputs aren't numbers:
  if (pris_max == "" && pris_min == ""){
    return(tabell)
  }
  
  #If they only have one limit:
  else if ((pris_max == "" && pris_min_n >= min_price) || (pris_max_n <= max_price && pris_min == "")){
    if (pris_max == ""){
      p_min_enquo = enquo(pris_min_n)
      
      rad <- tabell %>% filter(Pris >= (!!p_min_enquo))
      pris_tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl) 
      names(pris_tabell) <- substring(names(pris_tabell),5)
      print(paste("Vi fant: ", nrow(rad), "drikkevarer i denne prisklassen"))
      return(pris_tabell)
    }
    else {
      p_max_enquo = enquo(pris_max_n)
      
      rad <- tabell %>% filter(Pris <= (!!p_max_enquo))
      pris_tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
      names(pris_tabell) <- substring(names(pris_tabell),5)
      print(paste("Vi fant: ", nrow(rad), "drikkevarer i denne prisklassen"))
      return(pris_tabell)
    }
  }
  
  #If the user doesnt enter numbers
  else if (is.na(pris_max_n) || is.na(pris_min_n)){
    print("Ikke gyldig pris, vær så snill og prøv igjen: ")
    pris_max <- readline(prompt=paste0("Prisene hos oss varierer fra ",min_price," til ",max_price,".", " Skriv inn din maks pris: "))
    pris_min <- readline(prompt=paste0("Skriv inn din minimum pris: "))
    
    return(choose_price(pris_max, pris_min, tabell))
  }
  
  #If they have a upper and lower limit, filter within these:
  else if (pris_max_n <= max_price && pris_min_n >= min_price) {
    p_max_enquo = enquo(pris_max_n)
    p_min_enquo = enquo(pris_min_n) #får den under til å funke, men aner faen ikke hvorfor - this took me 6 hours :) 
    
    rad <- tabell %>% filter(Pris <= (!!p_max_enquo) && Pris >= (!!p_min_enquo))
    pris_tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl) 
    names(pris_tabell) <- substring(names(pris_tabell),5)
    print(paste("Vi fant: ", nrow(rad), "drikkevarer i denne prisklassen"))
    return(pris_tabell)
  }
  
  #usikker på om denne trengs? Kanskje greit å ha? 
  else {
    print("Ikke gyldig pris, vær så snill og prøv igjen: ")
    pris_max <- readline(prompt=paste0("Prisene hos oss varierer fra ",min_price," til ",max_price,".", " Skriv inn din maks pris: "))
    pris_min <- readline(prompt=paste0("Skriv inn din minimum pris: "))
    
    return(choose_price(pris_max, pris_min, tabell))
  }
  
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------


#VARETYPE FUNKSJON
choose_type <- function(type, tabell){
  #Make it case insensitive:
  type <- tolower(type)
  Varetype <- tolower(tabell$Varetype)
  rad <- tabell[grep(type, tabell$Varetype,ignore.case = TRUE, value = F), ]
  
  if(type==""){
    return(tabell)
  }
  else if (nrow(rad)!=0){
    rad <- tabell[grep(type, tabell$Varetype,ignore.case = TRUE, value = F), ]
    tabell_type <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil,rad$Vareurl)
    names(tabell_type) <- substring(names(tabell_type),5)
    print(paste("Vi fant: ", nrow(rad), "drikkevarer av denne typen"))
    return (tabell_type)
  } 
  else {
    print("Vinmonopolet har dessverre ingen drikkevarer av denne typen, vær så snill og prøv igjen: ")
    type <- readline(prompt = "Velg hva slags type drikkevarer du ønsker: ")
    return(choose_type(type, tabell))
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Funksjon for hvilket land
choose_country <- function(country, tabell){
  #Make it case insensitive:
  country <- tolower(country)
  land <- tolower(tabell$Land)
  
  if(country==""){
    return(tabell)
  }
  else if (country %in% land){
    rad <- tabell[grep(country, tabell$Land, ignore.case = TRUE, value = F), ]
    tabell_country <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
    names(tabell_country) <- substring(names(tabell_country),5)
    print(paste("Vi fant: ", nrow(rad), " drikkevarer fra ", country))
    return (tabell_country)
  } 
  
  else {
    print(paste("Vinmonopolet har dessverre ingen drikkevarer fra ", country, " vær så snill og prøv igjen: "))
    country <- readline(prompt = "Hvilket land ønsker du at varene skal være fra? ")
    return(choose_country(country, tabell))
  }
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------


#PASSER TIL FUNKSJON
choose_fits <- function(fits, tabell){
  #Make it case insensitive:
  fits<-gsub(",", "", fits)
  one_word<-fits
  fits <- tolower(as.list(scan(text=fits, what = ""))) #Småbokstaver og lager til liste
  fits <- fits[fits != "og"]  #fjerner ordene som ikke skal med:
  fits <- fits[fits != "and"] #fjerner ordene som ikke skal med:
  
  
  passertil <- tolower(tabell$Passertil)
  
  rad <- tabell[grep(fits[1], tabell$Passertil,ignore.case = TRUE, value = F), ]
  
  if(length(fits)==0){
    return(tabell)
  }
  
  else {
    rad <- rad[grep(fits[i], rad$Passertil,ignore.case = TRUE, value = F), ]
    
    
    
    #hvis det er rader igjen etter filtreringen: 
    if (nrow(rad) != 0){
      print(paste("Vi fant ", nrow(rad), "drikkevarer som passer til", one_word))
      return (tabell_fits)
    }
    else{
      print(paste("Vi fant dessverre ingen varer som passer til", one_word, ", vær så snill og prøv igjen: "))
      fits <- readline(prompt = "Hva ønsker du at drikkevaren skal passe bra til: ")
      return (choose_fits(fits, tabell))
    }
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#HOVEDFUNKSJON
full_function <- function(){
  name <- readline(prompt = "Velg navn på drikkevaren du ønsker å finne: ")
  Varenavn <- tolower(products$Varenavn)
  full_name<-helt_lik(name,products)
  eksakt<-as.numeric(name==Varenavn)
  name_tabell <- choose_name(name,products)
  
  
  
  if(sum(eksakt)>0){
    return(full_name)
  }
  
  #FORTSETT HER- FÅR 17 MATCH ISTEDNEFOR 2
  else if (nrow(name_tabell)==1){ #hvis det bare er en av dette navnet, unødvendig å gjennom resten av spm
    return(name_tabell)
  }
  
  
  else{
    
    pris_max <- readline(prompt=paste0("Prisene hos oss varierer fra ",min_price," til ",max_price,".", " Skriv inn din maks pris: "))
    pris_min <- readline(prompt=paste0("Skriv inn din minimum pris: "))
    
    pris_tabell <- choose_price(pris_max, pris_min, name_tabell)
    if(nrow(pris_tabell)==1){
      return(pris_tabell)
    }
    else{
      type <- readline(prompt = "Velg hva slags type drikkevarer du ønsker: ")
      type_tabell <- choose_type(type, pris_tabell)
      if(nrow(type_tabell)==1){
        return(type_tabell)}
      
      else{
        country <- readline(prompt = "Hvilket land ønsker du at varene skal være fra? ")
        country_tabell <- choose_country(country, type_tabell)
        
        if(nrow(country_tabell)==1){
          return(country_tabell)
        }
        
        else{
          for (i in nrow(country_tabell)){
            sumrow = 0 
            row = str_length(str_trim(country_tabell$Passertil[i]))
            sumrow = sumrow + row
          }
          
          if (sumrow != 0){
            fits <- readline(prompt = "Hva ønsker du at drikkevaren skal passe bra til: ")
            fits_tabell <- choose_fits(fits, country_tabell)
            return(fits_tabell)
          }
          else {
            tabell <- country_tabell %>% select(-Passertil) #Fjerner kolonnen med passer til siden det ikke er noe der
            return (tabell)
          }
        }
      }
    }
  }
}

full_function()

#----------------------------------------------------------------------------------------------------------------------------------
#SHINY APP





#Define UI
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  ################Header panel###################################################
  headerPanel("Vinmonopolets app"),
  
  ################Sidebar panel##################################################
  sidebarPanel(
    tags$h3("Filtrer under:"),
    
    textInput(
      inputId = "name", #name will be sent to the server
      label = "Navn på alkoholen:", "",
    ), 
    
    # Her viser appen en slidebar hvor han kan velge på pris
    sliderInput(
      inputId = "pris", 
      label = "Prisintervall:", "",
      min = min_price,
      max = max_price,
      value = 1000,
      step = 50
    ),
    
    # Her viser appen de forskjellige typene alkohol man kan velge mellom
    selectInput(
      inputId = "type",
      label = "Type alkohol", "",
      choices = products$Varetype
    ),
    
    #Filter på land
    selectInput(
      inputId = "land",
      label = "Hvilket land ønsker du: ", "",
      choices = products$Land,
    ),
    
    #Filter på passer til
    selectInput(
      inputId = "passertil",
      label = "Hva vil du drikkevaren skal passe til: ", "",
      choices = products$Passertil,
    ),
  
    #Submitknapp når man er ferdig med å filtrere
    actionButton(
      inputId = "full_f",
      label = "Ferdig"
#      tableOutput("vin_table")
    )
  ),
  ##################Main panel ###################################################
  #Displaying outputs
  mainPanel(
    h1("Liste over alkohol"),
    dataTableOutput("vin_table")

  )
)

#HOVEDFUNKSJON
full_f <- function(name,pris,type,land,passertil){
  name_tabell <- choose_name(name, products)
  
  pris_tabell <- choose_price(pris, name_tabell)
  
  type_tabell <- choose_type(type, pris_tabell)
  
  country_tabell <- choose_country(land, type_tabell)
  
  fits_tabell <- choose_fits(passertil, country_tabell)
  
  return(fits_tabell)
}


#Define server function - logic required to do the output
server <- function(input, output){

  
  mypar <- eventReactive(input$full_f, {
    name <- input$name
    pris <- as.numeric(input$pris)
    type <- input$type
    land <- input$land
    passertil <- input$passertil
    
    return (full_f(name,pris,type,land,passertil))
    
  })
  
  output$vin_table <- renderDataTable({
    mypar()
  })
}

#create shiny object
shinyApp(ui, server) 



#  output$datatable <- renderDataTable(mypar())

