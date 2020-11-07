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
  #mutate(Datotid= anytime(Datotid)) %>% #for å få finere tid-format
  mutate(Pris= as.numeric(gsub(",",".",Pris)))# %>%  #changing the separator , to . and making numeric

#----------------------------------------------------------------------------------------------------------------------
#NAME FUNKSJON 
#må gjøres:
#- hvis kun rad=1- still ingen fler spørmsål
#- få vekk "read 1 items"


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
    }
  }
} 

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#PRIS FUNKSJON
#Lager min- og makspris for å gi brukeren et interval for pris
min_price <- round(min(products$Pris))#rounded minimum price 
max_price <- round(max(products$Pris))#rounded maximum price


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
    return(NULL)
  }
  
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Funksjon for hvilket land
choose_country <- function(country, tabell){
  #Make it case insensitive:
  country <- tolower(country)
  land <- tolower(tabell$Land)
  
  if(country=="ingen preferanser"){
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
    #TROR NOE MÅ HER MTP ALERT
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
    for (i in 1:length(fits)){
      rad[grep(fits[i], rad$Passertil,ignore.case = TRUE, value = F), ]
    }
    
    #hvis det er rader igjen etter filtreringen: 
    if (nrow(rad) != 0){
      tabell_fits <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
      names(tabell_fits) <- substring(names(tabell_fits),5) #removing the "rad." part of every colname
      print(paste("Vi fant ", nrow(rad), "drikkevarer som passer til", one_word))
      return (tabell_fits)
    }
    
    else{
      return (tabell)
    }
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#HOVEDFUNKSJON
full_function <- function(name,pris_max, pris_min, type,land,passertil){
  
  name_tabell <- choose_name(name,products)
  
  
  if (nrow(name_tabell)==1){ #hvis det bare er en av dette navnet, unødvendig å gjennom resten av spm
    return(name_tabell)
  }
  
  
  else{
    
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


no_match <- function(input) {
  if (!(input %in% pris_tabell)) {
    "Choose another liquor or leave the space open. "
    return (pris_tabell)
  } else if (input == "") {
    return (pris_tabell)
  } else {
    return (type_tabell)
  }
}

#----------------------------------------------------------------------------------------------------------------------------------
#SHINY APP

#Define UI
library(shinythemes)
library(shinyalert)
library(shinyjs)
library(shinyBS)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  #useShinyalert(),
  #useShinyjs(),
  #bsAlert(),
  
  ################Header panel###################################################
  headerPanel("Vinmonopolets app"),
  
  ################Sidebar panel##################################################
  sidebarPanel(
    tags$h3("Filtrer under:"),
    
    textInput(
      inputId = "name", #name will be sent to the server
      label = "Navn på alkoholen:", "",
    ), 
    
    # Prisintervall
    sliderInput(
      inputId = "pris", 
      label = "Prisintervall: ",
      min = min_price,
      max = max_price,
      value = c(min, max),
    ),
    
    textInput(
      inputId = "type", #name will be sent to the server
      label = "Type alkohol:", "",
    ), 
    
    selectInput(
      inputId = "land", #name will be sent to the server
      label = "Hvor skal drikkevaren være fra?:", "",
      choices = c("Ingen preferanser", products$Land),
    ), 
    
    #Filter på passer til
    textInput(
      inputId = "passertil",
      label = "Hva vil du drikkevaren skal passe til: ", "",
    ),
    
    #Submitknapp når man er ferdig med å filtrere
    actionButton(
      inputId = "full_f",
      label = "Ferdig"
    )
  ),
  
  ##################Main panel ###################################################
  #Displaying outputs
  mainPanel(
    h1("Liste over alkohol"),
    dataTableOutput("vin_table")
  )
)


#Define server function - logic required to do the output
server <- function(input, output){
  
  mypar <- eventReactive(input$full_f, {
    name <- input$name
    pris_max <- as.numeric(input$pris[2])
    pris_min <- as.numeric(input$pris[1])
    type <- input$type
    land <- input$land
    passertil <- input$passertil
    
    
    name_tabell <- choose_name(name, products)
    pris_tabell <- choose_price(pris_max, pris_min, name_tabell)
    no_match(type)
    type_tabell <- choose_type(type, pris_tabell)
    
    #    if type %in% pris_tabell{
    #      type_tabell <- choose_type(type, pris_tabell)
    #    }
    #    else 
    #      ALERT
    
    #FORSØK PÅ ALERT HVIS INPUT IKKE ER TILGJENGELIG I VARELAGER
    
    #    observeEvent(input$type, {
    #      if (is.null(type_tabell))
    #        type_tabell <- pris_tabell
    #        return()
    #      id <<- showNotification(paste("hei"),duration=0)
    #    })
    
    
    #    output$alert <- renderText({
    #      if (!(is.na(type_tabell))){
    #        createAlert(session, "alert", "typealert", content = "hei")
    #      }
    #      else{
    #        closeAlert(session, "typealert")
    #        return(type_tabell)
    #      }
    #    })
    
    country_tabell <- choose_country(land, type_tabell)
    fits_tabell <- choose_fits(passertil, country_tabell)
    
    return(fits_tabell)
  }
  
  )
  
  output$vin_table <- renderDataTable({
    mypar()
  })
}

#create shiny object
shinyApp(ui, server)




 
