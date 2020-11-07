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

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

################
tabell1 <- data.frame(products$Varenavn, products$Pris, products$Varetype, products$Passertil, products$Land)
################

#NAME FUNKSJON 
choose_name <-function(name, tabell){
  name <- tolower(name)
  Varenavn <- tolower(tabell$Varenavn)
  rad <- tabell[grep(name, tabell$Varenavn, ignore.case = T, value = F), ]# et datasett hvor inputen og datasettet matcher
  
  if(name==""){
    return(tabell)
  }
  
  else if (nrow(rad)>=0){   #hvis rad-datasettet har et innhold, altså antall rader større enn 0
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
  fits <- tolower(as.list(scan(text=fits, what = ","))) #Småbokstaver og lager til liste
  fits <- fits[fits != "og"]  #fjerner ordene som ikke skal med:
  fits <- fits[fits != "and"] #fjerner ordene som ikke skal med:
  
  
  passertil <- tolower(tabell$Passertil)
  
  rad <- tabell[grep(fits[1], tabell$Passertil,ignore.case = TRUE, value = F), ]
  
  if(fits == ""){
    return(tabell)
  }
  
  else {
    #bruker for loop for at det skal filtreres for alle ordene brukeren har skrevet inn 
    for (i in 1:length(fits)){
      rad <- rad[grep(fits[i], rad$Passertil,ignore.case = TRUE, value = F), ]
    }
    
    #Lager en dataramme av det ferdig filtrerte "rad"
    tabell_fits <- data.frame(rad$Varenavn, rad$Varetype, rad$Land,rad$Volum, rad$Pris, rad$Passertil,rad$Vareurl)
    names(tabell_fits) <- substring(names(tabell_fits),5)
    
    
    #hvis det er rader igjen etter filtreringen: 
    if (nrow(rad) != 0){
      print(paste("Vi fant ", nrow(rad), "drikkevarer som passer til", fits))
      return (tabell_fits)
    }
    else{
      print(paste("Vi fant dessverre ingen varer som passer til", fits, ", vær så snill og prøv igjen: "))
      fits <- readline(prompt = "Hva ønsker du at drikkevaren skal passe bra til: ")
      return (choose_fits(fits, tabell))
    }
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#HOVEDFUNKSJON
full_function <- function(){
  name <- readline(prompt = "Velg navn på drikkevaren du ønsker å finne: ")
  
  name_tabell <- choose_name(name, products)
  
  pris_max <- readline(prompt=paste0("Prisene hos oss varierer fra ",min_price," til ",max_price,".", " Skriv inn din maks pris: "))
  pris_min <- readline(prompt=paste0("Skriv inn din minimum pris: "))
  
  pris_tabell <- choose_price(pris_max, pris_min, name_tabell)
  
  type <- readline(prompt = "Velg hva slags type drikkevarer du ønsker: ")
  type_tabell <- choose_type(type, pris_tabell)
  
  country <- readline(prompt = "Hvilket land ønsker du at varene skal være fra? ")
  country_tabell <- choose_country(country, type_tabell)
  
  fits <- readline(prompt = "Hva ønsker du at drikkevaren skal passe bra til: ")
  fits_tabell <- choose_fits(fits, country_tabell)
  
  return(fits_tabell)
}

full_function()


#KOMMENTARER FRA STINA
#DRITBRA, men her har noe vi kan jobbe videre med:
#- hvis en varetype ikke har noe i kolonnen "passer til", slik som feks vodka, hopp over?
#- fits-funksjonen funker ikke å hoppe over, kommer error- 
#- lage en loop til choose_name og choose_type  slik  du gjorde i fits-funksjonen
# for  at man ikke trenger å skrive i riktig rekkefølge?






#Define UI
library(shiny)
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
      label = "Navn på alkoholen:", ""
    ), 
    
    # Her viser appen en slidebar hvor han kan velge på pris
    sliderInput(
      inputId = "pris", 
      label = "Prisintervall:", "",
      min = min_price,
      max = max_price,
      value = c(500, 1000),
      step = 50
    ),
    
    # Her viser appen de forskjellige typene alkohol man kan velge mellom
    selectInput(
      inputId = "type",
      label = "Type alkohol", "",
      choices = c("", products$Varetype)
    ),
    
    #Filter på land
    selectInput(
      inputId = "land",
      label = "Hvilket land ønsker du: ", "",
      choices = c("", products$Land)
    ),
    
    #Filter på hva den passer til
    #radioButtons(
     # inputId = "passertil",
      #label = "Hva ønsker du at drikken skal passe til: ", "",
      #choices = 
    #),
    
    
    #Her hadde det vært nice med bulletmeny, men litt usikker på hvordan man mekker det
    
    
    #Submitknapp når man er ferdig med å filtrere
    actionButton(
      inputId = "submit",
      label = "Ferdig"
    )
  ), #Sidebarpanel
  
  ##################Main panel ###################################################
  mainPanel(
    h1("Liste over alkohol"),
    
    
    #tableOutput("tabell") --> tabellen laget i hovedfunksjonen over
    
    
    #Dette er bare noe fra youtubevideoen, men tenker at mainPanel må være tabellen med drikke 
    #h4("Navn"),
    #verbatimTextOutput("txtout"), #txtOut, generated from the server
    #h4("Minimumspris"),
    tableOutput(outputId = "tabell")
    
  )#Mainbarpanel
  
) #fluidpage


#Define server function
server <- function(input, output){
  output$tabell = renderTable({
    filtered <- 
      tabell1 %>% 
      #if(input$name = " "){
        filter(
          tabell1$products.Varetype == input$type,
          tabell1$products.Pris >= input$pris[1],
          tabell1$products.Pris <= input$pris[2],
          tabell1$products.Land == input$land
      )#}
      #else{
       # filter(
        #  tabell$products.Varenavn == input$name
      #)} 
    filtered
  })

} #Server

#create shiny object
shinyApp(ui = ui, server = server) 







 
