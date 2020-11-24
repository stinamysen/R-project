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
library(anytime)
#----------------------------------------------------------------------------------------------------------------------
#Since the data at vinmonopolet is changing everyday, we use the data.table library and the fread()-function 
#found this from https://www.r-bloggers.com/2015/03/getting-data-from-an-online-source/
produkter <- fread('https://www.vinmonopolet.no/medias/sys_master/products/products/hbc/hb0/8834253127710/produkter.csv')
is.data.frame(produkter)


products <- produkter %>% 
  select(-HovedGTIN,-Miljosmart_emballasje, -Gluten_lav_pa, -AndreGTINs) %>% 
  filter(Alkohol!="0,00") %>%
  unite('Passertil', Passertil01,Passertil02,Passertil03, sep = " ", remove=F ) %>% #Legger sammen passertil kolonnene
  mutate(Pris= as.numeric(gsub(",",".",Pris))) %>% #changing the separator , to . and making numeric
  mutate(Passertil=gsub("og","", Passertil)) %>% 
  mutate(Passertil=gsub(",","", Passertil)) %>% 
  mutate(Passertil=tolower(gsub("  "," ", Passertil)))

#----------------------------------------------------------------------------------------------------------------------
#NAME FUNKSJON 

choose_name <-function(name, tabell){
  name<-tolower(name) 
  list_name <- tolower(as.list(scan(text=name, what = ","))) #Småbokstaver og lager til liste
  Varenavn <- tolower(tabell$Varenavn)
  
  rad <- tabell[grep(list_name[1], tabell$Varenavn,ignore.case = TRUE, value = F), ]
  
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
      return(tabell_name)
    }
    
    else {
      return(NULL)
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
  
  rad <- tabell %>% filter(Pris <= pris_max_n, Pris >= pris_min_n)
  
  
  #If they have a upper and lower limit, filter within these:
  if (nrow(rad) != 0) {
    
    pris_tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl) 
    names(pris_tabell) <- substring(names(pris_tabell),5)
    
    return(pris_tabell)
  }
  
  else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#VARETYPE FUNKSJON
choose_type <- function(type, tabell){
  #Make it case insensitive:
  type <- tolower(type)
  Varetype <- tolower(tabell$Varetype)
  rad <- tabell[grep(type, tabell$Varetype,ignore.case = TRUE, value = F), ]
  
  if(type == ""){
    return(tabell)
  }
  else if (nrow(rad)!=0){
    rad <- tabell[grep(type, tabell$Varetype,ignore.case = TRUE, value = F), ]
    tabell_type <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil,rad$Vareurl)
    names(tabell_type) <- substring(names(tabell_type),5)
    
    return (tabell_type)
  } 
  else {
    return(NULL)
  }
  
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#LAND FUNKSJON
choose_country <- function(country, tabell){
  #Make it case insensitive:
  country <- tolower(country)
  land <- tolower(tabell$Land)
  
  rad <- tabell[grep(country, tabell$Land, ignore.case = TRUE, value = F), ]
  
  if(country=="ingen preferanser"){
    return(tabell)
  }
  else if (nrow(rad)!=0){
    tabell_country <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
    names(tabell_country) <- substring(names(tabell_country),5)
    
    return (tabell_country)
  } 
  
  else {
    return(NULL)
  }
}
t <- choose_name('fetzer', products)
choose_country('Norge',t)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#PASSER TIL FUNKSJON
choose_fits <- function(fits, tabell){
  passertil <- tolower(tabell$Passertil)
  rad <- tabell[grep(fits, tabell$Passertil,ignore.case = TRUE, value = F), ]
  
  if(length(fits)==0){
    return(tabell)
  }
  
  else {
    for (i in 1:length(fits)){
      rad<-rad[grep(fits[i], rad$Passertil,ignore.case = TRUE, value = F), ]
      
    }
    
    #hvis det er rader igjen etter filtreringen: 
    if (nrow(rad) != 0){
      tabell_fits <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
      names(tabell_fits) <- substring(names(tabell_fits),5) #removing the "rad." part of every colname
      return (tabell_fits)
    }
    else{
      return(NULL)
    }
    
  }
}

#----------------------------------------------------------------------------------------------------------------------------------
#SHINY APP
#Define UI
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(shinyFeedback)
#library(tcltk2)

#Lage en vektor av alle unike ord i passertil-kolonnen
fjern <-c("lyst", "kjøtt", "")
vektor_passer <- products %>% pull(Passertil) %>% strsplit(" ") %>% unlist %>% unique() 
vektor_passer <-append(vektor_passer[!vektor_passer%in%fjern],"lyst kjøtt")

'%then%' <- shiny:::'%OR%'


#if (interactive()) { #Denne if statementen fant jeg på nettet, ser ikke ut som den gjør noe forskjell
#https://shiny.rstudio.com/reference/shiny/0.14/checkboxGroupInput.html?fbclid=IwAR3aICmOX4h3P0sDOzzYcw40KMyR1dzwBYVp7GD3zd-DcY49-w4oESoYHco

ui <- fluidPage(
  shinyjs::useShinyjs(),
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
    
    sliderInput(
      inputId = "pris", 
      label = "Prisintervall: ",
      min = min_price,
      max = max_price,
      value = c(min, max),
    ),
    
    textInput(
      inputId = "type", #name will be sent to the server
      label = "Type alkohol:", ""
    ), 
    
    selectInput(
      inputId = "land", #name will be sent to the server
      label = "Hvor skal drikkevaren være fra?:", "",
      choices = c("Ingen preferanser", products$Land),
    ), 
    
    checkboxGroupInput(
      inputId = "passertil", label="Hva vil du drikken skal passe til", 
      #choices=c("ingen preferanser"="",vektor_passer),
      #selected =c("ingen preferanser"="")
      choices = vektor_passer
    ),
    
    
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
    
   if (is.null(input$passertil)) {
     passertil <- ("")
     }
  else{
    passertil<-input$passertil
    }
    
    name <- input$name
    pris_max <- as.numeric(input$pris[2])
    pris_min <- as.numeric(input$pris[1])
    type <- input$type
    land <- input$land
    
    name_tabell <- choose_name(name, products)
    
    pris_tabell <- choose_price(pris_max, pris_min, name_tabell)
    validate(
      need(!is.null(pris_tabell), 'Prisklassen er ikke gyldig med den filtrerte drikkevarenavnet. Vennligst prøv igjen.')
    )
    
    type_tabell <- choose_type(type, pris_tabell)
    validate(
      need(!is.null(type_tabell), 'Varetypen finnes ikke innenfor den gitte prisklassen og med det gitte navnet. Vennligst prøv igjen eller la boksen stå tom.')
    )
    
    country_tabell <- choose_country(land, type_tabell)
    validate(
      need(!is.null(country_tabell), 'Ingen varer fra dette landet innenfor de gitte filtreringene. Vennligst forsøk igjen.')
    )
    
    fits_tabell <- choose_fits(passertil, country_tabell)
    validate(
      need(!is.null(fits_tabell), 'Ingen varer som passer til ønsket mat innefor de gitte filtreringene. Vennligst prøv igjen eller la boksen stå tom.')
    )
    
    return(fits_tabell)
    
  })

  
  #OUTPUT TABLE
  output$vin_table <- renderDataTable({
    
    #SJEKKER OM NAVN OG TYPE FINNES I VINMONOPOLET GENERELT
    name_l <- tolower(input$name)
    Varenavn <- tolower(products$Varenavn)
    rad_name <- products[grep(name_l, products$Varenavn,ignore.case = TRUE, value = F), ]
    
    type_l <- tolower(input$type)
    Varetype <- tolower(products$Varetype)
    rad_type <- products[grep(type_l, products$Varetype,ignore.case = TRUE, value = F), ]
    
    validate(
      need(nrow(rad_name)!=0 || input$name=='', 'Varenavnet finnes ikke i vinmonolopoets lager. Vennligst prøv igjen eller la boksen stå tom.') %then%
        need(nrow(rad_type)!=0 || input$type=='', 'Varetypen finnes ikke i vinmonolopoets lager. Vennligst prøv igjen eller la boksen stå tom.') 
    )
    
    mypar()
  })
  
}


#create shiny object
shinyApp(ui, server)
