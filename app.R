#Vinmonopolet data
library(httr)
library(jsonlite)
library(data.table)
library(bit64)
library(tidyverse)
library(dplyr)
library(shiny)
library(stringr)
library(anytime)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(shinyFeedback)
library(rsconnect)
#----------------------------------------------------------------------------------------------------------------------
#Since the data at vinmonopolet is changing everyday, we use the data.table library and the fread()-function 
#found this from https://www.r-bloggers.com/2015/03/getting-data-from-an-online-source/
produkter <- fread('https://www.vinmonopolet.no/medias/sys_master/products/products/hbc/hb0/8834253127710/produkter.csv')
is.data.frame(produkter)

#Some of the rows in the dataset from Vinmonopolet isn't liquor, so we remove these rows.
#We also sort out the liquor not containing any alcohol, and change the prices to numeric and changing their separator from "," to "." 
#Lastly, we unite the three columns of information about what the liquor fits with, separate the different fits with " ", rather than "og" and ",", and turn all cases to lower cases 
products <- produkter %>% 
  select(-HovedGTIN,-Miljosmart_emballasje, -Gluten_lav_pa, -AndreGTINs) %>% 
  filter(Alkohol!="0,00") %>%
  unite('Passertil', Passertil01,Passertil02,Passertil03, sep = " ", remove=F ) %>%
  mutate(Pris= as.numeric(gsub(",",".",Pris))) %>% #changing the separator , to . and making numeric
  mutate(Passertil=gsub("og","", Passertil)) %>% 
  mutate(Passertil=gsub(",","", Passertil)) %>% 
  mutate(Passertil=tolower(gsub("  "," ", Passertil)))

#----------------------------------------------------------------------------------------------------------------------
# The following functions will allow the user to filter on different preferences for what Vinmonopolet offers. 
# We have chosen to let the user have the possibility to filter on the following:
# "name", "price", "type", "country", "fits"

# All the functions follow the same format which is the following:
# it takes in a parameter from the user, iterates through the products-data set and tries to find any matches in the relevant column
# if it finds a match, it will create and return a data table and store the matched row with the following columns:
# "Varenavn", "Varetype", "Land", "Volum", "Pris", "Passertil", "Vareurl"
# if it doesn't find any matches with the user input, it will not return anything.
# if the table, after the new filtering, ends up having no matches, the user will get a message saying that there is no matches to this filtering

#-----------------------------------------------------------------------------------------------------------------------
#Navn-funksjon
choose_name <-function(name, tabell){
  #Make it case insensitive:
  name<-tolower(name)
  list_name <- tolower(as.list(scan(text=name, what = ",")))
  Varenavn <- tolower(tabell$Varenavn)
  
  rad <- tabell[grep(list_name[1], tabell$Varenavn,ignore.case = TRUE, value = F), ]
  
  if (length(list_name)==0){  #if the user does not filter on the name of the liqour
    return(tabell)            #the function will return the whole table
  }
  else {
    for (i in 1:length(list_name)){                                        #looping through all of the character-vectors, , 
      rad[grep(list_name[i], rad$Varenavn,ignore.case = TRUE, value = F), ]#ends up with the rows which contains these words, regardless of the order of the input words
    }
    
    if (nrow(rad)>0){
      tabell_name <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
      names(tabell_name) <- substring(names(tabell_name),5) #removing the "rad." part of every colname
      return(tabell_name) #returning the filtered table if the dataset finds any matches
    }
    
    else {
      return(NULL)       #When the the input does not match any name of the liqours
    }
  }
} 
#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#PRIS-FUNKSJON
# We create variables for the minimum- and maximum price so that the user knows which range he/she can choose to filter on. 
min_price <- round(min(products$Pris))#rounded minimum price 
max_price <- round(max(products$Pris))#rounded maximum price

choose_price <- function(pris_max, pris_min, tabell){

  pris_max_n <- as.numeric(pris_max)
  pris_min_n <- as.numeric(pris_min)#turning the min- and max-variables into numerical variables
  
  rad <- tabell %>% filter(Pris <= pris_max_n, Pris >= pris_min_n)
  
  #If they have a upper and lower limit, filter within these:
  if (nrow(rad) != 0) {
    
    pris_tabell <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl) 
    names(pris_tabell) <- substring(names(pris_tabell),5) #removing the "rad." part of every colname
    
    return(pris_tabell)
  }
  
  else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Varetype-funksjon

choose_type <- function(type, tabell){
  #Make it case insensitive:
  type <- tolower(type)
  Varetype <- tolower(tabell$Varetype)
  rad <- tabell[grep(type, tabell$Varetype,ignore.case = TRUE, value = F), ] #using the grep function to see which rows that matches
                                                                             #the input in the Varetype column
  if(type == ""){
    return(tabell)
  }
  else if (nrow(rad)!=0){
    rad <- tabell[grep(type, tabell$Varetype,ignore.case = TRUE, value = F), ]
    tabell_type <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil,rad$Vareurl)
    names(tabell_type) <- substring(names(tabell_type),5) #removing the "rad." part of every colname
    
    return (tabell_type)
  } 
  else {
    return(NULL)
  }
  
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Land-funksjon

choose_country <- function(country, tabell){
  #Make it case insensitive:
  country <- tolower(country)
  land <- tolower(tabell$Land)
  
  rad <- tabell[grep(country, tabell$Land, ignore.case = TRUE, value = F), ] 
  
  if(country=="ingen preferanser"){
    return(tabell) # the pull down-menu for countries contains and option to choose "ingen preferanser" and thus, it will return the old table without any filtering on country
  }
  else if (nrow(rad)!=0){
    tabell_country <- data.frame(rad$Varenavn, rad$Varetype, rad$Land, rad$Volum, rad$Pris, rad$Passertil, rad$Vareurl)
    names(tabell_country) <- substring(names(tabell_country),5) #removing the "rad." part of every colname
    
    return (tabell_country)
  } 
  
  else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#PASSER-TIL-FUNKSJON
choose_fits <- function(fits, tabell){
  #Make it case insensitive:
  passertil <- tolower(tabell$Passertil)
  rad <- tabell[grep(fits, tabell$Passertil,ignore.case = TRUE, value = F), ]
  
  if(length(fits)==0){
    return(tabell)
  }
  
  else {
    for (i in 1:length(fits)){
      rad<-rad[grep(fits[i], rad$Passertil,ignore.case = TRUE, value = F), ]
      
    }
    
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
#To create an app with shiny you'll need two functions:
# 1. the UI-function 
# 2. the server-function


#Creating a vector of all the unique words in the "passertil"-column
fjern <-c("lyst", "kjøtt", "")
vektor_passer <- products %>% pull(Passertil) %>% strsplit(" ") %>% unlist %>% unique() 
vektor_passer <-append(vektor_passer[!vektor_passer%in%fjern],"lyst kjøtt")

'%then%' <- shiny:::'%OR%' 


#The UI-function is the function that will display the results to the user. This is what you'll see when running the app
#This is where the filtering menu will be displayed, and the user will type in and select what he/she wants such that the table of liquor will be filtered with the things he/she have chosen.
#After the filtering, the data table will also be displayed

#The way shiny works is that each of the filtering choices is displayed in the "sidebar panel", where each of the filtering choices get a special id. 
# this id is passed to the server-function which then takes the id's and call the pre-made functions above depending on what filtering the user wants to do
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
#the my_par function is the function that will do all the filtering as soon as the user clicks on the action button.   
  mypar <- eventReactive(input$full_f, {
    
    if (is.null(input$passertil)) {
      passertil <- ("")
    }
    else{
      passertil<-input$passertil
    }
    
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
    shiny::validate(
      need(!is.null(pris_tabell), 'Prisklassen er ikke gyldig med den filtrerte drikkevarenavnet. Vennligst prøv igjen.')
    )
    
    type_tabell <- choose_type(type, pris_tabell)
    
    shiny::validate(
      need(!is.null(type_tabell), 'Varetypen finnes ikke innenfor den gitte prisklassen og med det gitte navnet. Vennligst prøv igjen eller la boksen stå tom.')
    )
    
    country_tabell <- choose_country(land, type_tabell)
    shiny::validate(
      need(!is.null(country_tabell), 'Ingen varer fra dette landet innenfor de gitte filtreringene. Vennligst forsøk igjen.')
    )
    
    fits_tabell <- choose_fits(passertil, country_tabell)
    shiny::validate(
      need(!is.null(fits_tabell), 'Ingen varer som passer til ønsket mat innefor de gitte filtreringene. Vennligst prøv igjen eller la boksen stå tom.')
    )
    
    return(fits_tabell)
    
  })
  
  
  #Output table. This is the table containing the liquor Vinmonopolet has available on that date taking the filtering into consideration.
  #If what you'll typed in doesn't match with the products data set, it will send an error message and allow you to try again. 
  output$vin_table <- renderDataTable({
    
    #Checking whether the name and type the user typed in is in the products data table
    name_l <- tolower(input$name)
    Varenavn <- tolower(products$Varenavn)
    rad_name <- products[grep(name_l, products$Varenavn,ignore.case = TRUE, value = F), ]
    
    type_l <- tolower(input$type)
    Varetype <- tolower(products$Varetype)
    rad_type <- products[grep(type_l, products$Varetype,ignore.case = TRUE, value = F), ]
    
    shiny::validate(
      need(nrow(rad_name)!=0 || input$name=='', 'Varenavnet finnes ikke i vinmonolopoets lager. Vennligst prøv igjen eller la boksen stå tom.') %then%
        need(nrow(rad_type)!=0 || input$type=='', 'Varetypen finnes ikke i vinmonolopoets lager. Vennligst prøv igjen eller la boksen stå tom.') 
    )
    
    mypar()
    
  })
  
}


#create shiny object
shinyApp(ui, server)













