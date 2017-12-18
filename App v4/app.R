## app

# This is the user-interface definition and the server logic of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#load packages
#############################
library(shiny)


### USER MESSAGE ##############
text <- c("Dear Mr. Smith! My insurance policy number is 983-035.2. A technical defect on 05/02/2016 caused a TV set to ignite and set fire to parts of the living room. The fire could be extinguished, but parts of my living room are completely damaged. Kind regards, John Doe"
          , "Hi there! I just realized today that my Toyota shows some damage in the back right. My insurance policy number is 173-038.5. What do you need from me? Please call me on 0796303274. Kind regards, John Smith"
          , "Hi! When my friend Julia Smith moved in, I slipped and broke my arm. Now the hospital wants money from me. Can you help me? My policy number is 675-892.4. Kind regards, Jane Doe"
          , "Dear Mrs. Simson. I need some information about your company. Please call me on my mobile phone. The number is 0765295374. Kind regards, Karen Clifford."
)

usermessage <- text[4]
#################

# import data
#############################
source('code/readDictionaryintoR.R', echo=FALSE)

classification_class <- c("Household", "Car", "Health", "Others")

# chatbot <- c("Thank you for your message. Is it possible that you send us your insurance-ID?", "Thank you for using this opportunity to inform us. Please upload an image of the damage!", "Thank you for your message. One of our experts will contact you shortly.")
# street <- c("St. Peterhofstatt 2",  "Grossmuensterplatz", "Raemistrasse 101")
# city <- c("8001 Zurich", "8001 Zurich", "8092 Zurich")
# country <- c("Switzerland", "Switzerland", "Switzerland")
# phone <- c( "+41 79 555 55 55", "079 111 11 11", "077 333 33 33")
# birthdate <- c("02.04.1978", "12.12.1987", "03.07.1980" )

# Helper Functions
##############################

# for hit prediction
vec_hit <- function(param.class, param.usermessage){
    vector <- sapply(param.class, grepl, param.usermessage)
    param.usermessage <- strsplit(param.usermessage, split=' ')[[1]]
    
    for (i in 1: length(param.class)){
        quantity <- grep(param.class[i], param.usermessage)
        
        if (vector[i] == TRUE){
            
            vector[i] <- length(quantity)
        } else {
            vector[i] <- 0
        }
    }
    
    return(vector)
}

numb_hit <- function(param.class, param.usermessage){
    return(sum(vec_hit(param.class, param.usermessage)))
    
}

# PREDICTION: number of hits
prediction_hits <- function(input){
    hits <- c(numb_hit(household, input), numb_hit(car, input), numb_hit(health, input))
    max_hit <- max(numb_hit(household, input), numb_hit(car, input), numb_hit(health, input))
    
    numb_hit(household, input)
    numb_hit(car, input)
    numb_hit(health, input)
    
    classification <- classification_class[which( hits ==  max_hit)]
    
    
    class_result <- if ( max(hits) > 2 ){
        if (max(abs(diff(hits))) > 1){
            if (max(abs(diff(hits))) == min(abs(diff(hits)))){
                classification
            }
            else { hits_sort <- sort(hits)
            if (abs(diff(hits_sort)[2]) > 1){
                if (length(classification) == 1){
                    classification
                } else {
                    "Others"
                }
            } else {
                "Others"
            }
            }
            
        } else {
            "Others"
        }
    } else {
        "Others"
    }
    
    return(class_result)
} 

# policy number 
policy <- function(usertext){
    pattern <- "\\d\\d\\d-\\d\\d\\d[[:punct:]]\\d"
    policy <- stringr::str_extract(usertext, pattern)
}

# find first name
first_split <- function(usertext){
    split1 <-  strsplit(usertext, split='Kind regards, ')[[1]][2]
    split2 <- strsplit(split1, split = ' ')[[1]][1]
    return(split2)
}

# find last name
last_split <- function(usertext){
    split1 <-  strsplit(usertext, split='Kind regards, ')[[1]][2]
    split2 <- strsplit(split1, split = ' ')[[1]][2]
    return(split2)
}

# find phone number
phone <- function(usertext){
    pattern <- "\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d"
    policy <- stringr::str_extract(usertext, pattern)
}


# User Interface 
##############################

ui <- fluidPage(
    fluidRow(
        # phone
        column(4, style="padding:9% 4%; background-image: url(\"resized_phone.jpg\"); background-size:100% 100%; min-height:60em",
               # customer side
               column(7,
                      tags$h1(htmlOutput("customer", inline = FALSE)),
                      wellPanel(style="border-color: #8b8b8b; background-color: darkorange",
                                textAreaInput(inputId="user",label="Input message", rows=12, value = usermessage),
                                actionButton(inputId = "send", label = "Send")),
                      tags$style(HTML("#user{background-color:#f7a92a; font-size:20px}")),
                      wellPanel( style="border-color: #8b8b8b; background-color: darkorange", 
                                 actionButton(inputId = "uploadimage", label = "Upload Image")
                                 
                      )
               ),
               # chat bot
               column(5,
                      tags$h1(htmlOutput("bot", inline = FALSE)),
                      
                      wellPanel(style=" border-color: #8b8b8b; background-color: DeepSkyBlue",
                                tags$label("Bot's response"),
                                tags$p(textOutput("chatbot", inline = FALSE)),
                                tags$style(HTML("#chatbot{border:1px solid #ccc; padding:1em; color:#535559; font-size:20px}"))
                      ),
                      wellPanel(style="border-color: #8b8b8b; background-color: DeepSkyBlue",
                                textAreaInput(inputId="chatbot_class",label="Your message is classified as", rows=2, value =  "Household")),
                      
                      tags$style(HTML("#chatbot_class{background-color:DeepSkyBlue; font-size:20px}"))
               )
        ),
        
        # insurance company side
        column(8,
               fluidRow(
                   column(6, 
                          tags$h1( htmlOutput("insurance", inline = FALSE)),
                          
                          
                          conditionalPanel( "input.chatbot_class == 'Household'",
                                            wellPanel( style="border-color: #8b8b8b; background-color: darkorange",
                                                       fluidRow(
                                                           column(2,
                                                                  uiOutput(outputId = "icon1")),
                                                           column(5,
                                                                  tags$h3(htmlOutput("form1", inline = FALSE))),
                                                           column(4, offset = 1, 
                                                                  textInput(inputId = "policynumber1", label="Policy number", value = ""))
                                                           
                                                       ),
                                                       tags$hr(),
                                                       
                                                       fluidRow (
                                                           
                                                           column(3, tags$h5(htmlOutput(outputId = "name1", inline = FALSE)), textInput(inputId = "first1", label="First", value = "")),
                                                           column(3, style="margin-top:2.5em;" , textInput(inputId = "last1", label="Last", value = "")),
                                                           column (4, offset = 2, uiOutput(outputId = "realimage1"))
                                                       ),
                                                       
                                                       tags$hr(),
                                                       
                                                       
                                                       
                                                       fluidRow(
                                                           column(3,  textInput(inputId = "phone1", label="Phone", value = "")), 
                                                           column(5,      textInput(inputId = "email1", label="E-Mail", value = "")),
                                                           column (4, 
                                                                   textAreaInput(inputId = "textarea1", label = "Comments", rows=4, width = 175)
                                                           )
                                                       )
                                                       #,
                                                       # htmlOutput(outputId = "address1", inline = FALSE),
                                                       # textInput(inputId = "street1", label="", value = ""),
                                                       # textInput(inputId = "city1", label="", value = ""),
                                                       # textInput(inputId = "country1", label="", value = ""),
                                                       # textInput(inputId = "birthdate1", label="Birth Date", value = "")
                                            )),
                          
                          
                          conditionalPanel( "input.chatbot_class != 'Household'", 
                                            wellPanel( style="border-color: #8b8b8b; background-color: lightgrey; padding: 224px;")  )
                          
                   ),
                   
                   column(6, 
                          conditionalPanel( "input.chatbot_class == 'Car'",
                                            wellPanel( style="margin-top:5em;border-color: #8b8b8b; background-color: darkorange",
                                                       fluidRow(
                                                           column(2,
                                                                  uiOutput(outputId = "icon2")),
                                                           column(5,
                                                                  tags$h3(htmlOutput(outputId = "form2", inline = FALSE))),
                                                           column(4, offset = 1,
                                                                  textInput(inputId = "policynumber2", label="Policy number", value = ""))
                                                       ),
                                                       
                                                       tags$hr(),
                                                       
                                                       fluidRow (
                                                           
                                                           column(3, tags$h5(htmlOutput(outputId = "name2", inline = FALSE)), textInput(inputId = "first2", label="First", value = "")),
                                                           column(3, style="margin-top:2.5em;" , textInput(inputId = "last2", label="Last", value = "")),
                                                           column (4, offset = 2, uiOutput(outputId = "realimage2"))
                                                       ),
                                                       
                                                       tags$hr(),
                                                       
                                                       fluidRow(
                                                           column(3, textInput(inputId = "phone2", label="Phone", value = "")), 
                                                           column(5, textInput(inputId = "email2", label="E-Mail", value = "")),
                                                           column (4, textAreaInput(inputId = "textarea2", label = "Comments", rows=4, width = 175))
                                                       )
                                                       #,
                                                       # htmlOutput(outputId = "address2", inline = FALSE),
                                                       # textInput(inputId = "street2", label="", value = ""),
                                                       # textInput(inputId = "city2", label="", value = ""),
                                                       # textInput(inputId = "country2", label="", value = ""),
                                                       # textInput(inputId = "birthdate2", label="Birth Date", value = "")
                                            )
                          ),
                          
                          conditionalPanel( "input.chatbot_class != 'Car'", 
                                            wellPanel( style="margin-top:5em; border-color: #8b8b8b; background-color: lightgrey; padding: 224px;")  )
                   ),
                   
                   column(6, 
                          conditionalPanel( "input.chatbot_class == 'Health'",
                                            wellPanel( style="border-color: #8b8b8b; background-color: darkorange",
                                                       fluidRow(
                                                           column(2,
                                                                  uiOutput(outputId = "icon3")),
                                                           column(5,
                                                                  tags$h3(htmlOutput("form3", inline = FALSE))),
                                                           column(4, offset = 1, 
                                                                  textInput(inputId = "policynumber3", label="Policy number", value = ""))
                                                       ),
                                                       
                                                       tags$hr(),
                                                       
                                                       fluidRow (
                                                           
                                                           column(3, tags$h5(htmlOutput(outputId ="name3", inline = FALSE)), textInput(inputId = "first3", label="First", value = "")),
                                                           column(3, style="margin-top:2.5em;" , textInput(inputId = "last3", label="Last", value = "")),
                                                           column (4, offset = 2, uiOutput(outputId = "realimage3"))
                                                       ),
                                                       
                                                       tags$hr(),
                                                       
                                                       
                                                       
                                                       fluidRow(
                                                           column(3,  textInput(inputId = "phone3", label="Phone", value = "")), 
                                                           column(5,      textInput(inputId = "email3", label="E-Mail", value = "")),
                                                           column (4,
                                                                   textAreaInput(inputId = "textarea3", label = "Comments", rows=4, width = 175)
                                                           )
                                                       )
                                                       
                                                       #,
                                                       # htmlOutput(outputId = "address3", inline = FALSE),
                                                       # textInput(inputId = "street3", label="", value = ""),
                                                       # textInput(inputId = "city3", label="", value = ""),
                                                       # textInput(inputId = "country3", label="", value = ""),
                                                       # textInput(inputId = "birthdate3", label="Birth Date", value = "")
                                            )),
                          
                          
                          conditionalPanel( "input.chatbot_class != 'Health'", 
                                            wellPanel( style="border-color: #8b8b8b; background-color: lightgrey; padding: 224px;")  )
                          
                   ),
                   
                   column(6, 
                          
                          conditionalPanel( "input.chatbot_class == 'Others'",
                                            wellPanel( style="border-color: #8b8b8b; background-color: darkorange",
                                                       fluidRow(
                                                           column(2,
                                                                  uiOutput(outputId = "icon4")),
                                                           column(5,
                                                                  tags$h3(htmlOutput("form4", inline = FALSE))),
                                                           column(4, offset = 1,
                                                                  textInput(inputId = "policynumber4", label="Policy number", value = "")
                                                           )
                                                       ),
                                                       tags$hr(),
                                                       
                                                       fluidRow (
                                                           
                                                           column(3, tags$h5(htmlOutput(outputId = "name4", inline = FALSE)), textInput(inputId = "first4", label="First", value = "")),
                                                           column(3, style="margin-top:2.5em;" , textInput(inputId = "last4", label="Last", value = "")),
                                                           column (4, offset = 2, uiOutput(outputId = "realimage4"))
                                                       ),
                                                       
                                                       tags$hr(),
                                                       
                                                       fluidRow(
                                                           column(3,  textInput(inputId = "phone4", label="Phone", value = "")), 
                                                           column(5,      textInput(inputId = "email4", label="E-Mail", value = "")),
                                                           column (4,
                                                                   textAreaInput(inputId = "textarea4", label = "Comments", rows=4, width = 175)
                                                           )
                                                       )
                                                       
                                                       #,
                                                       # htmlOutput(outputId = "address4", inline = FALSE),
                                                       # textInput(inputId = "street4", label="", value = ""),
                                                       # textInput(inputId = "city4", label="", value = ""),
                                                       # textInput(inputId = "country4", label="", value = ""),
                                                       # textInput(inputId = "birthdate4", label="Birth Date", value = "")
                                            ) ),
                          
                          conditionalPanel( "input.chatbot_class != 'Others'", 
                                            wellPanel( style="border-color: #8b8b8b; background-color: lightgrey; padding: 224px;")  )
     
                   )
                   
               ))
    )
)

# Server with app logic
##############################

server <- function(input, output, session){
    
    output$customer<- renderUI({ HTML("<b>Customer ") })
    
    output$bot <- renderUI({ HTML("<b>Chat bot ") })
    output$chatbot <- renderText("Welcome! Please describe your problem.")
    output$insurance <- renderUI({ HTML("<b>Insurance company") })
    output$icon1 <- renderUI({
        
        tags$img(src = "household.png")
        
    })
    output$icon2 <- renderUI({
        
        tags$img(src = "car.png")
        
    })
    output$icon3 <- renderUI({
        
        tags$img(src = "healthcare.png")
        
    })
    output$icon4 <- renderUI({
        
        tags$img(src = "others.png")
        
    })
    
    output$form1 <- renderUI({ HTML("<b>Form: Household") })
    output$form2 <- renderUI({ HTML("<b>Form: Car") })
    output$form3 <- renderUI({ HTML("<b>Form: Health") })
    output$form4 <- renderUI({ HTML("<b>Form: Others") })
    
    output$name1 <- renderUI({ HTML("<b>Name") })
    output$name2 <- renderUI({ HTML("<b>Name") })
    output$name3 <- renderUI({ HTML("<b>Name") })
    output$name4 <- renderUI({ HTML("<b>Name") })
    output$address <- renderUI({ HTML("<b>Address") })
    
    output$realimage1 <- renderUI({
        
        tags$img(src = "photo.png", height = 120, width = 170)
        
    })
    
    output$realimage2 <- renderUI({
        
        tags$img(src = "photo.png", height = 120, width = 170)
        
    })
    
    output$realimage3 <- renderUI({
        
        tags$img(src = "photo.png", height = 120, width = 170)
        
    })
    
    output$realimage4 <- renderUI({
        
        tags$img(src = "photo.png", height = 120, width = 170)
        
    })
    
    observeEvent(input$send, {
        
        # transform to lower case
        lower_usermessage <- tolower(input$user)
        
        # remove punctuation (.-)
        rmpunct_usermessage <- tm::removePunctuation(lower_usermessage)
        
        #remove stopwords using the standard list in tm
        final_usermessage <- tm::removeWords(rmpunct_usermessage, stopwords("english"))
        final_usermessage <- tm::removeWords(final_usermessage, "regards")
        
        class_result <- prediction_hits(final_usermessage)
        
        updateTextInput(session, inputId = "chatbot_class", value = class_result )
        
        if (class_result == classification_class[1]){
            updateTextInput(session, inputId = "policynumber1",  value = policy(input$user))
            updateTextInput(session, inputId = "first1",  value = first_split(input$user))
            updateTextInput(session, inputId = "last1", value = last_split(input$user))
            updateTextInput(session, inputId = "phone1",  value = phone(input$user))
            if (!is.na(first_split(input$user)) == TRUE && !is.na(last_split(input$user)) == TRUE ){
                updateTextInput(session, inputId = "email1",  value = paste0(first_split(input$user),last_split(input$user), "@company.com"))
            } else {
                updateTextInput(session, inputId = "email1",  value = "")
            }
        } else {
            if (class_result == classification_class[2]){
                updateTextInput(session, inputId = "policynumber2",  value = policy(input$user))
                updateTextInput(session, inputId = "first2",  value = first_split(input$user))
                updateTextInput(session, inputId = "last2", value = last_split(input$user))
                updateTextInput(session, inputId = "phone2",  value = phone(input$user))
                if (!is.na(first_split(input$user)) == TRUE && !is.na(last_split(input$user)) == TRUE ){
                updateTextInput(session, inputId = "email2",  value = paste0(first_split(input$user),last_split(input$user), "@company.com"))
                } else {
                    updateTextInput(session, inputId = "email2",  value = "")
                }
                
            } else {
                if (class_result == classification_class[3]) {
                    updateTextInput(session, inputId = "policynumber3",  value = policy(input$user))
                    updateTextInput(session, inputId = "first3",  value = first_split(input$user))
                    updateTextInput(session, inputId = "last3", value = last_split(input$user))
                    updateTextInput(session, inputId = "phone3",  value = phone(input$user))
                    if (!is.na(first_split(input$user)) == TRUE && !is.na(last_split(input$user)) == TRUE ){
                    updateTextInput(session, inputId = "email3",  value = paste0(first_split(input$user),last_split(input$user), "@company.com"))
                    } else {
                        updateTextInput(session, inputId = "email3",  value = "")
                    }
                } else {
                    updateTextInput(session, inputId = "policynumber4",  value = policy(input$user))
                    updateTextInput(session, inputId = "first4",  value = first_split(input$user))
                    updateTextInput(session, inputId = "last4", value = last_split(input$user))
                    updateTextInput(session, inputId = "phone4",  value = phone(input$user))
                    if (!is.na(first_split(input$user)) == TRUE && !is.na(last_split(input$user)) == TRUE ){
                    updateTextInput(session, inputId = "email4",  value = paste0(first_split(input$user),last_split(input$user), "@company.com"))
                    } else {
                        updateTextInput(session, inputId = "email4",  value = "")
                    }
                }
            }
        }
        
        output$chatbot <- renderText({
            input$send
            
            isolate( 
                "Thank you for using this opportunity to inform us."
               
                # chatbot <- c("Thank you for your message. Is it possible that you send us your insurance-ID?",  Please upload an image of the damage!", "Thank you for your message. One of our experts will contact you shortly.")
                # 
                # else{if (input$user == d.messages[2,2]){
                #     d.messages[2, 3]
                # } else { if (input$user == d.messages[3,2]){
                #     d.messages[3, 3]
                # } else { if (input$user == "Yes, of course. My insurance policy number is 123123123. Kind regards, John Doe"){
                #     "Thank you. We will process your claim."
                # } else {
                #     "Sorry to hear that."
                # }
                # 
                #                 }
                #                 }
                # 
                #                 }
            )
            
        })
        
        # if (input$user == d.messages[1,2]){
        # 
        #     updateTextInput(session, inputId = "user", label="Input message", value = "Yes, of course. My insurance policy number is 123123123. Kind regards, John Doe" )
        # 
        # }
        
        
        output$realimage1 <- renderUI({ 
            input$uploadimage
            isolate( tags$img(src = "photo.png", height = 120, width = 170) )
            
        })
        output$realimage2 <- renderUI({ 
            input$uploadimage
            isolate( tags$img(src = "photo.png", height = 120, width = 170) )
            
        })
        output$realimage3 <- renderUI({ 
            input$uploadimage
            isolate( tags$img(src = "photo.png", height = 120, width = 170) )
            
        })
        output$realimage4 <- renderUI({ 
            input$uploadimage
            isolate( tags$img(src = "photo.png", height = 120, width = 170) )
            
        })
        
        # updateTextInput(session, inputId = "last", value = last_split(input$user))
        # updateTextInput(session, inputId = "first",  value = first_split(input$user))
        # 
        # updateTextInput(session, inputId = "street",  value = data_user(policy_split(input$user))[1])
        # updateTextInput(session, inputId = "city",  value = data_user(policy_split(input$user))[2])
        # updateTextInput(session, inputId = "country",  value = data_user(policy_split(input$user))[3])
        # updateTextInput(session, inputId = "phone",  value = data_user(policy_split(input$user))[4])
        # updateTextInput(session, inputId = "email",  value = paste0(first_split(input$user),last_split(input$user), "@company.com"))
        # updateTextInput(session, inputId = "birthdate",  value = data_user(policy_split(input$user))[5])
    })
    
    observeEvent( input$uploadimage, {
        
        isolate(if (input$chatbot_class == "Household"){
            output$realimage1 <- renderUI({
                input$uploadimage
                tags$img(src = "desk.png", heigt = 100, width = 170)
            })
            
        } else { if (input$chatbot_class == "Car"){
            output$realimage2 <- renderUI({
                input$uploadimage
                tags$img(src = "cardamage.png", height = 120, width = 170)
            })
            
        } else  { if (input$chatbot_class == "Health"){
            output$realimage3 <- renderUI({
                input$uploadimage
                tags$img(src = "claim.png", height = 120, width = 170)
            })
            
        } else { 
            output$realimage4 <- renderUI({
                input$uploadimage
                tags$img(src = "TV.png", height = 120, width = 170)
            })
            
        }
            
        } 
        }
        )
        
        output$chatbot <- renderText({
            input$uploadimage
            
            isolate(
                "You have uploaded a picture!"
                
            )
            
        })
    })
    
    
}


shinyApp(ui = ui, server = server)