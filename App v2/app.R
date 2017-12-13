## app

# This is the user-interface definition and the server logic of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#load packages
library(shiny)

# Helper Functions
##############################


text <- c("Hi there! A technical defect on 05/02/2016 caused a TV set to ignite and set fire to parts of the living room. The fire could be extinguished, but parts of my living room are completely damaged. Kind regards, John Doe"
          , "Hi there! I just realized today that my Toyota shows some damage in the back right. My insurance policy number is 123456789. What do you need from me? Kind regards, John Smith"
          , "Hi there! When my friend Julia Smith moved in, I unfortunately dropped my friend's TV. It is broken. Can you help me? My policy number is 987654321. Kind regards, Jane Doe"
)


class <- c("Household", "Car", "Other")
chatbot <- c("Thank you for your message. Is it possible that you send us your insurance-ID?", "Thank you for using this opportunity to inform us. Please upload an image of the damage!", "Thank you for your message. One of our experts will contact you shortly.")
d.messages <- as.data.frame(class)
d.messages$message <- text
d.messages$chatbot <- chatbot

first <- c("John", "John", "Jane")
last <- c("Doe", "Smith", "Doe")

street <- c("St. Peterhofstatt 2",  "Grossmuensterplatz", "Raemistrasse 101")
city <- c("8001 Zurich", "8001 Zurich", "8092 Zurich")
country <- c("Switzerland", "Switzerland", "Switzerland")
phone <- c( "+41 79 555 55 55", "079 111 11 11", "077 333 33 33")
birthdate <- c("02.04.1978", "12.12.1987", "03.07.1980" )

d.user <- data.frame(first)
d.user$last <- last
d.user$street <- street
d.user$city <- city
d.user$country <- country
d.user$phone <- phone
d.user$birthdate <- birthdate


policy_split <- function(text){
    split1 <-  strsplit(text, split='policy number is ')[[1]][2]
    split2 <- strsplit(split1, split = '. ')[[1]][1]
    
    if (length(strsplit(split2, split ="")[[1]])== 9){
        return(split2)
    } else {
        return("Please verify the policy number!")
    }
    
    
}

first_split <- function(usertext){
    split1 <-  strsplit(usertext, split='Kind regards, ')[[1]][2]
    split2 <- strsplit(split1, split = ' ')[[1]][1]
    return(split2)
}
last_split <- function(usertext){
    split1 <-  strsplit(usertext, split='Kind regards, ')[[1]][2]
    split2 <- strsplit(split1, split = ' ')[[1]][2]
    return(split2)
}


data_user <- function(policy){
    if (policy == "123123123"){
        street <- d.user$street[1]
        city <- d.user$city[1]
        country <- d.user$country[1]
        phone <- d.user$phone[1]
        birthdate <- d.user$birthdate[1]
    } else{
        if (policy == "123456789"){
            street <- d.user$street[2]
            city <- d.user$city[2]
            country <- d.user$country[2]
            phone <- d.user$phone[2]
            birthdate <- d.user$birthdate[2]
        }
        else{
            if (policy == "987654321"){
                street <- d.user$street[3]
                city <- d.user$city[3]
                country <- d.user$country[3]
                phone <- d.user$phone[3]
                birthdate <- d.user$birthdate[3]
            }
            else{
                street <- "not found"
                city <- "not found"
                country <- "not found"
                phone <- "not found"
                birthdate <- "not found"
            }
            
        }}
    return(c(street, city, country, phone, birthdate))
}


# User Interface 
##############################

ui <- fluidPage(
    fluidRow(
        #phone
        column(4, style="padding:9% 4%; background-image: url(\"resized_phone.jpg\"); background-size:100% 100%; min-height:60em",
               #customer
               column(7,
                      tags$h1(htmlOutput("customer", inline = FALSE)),
                      wellPanel(style="border-color: #8b8b8b; background-color: darkorange",
                                textAreaInput(inputId="user",label="Input message", rows=12, value =  d.messages[1, 2 ]),
                                actionButton(inputId = "send", label = "Send")),
                      tags$style(HTML("#user{background-color:#f7a92a; font-size:20px}")),
                      wellPanel( style="border-color: #8b8b8b; background-color: darkorange", 
                                 actionButton(inputId = "uploadimage", label = "Upload Image")
                                 
                      )
               ),
               #chat bot
               column(5,
                      tags$h1(htmlOutput("bot", inline = FALSE)),
                      
                      wellPanel(style=" border-color: #8b8b8b; background-color: DeepSkyBlue",
                                tags$label("Bot's response"),
                                tags$p(textOutput("chatbot", inline = FALSE)),
                                tags$style(HTML("#chatbot{border:1px solid #ccc; padding:1em; color:#535559; font-size:20px}"))
                      ),
                      wellPanel(style="border-color: #8b8b8b; background-color: DeepSkyBlue",
                                textAreaInput(inputId="chatbot_class",label="Your message is classified as", rows=2, value =  "Car")),
                      
                      tags$style(HTML("#chatbot_class{background-color:DeepSkyBlue; font-size:20px}"))
               )
        ),
        #insurance
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
                                                       
                                                       column (6, 
                                                               uiOutput(outputId = "realimage1")),
                                                       column (4, offset = 2, textAreaInput(inputId = "textarea", label = "Comments", rows=5, width = 175))
                                                   ),
                                                   
                                                   tags$hr(),
                                                   tags$h5(htmlOutput("name1", inline = FALSE)),
                                                   
                                                   
                                                   fluidRow(
                                                       column(3, textInput(inputId = "first1", label="First", value = "")),
                                                       column(3, textInput(inputId = "last1", label="Last", value = ""))
                                                   )
                                                   
                                                   #,
                                                   # htmlOutput("address3", inline = FALSE),
                                                   # textInput(inputId = "street3", label="", value = ""),
                                                   # textInput(inputId = "city3", label="", value = ""),
                                                   # textInput(inputId = "country3", label="", value = ""),
                                                   # textInput(inputId = "phone3", label="Phone", value = ""),
                                                   # textInput(inputId = "email3", label="E-Mail", value = ""),
                                                   # textInput(inputId = "birthdate3", label="Birth Date", value = "")
                                        )),
                      
                      
                      conditionalPanel( "input.chatbot_class != 'Household'", 
                                        wellPanel( style="border-color: #8b8b8b; background-color: lightgrey; padding: 223px;")  )
                      
               ),
             
        column(6, 
               conditionalPanel( "input.chatbot_class == 'Car'",
               wellPanel( style="margin-top:5em;border-color: #8b8b8b; background-color: darkorange",
                          fluidRow(
                              column(2,
                                     uiOutput(outputId = "icon2")),
                              column(5,
                                     tags$h3(htmlOutput("form2", inline = FALSE))),
                              column(4, offset = 1,
                                     textInput(inputId = "policynumber2", label="Policy number", value = "")  )
                              ),
                   tags$hr(),
                   fluidRow (
                       column (6, 
                               uiOutput(outputId = "realimage2")),
                       column (4, offset = 2, textAreaInput(inputId = "textarea", label = "Comments", rows=5, width = 175))
                   ),
                   
                   tags$hr(),
                   tags$h5(htmlOutput("name2", inline = FALSE)),
                   
                   
                   fluidRow(
                       column(3, textInput(inputId = "first2", label="First", value = "")),
                       column(3, textInput(inputId = "last2", label="Last", value = ""))
                       )
                   
                   #,
                   # htmlOutput("address2", inline = FALSE),
                   # textInput(inputId = "street2", label="", value = ""),
                   # textInput(inputId = "city2", label="", value = ""),
                   # textInput(inputId = "country2", label="", value = ""),
                   # textInput(inputId = "phone2", label="Phone", value = ""),
                   # textInput(inputId = "email2", label="E-Mail", value = ""),
                   # textInput(inputId = "birthdate2", label="Birth Date", value = "")
               )
               ),
               
               conditionalPanel( "input.chatbot_class != 'Car'", 
                                 wellPanel( style="margin-top:5em; border-color: #8b8b8b; background-color: lightgrey; padding: 223px;")  )
               
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
                       
                       column (6, 
                               uiOutput(outputId = "realimage3")),
                       column (4, offset = 2, textAreaInput(inputId = "textarea", label = "Comments", rows=5, width = 175))
                   ),
                   
                   tags$hr(),
                   tags$h5(htmlOutput("name3", inline = FALSE)),
                   
                   
                   fluidRow(
                       column(3, textInput(inputId = "first3", label="First", value = "")),
                       column(3, textInput(inputId = "last3", label="Last", value = ""))
                       )
                   
                   #,
                   # htmlOutput("address3", inline = FALSE),
                   # textInput(inputId = "street3", label="", value = ""),
                   # textInput(inputId = "city3", label="", value = ""),
                   # textInput(inputId = "country3", label="", value = ""),
                   # textInput(inputId = "phone3", label="Phone", value = ""),
                   # textInput(inputId = "email3", label="E-Mail", value = ""),
                   # textInput(inputId = "birthdate3", label="Birth Date", value = "")
               )),
               
               
               conditionalPanel( "input.chatbot_class != 'Health'", 
                                 wellPanel( style="border-color: #8b8b8b; background-color: lightgrey; padding: 223px;")  )
               
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
                       column (6, 
                               uiOutput(outputId = "realimage4")),
                       column (4, offset = 2, textAreaInput(inputId = "textarea4", label = "Comments", rows=5, width = 175))
                   ),
                   
                   tags$hr(),
                   tags$h5(htmlOutput("name4", inline = FALSE)),
                   
                   
                   fluidRow(
                       column(3, textInput(inputId = "first4", label="First", value = "")),
                       column(3, textInput(inputId = "last4", label="Last", value = "")))
                       
                   
                   #,
                   # htmlOutput("address4", inline = FALSE),
                   # textInput(inputId = "street4", label="", value = ""),
                   # textInput(inputId = "city4", label="", value = ""),
                   # textInput(inputId = "country4", label="", value = ""),
                   # textInput(inputId = "phone4", label="Phone", value = ""),
                   # textInput(inputId = "email4", label="E-Mail", value = ""),
                   # textInput(inputId = "birthdate4", label="Birth Date", value = "")
               ) ),
               
               conditionalPanel( "input.chatbot_class != 'Others'", 
                                 wellPanel( style="border-color: #8b8b8b; background-color: lightgrey; padding: 223px;")  )
               
        
               
               
               )
        
        ))
    )
)

# Server with app logic
##############################

server <- function(input, output, session){
    
    output$customer<- renderUI({ HTML("<b>Customer ") })
    output$txt <- renderText({ input$user })
    
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
        
        output$chatbot <- renderText({
            input$send 
            
            isolate(
                if (input$user == d.messages[1,2]){
                    
                    d.messages[1, 3]
                    
                }
                else{if (input$user == d.messages[2,2]){
                    d.messages[2, 3]
                } else { if (input$user == d.messages[3,2]){
                    d.messages[3, 3]
                } else { if (input$user == "Yes, of course. My insurance policy number is 123123123. Kind regards, John Doe"){
                    "Thank you. We will process your claim."
                } else {
                    "Sorry to hear that."
                }
                    
                    
                    
                    
                }
                }
                    
                }
            )
            
        })
        
        if (input$user == d.messages[1,2]){
            
            updateTextInput(session, inputId = "user", label="Input message", value = "Yes, of course. My insurance policy number is 123123123. Kind regards, John Doe" )
            
        }
        
        output$class <- renderUI({ 
            input$send
            isolate( tags$h3(HTML( input$button )) )
            
        })
        
        output$realimage1 <- renderUI({ 
            input$uploadimage
            isolate( tags$img(src = "photo.png", height = 120, width = 170) )
            
        })
        
        
        updateTextInput(session, inputId = "last", value = last_split(input$user))
        updateTextInput(session, inputId = "first",  value = first_split(input$user))
        
        updateTextInput(session, inputId = "policynumber",  value = policy_split(input$user))
        updateTextInput(session, inputId = "street",  value = data_user(policy_split(input$user))[1])
        updateTextInput(session, inputId = "city",  value = data_user(policy_split(input$user))[2])
        updateTextInput(session, inputId = "country",  value = data_user(policy_split(input$user))[3])
        updateTextInput(session, inputId = "phone",  value = data_user(policy_split(input$user))[4])
        updateTextInput(session, inputId = "email",  value = paste0(first_split(input$user),last_split(input$user), "@company.com"))
        updateTextInput(session, inputId = "birthdate",  value = data_user(policy_split(input$user))[5])
    })
    
    observeEvent( input$uploadimage, {
        
        
        
        output$realimage <- renderUI({
            input$uploadimage
            isolate(if (input$policynumber == "123456789"){
                tags$img(src = "cardamage.png", heigt = 100, width = 170)
            } else { if (input$policynumber == "987654321"){
                tags$img(src = "TV.png", height = 120, width = 170)
            } else  { if (input$policynumber == "123123123"){
                tags$img(src = "desk.png", height = 120, width = 170)
            } else {
                tags$img(src = "photo.png", height = 120, width = 170)
            }
                
            }
                
            }
            )
        })
        
        
        
        output$chatbot <- renderText({
            input$uploadimage
            
            isolate(
                if (input$user == d.messages[2,2]){
                    "Picture 'damage.png' was uploaded. One of our experts will contact you shortly."
                    
                    
                }
                else{ "You have uploaded a picture!"
                } 
                
                
                
            )
            
        })
    })
    
    
}


shinyApp(ui = ui, server = server)