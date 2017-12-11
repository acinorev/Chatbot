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
        column(6, style="padding:10% 5%; background-image: url(\"resized_phone.jpg\"); background-size:100% 100%; min-height:75em",
               #customer
               column(7,
                   tags$h1(htmlOutput("customer", inline = FALSE)),
                   wellPanel(style="border-color: #8b8b8b; background-color: darkorange",
                             radioButtons(inputId = "button", label = "Cases", choices = d.messages$class)),
                   wellPanel(style="border-color: #8b8b8b; background-color: darkorange",
                             textAreaInput(inputId="user",label="Input message", rows=8),
                             actionButton(inputId = "send", label = "Send")),
                   tags$style(HTML("#user{background-color:#f7a92a; font-size:20px}")),
                   wellPanel( style="border-color: #8b8b8b; background-color: darkorange", 
                              actionButton(inputId = "uploadimage", label = "Upload Image")
                              
                   )
               ),
               #chat bot
               column(5,
                   tags$h1(htmlOutput("bot", inline = FALSE)),
                   
                   wellPanel(style="margin-top:13em; border-color: #8b8b8b; background-color: DeepSkyBlue",
                       tags$label("Bot's response"),
                       tags$p(textOutput("chatbot", inline = FALSE)),
                       tags$style(HTML("#chatbot{border:1px solid #ccc; padding:1em; color:#535559; font-size:20px}"))
                   )
                )
        ),
        #insurance
        column(6, 
               tags$h1( htmlOutput("insurance", inline = FALSE)),
               
               wellPanel( 
                   tags$h4(htmlOutput("form", inline = FALSE)),
                   tags$hr(),
                   fluidRow (
                       column (6, tags$h4(htmlOutput("class", inline = FALSE))),
                       column (4, offset=2, 
                               uiOutput(outputId = "realimage"))
                   ),
                   
                   tags$hr(),
                   tags$h5(htmlOutput("name", inline = FALSE)),
                   
                   
                   fluidRow(
                       column(3, textInput(inputId = "first", label="First", value = "")),
                       column(3, textInput(inputId = "last", label="Last", value = "")),
                       column (4, offset=2, textAreaInput(inputId = "textarea", label = "Comments", rows=5, width = 175))),
                   
                   textInput(inputId = "policynumber", label="Policy number", value = ""),
                   htmlOutput("address", inline = FALSE),
                   textInput(inputId = "street", label="", value = ""),
                   textInput(inputId = "city", label="", value = ""),
                   textInput(inputId = "country", label="", value = ""),
                   textInput(inputId = "phone", label="Phone", value = ""),
                   textInput(inputId = "email", label="E-Mail", value = ""),
                   textInput(inputId = "birthdate", label="Birth Date", value = "")
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
    output$form <- renderUI({ HTML("<b>Form") })
    output$name <- renderUI({ HTML("<b>Name") })
    output$address <- renderUI({ HTML("<b>Address") })
    
    observe({
        input$button
        
        updateTextInput(session, inputId = "user", label="Input message", value =  d.messages[which(d.messages$class == input$button), 2 ] )
    })
    
    
    output$realimage <- renderUI({
        
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
                    "Sorry, I do not understand you."
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
        
        output$realimage <- renderUI({ 
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