library(shiny)
library(text2speech)
library(XML)
library(mscstts)
library(aws.polly)

ui <- fluidPage(

    titlePanel("Try Text-To-Speech (TTS) services"),

    sidebarLayout(
        sidebarPanel(
            selectInput("service",
                        "Pick a service provider:",
                        c("Amazon" = "amazon",
                          "Google" = "google",
                          "Microsoft" = "microsoft"),
                        selected = "microsoft"),
            uiOutput("credential"),
            tags$hr(),
            helpText("View", a("source code on GitHub", href="https://github.com/boltomli/shiny-speech", target="_blank"))
        ),

        mainPanel(
            uiOutput("locale_selector"),
            uiOutput("voice_selector"),
            textInput("text", "Input text to speak"),
            textOutput("ssml")
        )
    )
)

server <- function(input, output) {

    output$credential <- renderUI({
        if (input$service == "amazon") {
            list(
                textInput("key", "Enter access key ID"),
                textInput("secret", "Enter secret access key"),
                actionButton("set-key", "Send key to authenticate")
            )
        } else if (input$service == "google") {
            fileInput("file_json", "Upload credential JSON file")
        } else if (input$service == "microsoft") {
            list(
                selectInput("region", "Select a Microsoft Azure region", ms_regions()),
                textInput("key", "Enter key for the selected region"),
                actionButton("set-key-region", "Send key to authenticate")
            )
        }
    })

    available_voices <- reactiveVal()

    # handle Amazon
    observeEvent(input[["set-key"]], {
        req(input$key, input$secret)
        tryCatch({
            voices <- pollyHTTP("voices", key = input$key, secret = input$secret)$Voices
            names(voices)[names(voices) == "Name"] <- "voice"
            names(voices)[names(voices) == "LanguageCode"] <- "language_code"
            names(voices)[names(voices) == "Gender"] <- "gender"
            voices = voices[, c("voice", "language_code", "gender")]
            voices$service = "amazon"
            available_voices(voices)
        }, error = function(e){
            showNotification(as.character(safeError(e)), type = "warning")
        })
    })

    # handle Google
    observe({
        req(input$file_json)
        tryCatch({
            available_voices(tts_voices(service = "google", key_or_json_file = input$file_json$datapath[[1]]))
        }, error = function(e){
            showNotification(as.character(safeError(e)), type = "warning")
        })
    })

    # handle Microsoft
    observeEvent(input[["set-key-region"]], {
        req(input$region)
        if (input$key == "") {
            available_voices(tts_voices(service = "microsoft"))
        } else {
            tryCatch({
                voices <- ms_list_voices(api_key = input$key, region = input$region)
                names(voices)[names(voices) == "ShortName"] <- "voice"
                names(voices)[names(voices) == "Locale"] <- "language_code"
                names(voices)[names(voices) == "Gender"] <- "gender"
                voices = voices[, c("voice", "language_code", "gender")]
                voices$service = "microsoft"
                available_voices(voices)
            }, error = function(e){
                showNotification(as.character(safeError(e)), type = "warning")
            })
        }
    })
    
    available_locales <- reactive({
        available_voices <- available_voices()
        if (!is.null(available_voices)) {
            unique(available_voices[order(available_voices$language_code),]["language_code"])
        }
    })

    observe({
        available_locales <- available_locales()
        output$locale_selector <- renderUI({
            selectInput("locale",
                        "Select from available locales:",
                        available_locales$language_code)
        })
    })

    observe({
        req(input$locale)
        available_voices <- available_voices()
        output$voice_selector <- renderUI({
            selectInput("voice",
                        "Select from available voices:",
                        available_voices[available_voices$language_code == input$locale,"voice"])
        })
    })

    observe({
        req(input$text, input$voice)
        voice <- available_voices()[available_voices()$voice == input$voice,]
        name <- voice$voice
        lang <- tolower(voice$language_code)
        gender <- voice$gender
        ssml <- newXMLDoc()
        ns <- c(xml = "http://www.w3.org/2000/xmlns")
        speak <- newXMLNode("speak", namespace = ns)
        addAttributes(speak, "version" = "1.0", "xml:lang" = lang)
        voice <- newXMLNode("voice", namespace = ns)
        addAttributes(voice, "xml:lang" = lang, "xml:gender" = gender, "name" = name)
        textNode <- newXMLTextNode(text = input$text)
        addChildren(voice, textNode)
        addChildren(speak, voice)
        addChildren(ssml, speak)
        output$ssml <- renderText({
            toString.XMLNode(ssml)
        })
    })
}

shinyApp(ui = ui, server = server)
