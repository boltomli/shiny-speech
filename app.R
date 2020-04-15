library(shiny)
library(text2speech)
library(XML)

ui <- fluidPage(

    titlePanel("Try Text-To-Speech (TTS) services"),

    sidebarLayout(
        sidebarPanel(
            selectInput("service",
                        "Pick a service provider:",
                        c("Amazon",
                          "Google",
                          "Microsoft"),
                        selected = "Microsoft"),
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

    service_provider <- reactive({
        switch (input$service,
            "Amazon" = "amazon",
            "Google" = "google",
            "Microsoft" = "microsoft"
        )
    })

    output$credential <- renderUI({
        if (service_provider() == "google") {
            fileInput("file_json", "Upload credential JSON file")
        } else if (service_provider() == "amazon") {
            list(
                textInput("key", "Enter key for authentication"),
                actionButton("set-key", "Send key to authenticate")
            )
        }
    })

    available_voices <- reactiveVal()

    # handle Amazon
    observeEvent(input[["set-key"]], {
        req(input$key)
        tryCatch({
            available_voices(tts_voices(service = "amazon", key_or_json_file = input$key))
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
    observe({
        if (service_provider() == "microsoft") {
            available_voices(tts_voices(service = "microsoft"))
        }
    })
    
    available_locales <- reactive({
        unique(available_voices()["language_code"])
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
