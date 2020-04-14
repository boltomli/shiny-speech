library(shiny)
library(text2speech)

ui <- fluidPage(

    titlePanel("Try Text-To-Speech (TTS)"),

    sidebarLayout(
        sidebarPanel(
            selectInput("service",
                        "Pick a service provider:",
                        c("Amazon",
                          "Google",
                          "Microsoft"),
                        selected = "Microsoft"),
            textInput("key",
                      "Key for the selected service:"),
            uiOutput("locale_selector"),
            uiOutput("voice_selector"),
            tags$hr(),
            helpText("View", a("source code on GitHub", href="https://github.com/boltomli/shiny-speech", target="_blank"))
        ),

        mainPanel(
            tableOutput("voices"),
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

    available_voices <- reactive({
        req(service_provider())
        if (input$key == "") {
            if (service_provider() == "microsoft") {
                tts_voices(service = service_provider())
            }
        } else {
            if (tts_auth(service = service_provider, key_or_json_file = input$key)) {
                tts_voices(service = service_provider())
            }
        }
    })

    available_locales <- reactive({
        req(available_voices())
        unique(available_voices()["language_code"])
    })

    output$locale_selector <- renderUI({
        req(service_provider())
        req(available_locales())
        selectInput("locale",
                    "Locale:",
                    available_locales())
    })

    output$voice_selector <- renderUI({
        req(available_voices())
        selectInput("voice",
                    "Voice:",
                    available_voices()[available_voices()$language_code == input$locale,"voice"])
    })

    output$voices <- renderTable({
        req(available_voices())
        available_voices()
    })
}

shinyApp(ui = ui, server = server)
