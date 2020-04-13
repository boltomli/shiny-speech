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
    output$locale_selector <- renderUI({
        req(service_provider())
        service_provider = service_provider()
        if (input$key == "") {
            if (service_provider == "microsoft") {
                selectInput("locale",
                            "Locale:",
                            unique(tts_voices(service = service_provider)["language_code"]))
            } else {
                helpText("Follow document of each service provider for authentication method.")
            }
        } else {
            if (tts_auth(service = service_provider, key_or_json_file = input$key)) {
                selectInput("locale",
                            "Locale:",
                            unique(tts_voices(service = service_provider)["language_code"]))
            }
        }
    })
    output$voices <- renderTable({
        req(service_provider())
        tts_voices(service = service_provider())
    })
}

shinyApp(ui = ui, server = server)
