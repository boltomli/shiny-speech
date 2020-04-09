library(shiny)
library(text2speech)

ui <- fluidPage(

    titlePanel("Try Text-To-Speech (TTS)"),

    sidebarLayout(
        sidebarPanel(
            selectInput("service",
                        "Pick a service provider:",
                        c("Amazon" = "amazon",
                          "Google" = "google",
                          "Microsoft" = "microsoft"),
                        selected = "microsoft"),
            tags$hr(),
            helpText("View", a("source code on GitHub", href="https://github.com/boltomli/shiny-speech", target="_blank"))
        ),

        mainPanel(
            helpText("Key for authentication might be required for specific service provider."),
            tags$hr(),
            tableOutput("voices")
        )
    )
)

server <- function(input, output) {

    output$voices <- renderTable({
        tts_voices(service = input$service)
    })
}

shinyApp(ui = ui, server = server)
