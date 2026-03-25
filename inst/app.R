library(shiny)
library(sasrvira)

ui <- fluidPage(
  title = "sasRvira demo",

  tags$head(
    tags$style(HTML("
      .code-box {
        height: 360px;
        display: flex;
      }

      .code-box .form-group {
        width: 100%;
        height: 100%;
        margin-bottom: 0;
      }

      .code-box textarea {
        width: 100% !important;
        height: 100% !important;
        border: solid #5B6DCD 1px;
        padding: 8px;
        font-family: monospace;
        font-size: 14px;
        resize: none;
      }

      .translate-btn {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 360px;
      }
    "))
  ),

  fluidRow(align = "center", h1("sasRvira demo")),

  fluidRow(

    # Left column -------------------------------------------------------------
    column(
      width = 5,
      h4("SAS code", align = "center"),
      div(
        class = "code-box",
        textAreaInput(
          inputId = "input_code",
          label = NULL,
          placeholder = "Paste your SAS code here...",
          value = ""
        )
      )
    ),

    # Middle button -----------------------------------------------------------
    column(
      width = 2,
      div(
        class = "translate-btn",
        actionButton(
          inputId = "translate",
          label   = "Translate",
          icon = icon("exchange-alt"),
          class = "btn-primary"
        )
      )
    ),

    # Right column ------------------------------------------------------------
    column(
      width = 5,
      h4("R code", align = "center"),
      div(
        class = "code-box",
        textAreaInput(
          inputId = "output_code",
          label = NULL,
          value = ""
        )
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$translate, {

    req(input$input_code)

    translated_code <- tryCatch(
      transastor(input$input_code),
      error = function(e) {
        paste("Error during translation:", e$message)
      }
    )

    updateTextAreaInput(
      session,
      inputId = "output_code",
      value = translated_code
    )
  })
}

shinyApp(ui = ui, server = server)
