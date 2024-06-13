ui <- shiny::fluidPage(
  shiny::titlePanel("Lav bankoplader og spil travebanko",
                    windowTitle = "banko"
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::actionButton(
        inputId = "render",
        label = "Generer plader",
        icon = shiny::icon("qrcode"), width = "100%"
      ),
      shiny::tags$hr(),
      shiny::numericInput(
        inputId = "n.cards",
        label = "Hvor mange plader skal du bruge?",
        value = 30,
        min=1,
        max=100
      ),
      shiny::numericInput(
        inputId = "seed",
        label = "Angiv udgave (seed)",
        value = abs(sample(.Random.seed,1))
      ),
      shiny::radioButtons(
        inputId = "travebanko",
        label = "Spil travebanko?",
        selected = "no",
        choices = list(
          "Ja" = "yes",
          "Nej" = "no"
        )
      ),
      shiny::tags$hr(),
      shiny::conditionalPanel(
        condition = "input.travebanko=='yes'",
        shiny::numericInput(
          inputId = "stops",
          label = "Angiv antal poster",
          value = 5,
          min=2,
          max=20
        ),
        shiny::textInput(
          inputId = "footer",
          label = "Fodnote på poster",
          value = "Opsat {Sys.Date()}, nedtages samme dag.")
      ),
      shiny::conditionalPanel(
        condition = "output.rendered=='yes'",
        shiny::downloadButton(
          outputId = "pdf",
          label = "PDF",
          icon = shiny::icon("vector-square")
        )
      )
    ),
    shiny::mainPanel(
      shiny::uiOutput("pdfview")
    )
  )
)
