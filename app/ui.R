ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(bootswatch = "minty"),
  title = "Lav bankoplader og spil travebanko",
  window_title = "Spil banko og travebanko",
  sidebar = bslib::sidebar(
    open = "always",
    bslib::input_task_button(
      id = "render",
      label = "Generer plader",
      icon = shiny::icon("table-cells"),
      label_busy = "Arbejder...",
      icon_busy = rlang::missing_arg(),
      type = "primary",
      auto_reset = TRUE
    ),
    shiny::tags$hr(),
    shiny::numericInput(
      inputId = "n.cards",
      label = "Hvor mange plader skal du bruge?",
      value = 30,
      min = 1,
      max = 100
    ),
    shiny::numericInput(
      inputId = "seed",
      label = "Angiv udgave (seed)",
      value = abs(sample(.Random.seed, 1))
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
        min = 2,
        max = 20
      ),
      shiny::textInput(
        inputId = "footer",
        label = "Fodnote på poster",
        value = "Opsat {format(Sys.Date(),'%d-%m-%Y')}, nedtages samme dag. Post {sign.index} af {stops}."
      )
    ),
    shiny::conditionalPanel(
      condition = "output.rendered=='yes'",
      shiny::downloadButton(
        outputId = "pdf",
        label = "PDF",
        icon = shiny::icon("circle-down")
      )
    )
  ),
  bslib::card(
    bslib::card_header("Plader og evt. poster"),
    shiny::uiOutput("pdfview")
  )
)
