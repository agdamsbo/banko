

server <- function(input, output, session) {

  v <- shiny::reactiveValues(
    pdfout = NULL
  )

  shiny::observeEvent(input$render, {
    cards_r <- cards(input$n.cards,input$seed)

    if (input$travebanko == "yes"){
      v$pdfout <- cards_r |>
        travebanko(
          stops = input$stops,
          post.footer = input$footer)
    } else {
      v$pdfout <- cards_r |>
        purrr::map(gg_card) |>
        cards_grob()
    }

    v$pdfout |>
      ## This was the key, to
      export_pdf(path = "www/banko.pdf")

    output$pdfview <- shiny::renderUI({
      shiny::tags$iframe(style="height:600px; width:100%", src="banko.pdf")
    })
  })

  output$rendered <- shiny::reactive({
    if (is.null(v$pdfout)) {
      "no"
    } else {
      "yes"
    }
  })

  #####
  #### Generating output
  #####

  shiny::outputOptions(output, 'rendered', suspendWhenHidden = FALSE)

  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$pdf <- shiny::downloadHandler(
    filename = function() {
      glue::glue("banko.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      v$pdfout |>
        export_pdf(path = file)
    }
  )

  session$onSessionEnded(function() {
    cat("Session Ended\n")
    unlink("www/banko.pdf")
  })

}
