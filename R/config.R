config <- function() {

  ui <- miniUI::miniPage(

    miniUI::gadgetTitleBar("Control settings for the scraper building:"),

    miniUI::miniContentPanel(

      sliderInput(inputId = "timeout", label = "Maximum request time for GET / POST request with httr.", min = 1, max = 10, value = 6),
      shiny::h5("The amount of target values to analyse is a tradeoff between accuracy and run time"),
      numericInput(inputId = "max_tv", label = "Maximum amount of target values:", min = 1, max = 10000, value = 10),
      sliderInput(inputId = "tv_ratio", label = "timeout", min = 0, max = 1, value = 0.4, step = 0.05),
      sliderInput(inputId = "param_fuzzy", label = "Fuzzy parameter for creation of new target values done by aregexec", min = 0, max = 1, value = 0.1, step = 0.05),
      sliderInput(inputId = "tv_extract_ratio", label = "Allowed distance between found texts and target values in html extraction", min = 0, max = 1, value = 0.1, step = 0.05)
    )
  )

  server <- function(input, output, session) {

    global <- reactiveValues(rmd_img_code = NULL)

    observeEvent(input$done, {
      sivis$max_target_Values <- input$max_tv
      sivis$timeout <- input$timeout
      sivis$tv_ratio_treshold <- input$tv_ratio
      sivis$param_fuzzy <- input$param_fuzzy
      sivis$tv_extract_ratio <- input$tv_extract_ratio

      stopApp()
    })

  }

  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
