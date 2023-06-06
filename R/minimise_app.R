#' App interface for the multimini package
#'
#' @param data patient data
#' @param ... options passed to `shiny::shinyApp()`
#'
#' @export
minimise_app <- function(data, ...) {

  ui <- shiny::fluidPage(

    shiny::titlePanel("Multi Arm Minimisation"),

    shiny::sidebarLayout(

      shiny::sidebarPanel(
        shiny::numericInput("groups", "Number of groups", value = 3, min = 2),
        shiny::selectInput("factors", "Factor variable(s)", choices = names(data),
                           multiple = T),
        shiny::numericInput("burnin", "Length of burnin period", min = 1,
                            max = nrow(data), value = 10),
        shiny::numericInput("minprob1", "Minimisation probabilities", min = 0,
                            max = 1, value = 0.8, step = 0.1),
        shiny::uiOutput("minprobInput"),
        shiny::numericInput("ratio1", "Ratios", min = 1, value = 1,
                            step = 1),
        shiny::uiOutput("ratioInput"),
        shiny::checkboxInput("stratify", "Stratify minimisation?"),
        shiny::uiOutput("stratifyInput")
      ),

      shiny::mainPanel(
            shiny::verbatimTextOutput("minimise"),
            shiny::verbatimTextOutput("balance"),
            shiny::plotOutput("plot")
      )
    )
  )

  server <- function(input, output) {

    output$minprobInput <- shiny::renderUI({
      lapply(2:input$groups, function(i) {
        shiny::numericInput(paste0("minprob", i), NULL, min = 0, max = 1,
                            value = 0.2/(input$groups - 1), step = 0.1)
      })
    })

    output$ratioInput <- shiny::renderUI({
      lapply(2:input$groups, function(i) {
        shiny::numericInput(paste0("ratio", i), NULL, min = 1, value = 1,
                            step = 1)
      })
    })

    output$stratifyInput <- shiny::renderUI({
      if(input$stratify) {
        shiny::selectInput("stratvar", "Stratification variable",
                           choices = names(data))
      }
    })

    mini <- shiny::reactive({
      shiny::req(input$minprob1); shiny::req(input$factors)
      minprob <- sapply(1:input$groups,
                        function(i) input[[paste0("minprob", i)]])
      if(input$stratify) {
        stratvar <- input$stratvar
      } else {
        stratvar <- NULL
      }
      minimise(data, groups = input$groups, factors = input$factors,
               burnin = input$burnin, minprob = minprob,
               stratify = stratvar)
    })

    output$minimise <- shiny::renderPrint({
      mini()
    })

    output$balance <- shiny::renderPrint({
      balance(mini())
    })

    plots <- shiny::reactive({
      plot(mini(), show.plots = FALSE)
    })

    output$plot <- shiny::renderPlot({
      ggpubr::ggarrange(plotlist = plots(), ncol = 1)
    })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server, ...)


}
