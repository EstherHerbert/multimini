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
        shiny::checkboxInput("names", "Give group names?"),
        shiny::uiOutput("groupnamesInput"),
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
        shiny::uiOutput("stratifyInput"),
        shiny::checkboxInput("seed", "Set seed?"),
        shiny::uiOutput("seedInput"),
        shiny::actionButton("minimise", "Minimise"),
        shiny::downloadButton("download", label = "Download csv"),
        shiny::downloadButton("report", label = "Generate report")
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

    output$groupnamesInput <- shiny::renderUI({
      if(input$names) {
        lapply(1:input$groups, function(i) {
          shiny::textInput(paste0("name", i), NULL)
        })
      }
    })

    output$seedInput <- shiny::renderUI({
      if(input$seed) {
        shiny::numericInput("seed.n", NULL, value = NULL)
      }
    })

    mini <- shiny::eventReactive(input$minimise, {
      shiny::req(input$minprob1); shiny::req(input$factors)
      if(input$names) {
        names <- sapply(1:input$groups,
                        function(i) input[[paste0("name", i)]])
      } else {
        names <- NULL
      }
      minprob <- sapply(1:input$groups,
                        function(i) input[[paste0("minprob", i)]])
      ratio <- sapply(1:input$groups, function(i) input[[paste0("ratio", i)]])
      if(input$stratify) {
        stratvar <- input$stratvar
      } else {
        stratvar <- NULL
      }
      if(input$seed) {
        seed.n <- input$seed.n
      } else {
        seed.n <- NULL
      }

      minimise(data, groups = input$groups, factors = input$factors,
               burnin = input$burnin, minprob = minprob, ratio = ratio,
               stratify = stratvar, group.names = names, seed = seed.n)
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

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("minimise_", format(Sys.time(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write.csv(mini(), file)
      }
    )

    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(system.file("report.Rmd", package = "multimini"),
                  tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(mini = mini(), plots = plots())

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server, ...)


}
