# UI ---------------------------------------------------------------------------
ui <- shiny::navbarPage(
  title = "Multi Arm Minimisation",

  # Initialise -----------------------------------------------------------------

  tabPanel(
    title = "Initialise",
    shiny::fluidPage(

      rclipboard::rclipboardSetup(),

      shiny::sidebarLayout(

        shiny::sidebarPanel(
          shiny::fileInput("data", "Participant Data"),
          shiny::numericInput("groups", "Number of groups", value = 3, min = 2),
          shiny::checkboxInput("names", "Give group names?"),
          shiny::uiOutput("groupnamesInput"),
          shiny::selectInput("factors", "Factor variable(s)", choices = NULL,
                             multiple = T),
          shiny::numericInput("burnin", "Length of burnin period", min = 1,
                              value = 10),
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
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::actionButton("code", label = "Generate Code",
                              icon = shiny::icon(name = "code")),
          shiny::downloadButton("download", label = "Download allocations",
                                icon = shiny::icon(name = "file-csv")),
          shiny::downloadButton("downloadRData", label = "Download R object"),
          shiny::downloadButton("report", label = "Download report",
                                icon = shiny::icon(name = "file"))
        ),

        shiny::mainPanel(
          shiny::verbatimTextOutput("minimise"),
          shiny::verbatimTextOutput("balance"),
          shiny::plotOutput("plot"),
          shiny::uiOutput("clip"),
          shiny::verbatimTextOutput("code")
        )
      )
    )
  ),

  # Update ---------------------------------------------------------------------

  tabPanel(
    title = "Update",
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fileInput("data.mini", "Minimisation to update",
                           accept = ".rds"),
          shiny::fileInput("new.data", "New patients to minimise",
                           accept = ".csv")
        ),
        shiny::mainPanel(
          shiny::verbatimTextOutput("update"),
          shiny::verbatimTextOutput("update_balance"),
          shiny::plotOutput("update_plot"),
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {

  # Initialise -----------------------------------------------------------------

  data <- shiny::reactive({
    shiny::req(input$data)
    read.csv(input$data$datapath)
  })

  output$groupnamesInput <- shiny::renderUI({
    if(input$names) {
      lapply(1:input$groups, function(i) {
        shiny::textInput(paste0("name", i), NULL)
      })
    }
  })

  shiny::observe({
    shiny::req(input$data)
    shiny::updateSelectInput(session, "factors", choices = names(data()))
    shiny::updateNumericInput(session, "burnin", max = nrow(data()))
  })

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
                         choices = names(data()))
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

    minimise(data(), groups = input$groups, factors = input$factors,
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

  code <- shiny::eventReactive(input$code, {
    shiny::req(data(), input$factors)
    if(input$names) {
      names <- sapply(1:input$groups,
                      function(i) input[[paste0("name", i)]])
      names <- paste0("c(", paste0("\"", names, "\"", collapse = ", "), ")")
    } else {
      names <- "NULL"
    }

    minprob <- sapply(1:input$groups, function(i) input[[paste0("minprob", i)]])
    minprob <- paste(minprob, collapse = ", ")

    ratio <- sapply(1:input$groups, function(i) input[[paste0("ratio", i)]])
    ratio <- paste(ratio, collapse = ", ")

    if(input$stratify) {
      stratvar <- paste0("\"", input$stratvar, "\"")
    } else {
      stratvar <- "NULL"
    }

    if(input$seed) {
      seed.n <- input$seed.n
    } else {
      seed.n <- "NULL"
    }

    paste0(
      "install.packages(\"devtools\")\n",
      "devtools::install_github(\"EstherHerbert/multimini\")\n",
      "library(multimini)\n",
      "# Running the following code within R will produce the results\n",
      "# NB: if seed is not set then the results may differ\n",
      "data <- read.csv(\"", input$data$name, "\")  # make sure the file is in ",
      "your working directory\n",
      "mini <- minimise(data, ",
      "groups = ", input$groups, ", ",
      "factors = c(", paste0("\"", input$factors, "\"", collapse = ", "), "), ",
      "burnin = ", input$burnin, ", ",
      "minprob = c(", minprob, "), ",
      "ratio = c(", ratio, ")",
      ifelse(input$stratify, paste0(", stratify = ", stratvar), ""),
      ifelse(input$names, paste0(", group.names = ", names), ""),
      ifelse(input$seed, paste0(", seed = ", seed.n), ""),
      ")\n",
      "mini\n",
      "balance(mini)\n",
      "plot(mini)\n"
    )
  })

  output$code <- shiny::renderText({
    shiny::req(input$code)
    code()
  })

  output$clip <- shiny::renderUI({
    rclipboard::rclipButton(
      inputId = "clipbtn",
      label = "Copy",
      clipText = code(),
      icon = shiny::icon("clipboard")
    )
  })

  output$download <- shiny::downloadHandler(
    filename = function() {
      paste0("minimise_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(mini(), file, row.names = FALSE)
    }
  )

  output$downloadRData <- downloadHandler(
    filename = "minimise.rds",
    content = function(file) {
      df <- mini()
      saveRDS(df, file = file)
    }
  )


  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(system.file("shiny", "multimini", "report.Rmd",
                            package = "multimini"),
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

  # Update ---------------------------------------------------------------------

  data.mini <- shiny::reactive({
    shiny::req(input$data.mini)
    readRDS(input$data.mini$datapath)
  })

  new.data <- shiny::reactive({
    shiny::req(input$new.data)
    read.csv(input$new.data$datapath)
  })

  mini_u <- shiny::reactive({
    shiny::req(input$data.mini, input$new.data)
    update(data.mini(), new.data())
  })

  output$update <- shiny::renderPrint({
    mini_u()
  })

  output$update_balance <- shiny::renderPrint({
    balance(mini_u())
  })

  update_plots <- shiny::reactive({
    plot(mini_u(), show.plots = FALSE)
  })

  output$update_plot <- shiny::renderPlot({
    ggpubr::ggarrange(plotlist = update_plots(), ncol = 1)
  })

  session$onSessionEnded(stopApp)

}

# Run the application
shiny::shinyApp(ui = ui, server = server)
