#' App interface for the multimini package
#'
#' @export
minimise_app <- function() {

  # UI ---------------------------------------------------------------------------
  ui <- shiny::navbarPage(
    title = "Multi Arm Minimisation",

    # Initialise -----------------------------------------------------------------

    shiny::tabPanel(
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
            shiny::numericInput("minprob", "Minimisation probability", min = 0,
                                max = 1, value = 0.8, step = 0.1),
            shiny::numericInput("ratio1", "Ratios", min = 1, value = 1,
                                step = 1),
            shiny::uiOutput("ratioInput"),
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

    shiny::tabPanel(
      title = "Update",
      shiny::fluidPage(
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::fileInput("data.mini", "Minimisation to update",
                             accept = ".rds"),
            shiny::radioButtons("new.format", "New patients",
                                choices = c("As .csv", "Input factors")),
            shiny::uiOutput("new.dataInput"),
            shiny::actionButton("update", "Update minimisation"),
            shiny::downloadButton("update_download",
                                  label = "Download allocations",
                                  icon = shiny::icon(name = "file-csv")),
            shiny::downloadButton("update_downloadRData",
                                  label = "Download R object"),
            shiny::downloadButton("update_report", label = "Download report",
                                  icon = shiny::icon(name = "file"))
          ),
          shiny::mainPanel(
            shiny::verbatimTextOutput("update"),
            shiny::verbatimTextOutput("update_balance"),
            shiny::plotOutput("update_plot"),
          )
        )
      )
    ),
    # Simulate -----------------------------------------------------------------
    shiny::tabPanel(
      title = "Simulate",
      shiny::fluidPage(
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::numericInput("sampsize", "Sample Size", value = 100),
            shiny::textInput("simfactors", "Factors"),
            shiny::uiOutput("factorInput"),
            shiny::numericInput("Nsims", "Number of simulations per scenario",
                                10, min = 1),
            shiny::numericInput("simgroups", "Number of groups", 3, min = 2),
            shiny::textInput("simburnin", "Burnin options"),
            shiny::textInput("simminprob", "Minimisation probability options"),
            shiny::numericInput("simratio1", "Ratios", min = 1, value = 1,
                                step = 1),
            shiny::uiOutput("simratioInput"),
            shiny::actionButton("simulate", "Run simulations")
          ),
          shiny::mainPanel(
            shiny::verbatimTextOutput("temp")
          )
        )
      )
    )
  )

  # Server ---------------------------------------------------------------------

  server <- function(input, output, session) {

    # Initialise ---------------------------------------------------------------

    data <- shiny::reactive({
      shiny::req(input$data)
      utils::read.csv(input$data$datapath)
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

    output$ratioInput <- shiny::renderUI({
      lapply(2:input$groups, function(i) {
        shiny::numericInput(paste0("ratio", i), NULL, min = 1, value = 1,
                            step = 1)
      })
    })

    output$seedInput <- shiny::renderUI({
      if(input$seed) {
        shiny::numericInput("seed.n", NULL, value = NULL)
      }
    })

    mini <- shiny::eventReactive(input$minimise, {
      shiny::req(input$minprob); shiny::req(input$factors)
      if(input$names) {
        names <- sapply(1:input$groups,
                        function(i) input[[paste0("name", i)]])
      } else {
        names <- NULL
      }
      ratio <- sapply(1:input$groups, function(i) input[[paste0("ratio", i)]])

      if(input$seed) {
        seed.n <- input$seed.n
      } else {
        seed.n <- NULL
      }

      minimise(data(), groups = input$groups, factors = input$factors,
               burnin = input$burnin, minprob = input$minprob, ratio = ratio,
               group.names = names, seed = seed.n)
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
      print(plots())
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

      ratio <- sapply(1:input$groups, function(i) input[[paste0("ratio", i)]])
      ratio <- paste(ratio, collapse = ", ")

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
        "minprob = ", input$minprob, ", ",
        "ratio = c(", ratio, ")",
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
        utils::write.csv(mini(), file, row.names = FALSE)
      }
    )

    output$downloadRData <- shiny::downloadHandler(
      filename = "minimise.rds",
      content = function(file) {
        df <- mini()
        saveRDS(df, file = file)
      }
    )

    output$report <- shiny::downloadHandler(
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

    output$new.dataInput <- shiny::renderUI({
      switch(
        input$new.format,
        "As .csv" = shiny::fileInput("new.data", "New patients to minimise",
                                     accept = ".csv"),
        "Input factors" = lapply(factors(data.mini()), function(i) {
          shiny::textInput(paste0("factor.", i), label = i)
        })
      )
    })

    new.data <- shiny::reactive({
      shiny::req(input$new.format)
      switch(
        input$new.format,
        "As .csv" = utils::read.csv(input$new.data$datapath),
        "Input factors" =
          dplyr::bind_rows(sapply(factors(data.mini()),
                                  function(i) input[[paste0("factor.", i)]])) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(all(!grepl("\\D", .x)), as.numeric(.x), .x)))
      )
    })

    mini_u <- shiny::eventReactive(input$update, {
      shiny::req(input$data.mini, new.data())
      update.mini(data.mini(), new.data())
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
      print(update_plots())
    })

    output$update_download <- shiny::downloadHandler(
      filename = function() {
        paste0("update_minimise_", format(Sys.time(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(mini_u(), file, row.names = FALSE)
      }
    )

    output$update_downloadRData <- shiny::downloadHandler(
      filename = "update_minimise.rds",
      content = function(file) {
        df <- mini_u()
        saveRDS(df, file = file)
      }
    )


    output$update_report <- shiny::downloadHandler(
      filename = "update_report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(system.file("shiny", "multimini", "report.Rmd",
                              package = "multimini"),
                  tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(mini = mini_u(), plots = update_plots())

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

    session$onSessionEnded(shiny::stopApp)

    # Simulate -----------------------------------------------------------------

    output$factorInput <- shiny::renderUI({

      factors <- strsplit(input$simfactors, ", |,")[[1]]

      lapply(factors, function(f) {

        list(shiny::textInput(paste0("levels_", f), paste("Levels of", f)),
        shiny::textInput(paste0("props_", f), paste("Probabilities of", f)))

      })

    })

    output$simratioInput <- shiny::renderUI({
      lapply(2:input$groups, function(i) {
        shiny::numericInput(paste0("simratio", i), NULL, min = 1, value = 1,
                            step = 1)
      })
    })

    simfactors <- shiny::reactive({
      factors <- strsplit(input$simfactors, ", |,")[[1]]
      factors.l <- as.list(factors)
      names(factors.l) <- factors
      lapply(factors.l, function(f) {
        levels <- strsplit(input[[paste0("levels_", f)]], ", |,")[[1]]
        props <- strsplit(input[[paste0("props_", f)]], ", |,")[[1]]
        return(list(levels = levels, props = props))
      })
    })

    simburnin <- shiny::reactive({
      as.numeric(strsplit(input$simburnin, ", |,")[[1]])
    })

    simminprob <- shiny::reactive({
      as.numeric(strsplit(input$simminprob, ", |,")[[1]])
    })

    sims <- shiny::eventReactive(input$simulate, {

      pbo <- pbapply::pboptions(type = "none")
      on.exit(pbapply::pboptions(pbo), add = TRUE)

      ratio <- sapply(1:input$simgroups,
                      function(i) input[[paste0("simratio", i)]])

      simulate_mini(input$sampsize, simfactors(), Nsims = input$Nsims,
                    groups = input$simgroups, burnin = simburnin(),
                    minprob = simminprob(), ratio = ratio)
    })

    output$temp <- shiny::renderPrint({
      sims()
    })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server,
                  options = list(launch.browser = TRUE, display.mode = "normal"))


}
