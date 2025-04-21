#’ @import shiny shinyjs
#’ @importFrom rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col
NULL

mod_optim_vec_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 4,
             wellPanel(
               h4("Reported Summary Statistics"),
               numericInput(ns("N"), "Sample Size:", value = 864, min = 5, step = 1),
               p("Enter one row per variable below:"),
               div(style = "width:100%; overflow-x:auto;",
                   rhandsontable::rHandsontableOutput(ns("param_table"), width = "100%")
               ),
               fluidRow(
                 column(6, actionButton(ns("add_row"),    "Add row",    class = "btn-sm btn-block")),
                 column(6, actionButton(ns("remove_row"), "Remove row", class = "btn-sm btn-block"))
               )
             ),
             wellPanel(
               h4("Algorithm Hyperparameters"),
               numericInput(ns("tolerance"),    "Tolerance:",      value = 0.001,       min = 0,    step = 1e-4),
               numericInput(ns("max_iter"),     "Max Iterations:", value = 1e5,         min = 1,    step = 1000),
               numericInput(ns("init_temp"),    "Initial Temp.:",  value = 1,           step = 0.1),
               numericInput(ns("cooling_rate"), "Cooling Rate:",   value = (1e5-10)/1e5,step = 0.01),
               numericInput(ns("max_starts"),   "Max Starts:",     value = 3,           min = 1,    step = 1),
               checkboxInput(ns("parallel"),    "Run in parallel?",value = FALSE),
               tags$hr(),
               h5("Objective Weights"),
               div(style = "width:100%; overflow-x:auto;",
                   rhandsontable::rHandsontableOutput(ns("weight_table"), width = "100%")
               ),
               actionButton(ns("estimate_weights"), "Estimate Weights", class = "btn-info btn-block")
             ),
             tags$hr(),
             actionButton(ns("run"), "Run Optimization", class = "btn-primary btn-block")
      ),

      column(width = 8,
             h4("Optimization Output"),
             tableOutput(ns("best_errors")),
             fluidRow(
               column(12,
                      actionButton(ns("show_error_traj"), "Error trajectory", class = "btn-sm"),
                      actionButton(ns("show_rmse"),       "Inspect RMSE",     class = "btn-sm"),
                      actionButton(ns("show_summary"),    "Inspect variables",class = "btn-sm"),
                      actionButton(ns("show_cooling"),    "Inspect cooling",  class = "btn-sm"),
                      actionButton(ns("display_data"),    "Display data",     class = "btn-sm"),
                      actionButton(ns("download"),        "Download",         class = "btn-sm")
               )
             ),
             br(),
             uiOutput(ns("main_output"))
      )
    )
  )
}

mod_optim_vec_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # default 5‑row parameter table
    initial_df <- data.frame(
      Variable = paste0("V", 1:5),
      Mean     = c(10.53, 0.27, 2.49, -0.64, 0.03),
      SD       = c(7.82,  0.45, 11.68, 1.85,  2.05),
      Min      = c(0,     0,   -24,   -3,    -3),
      Max      = c(36,    1,    24,    3,     3),
      Integer  = rep(TRUE, 5),
      stringsAsFactors = FALSE
    )
    rv <- reactiveValues(
      params = initial_df,
      result = NULL,
      last   = NULL,
      status = "hold"
    )

    # render & sync param table
    output$param_table <- rhandsontable::renderRHandsontable({
      tbl <- rhandsontable::rhandsontable(rv$params, rowHeaders = NULL)
      rhandsontable::hot_col(tbl, "Integer", type = "checkbox")
    })
    observeEvent(input$param_table, {
      req(input$param_table)
      rv$params <- rhandsontable::hot_to_r(input$param_table)
    })

    # add / remove rows
    observeEvent(input$add_row, {
      n <- nrow(rv$params) + 1
      rv$params <- rbind(rv$params, data.frame(
        Variable = paste0("V", n),
        Mean     = 0,
        SD       = 1,
        Min      = 0,
        Max      = 1,
        Integer  = FALSE,
        stringsAsFactors = FALSE
      ))
    })
    observeEvent(input$remove_row, {
      if (nrow(rv$params) > 1) {
        rv$params <- rv$params[-nrow(rv$params), , drop = FALSE]
      }
    })

    # dynamic cooling_rate default
    observeEvent(input$max_iter, {
      default_cool <- (input$max_iter - 10) / input$max_iter
      updateNumericInput(session, "cooling_rate", value = default_cool)
    })

    # placeholder weights table
    weight_df <- reactiveVal(data.frame(
      Variable   = paste0("V", 1:5),
      WeightMean = rep(1,5),
      WeightSD   = rep(1,5),
      stringsAsFactors = FALSE
    ))
    output$weight_table <- rhandsontable::renderRHandsontable({
      tbl <- rhandsontable::rhandsontable(weight_df(), rowHeaders = NULL)
      rhandsontable::hot_col(tbl, "WeightMean", format = "0.000")
      rhandsontable::hot_col(tbl, "WeightSD",   format = "0.000")
    })
    observeEvent(input$estimate_weights, {
      df <- rv$params; req(df)
      w_list <- weights_vec(
        N           = input$N,
        target_mean = df$Mean,
        target_sd   = df$SD,
        range       = rbind(df$Min, df$Max),
        init_distr  = "uniform", skew = 0, kurt = 1,
        obj_weight  = c(1,1), prior_weight = 0.5,
        integer     = df$Integer
      )
      w_mat <- do.call(rbind, w_list)
      weight_df(data.frame(
        Variable   = df$Variable,
        WeightMean = w_mat[,1],
        WeightSD   = w_mat[,2],
        stringsAsFactors = FALSE
      ))
    })

    # disable Run & action buttons until params are valid
    shinyjs::disable("run")
    lapply(c("show_error_traj","show_rmse","show_summary","show_cooling","display_data","download"),
           function(btn) shinyjs::disable(btn))

    observe({
      df <- rv$params
      ok <- all(
        nzchar(df$Variable),
        !is.na(df$Mean), !is.na(df$SD),
        !is.na(df$Min),  !is.na(df$Max)
      )
      if (ok) shinyjs::enable("run") else shinyjs::disable("run")
    })

    # run optimization
    observeEvent(input$run, {
      # indicate in-progress
      rv$status <- "in progress..."
      # disable run during execution
      shinyjs::disable("run")

      df  <- rv$params
      wdf <- rhandsontable::hot_to_r(input$weight_table)
      rv$result <- optim_vec(
        N            = input$N,
        target_mean  = setNames(df$Mean, df$Variable),
        target_sd    = setNames(df$SD,   df$Variable),
        range        = rbind(df$Min, df$Max),
        tolerance    = input$tolerance,
        integer      = df$Integer,
        max_iter     = input$max_iter,
        init_temp    = input$init_temp,
        cooling_rate = input$cooling_rate,
        max_starts   = input$max_starts,
        obj_weight   = lapply(seq_len(nrow(wdf)), function(i)
          c(wdf$WeightMean[i], wdf$WeightSD[i])),
        parallel     = input$parallel
      )
      # mark done
      rv$status <- "done"
      # enable action buttons
      lapply(c("show_error_traj","show_rmse","show_summary","show_cooling","display_data","download"),
             function(btn) shinyjs::enable(btn))
    })

    # best‑errors horizontal table, showing status until done
    output$best_errors <- renderTable({
      vars <- rv$params$Variable
      if (rv$status != "done") {
        df <- as.data.frame(as.list(rep(rv$status, length(vars))), stringsAsFactors = FALSE)
        colnames(df) <- vars
        return(df)
      }
      bes  <- unlist(rv$result$best_error)
      disp <- ifelse(bes < input$tolerance, "converged", format(bes))
      df <- as.data.frame(as.list(disp), stringsAsFactors = FALSE)
      colnames(df) <- names(bes)
      df
    }, rownames = FALSE)

    # track which button clicked
    observeEvent(input$show_error_traj,  rv$last <- reactive("traj"))
    observeEvent(input$show_rmse,        rv$last <- reactive("rmse"))
    observeEvent(input$show_summary,     rv$last <- reactive("summary"))
    observeEvent(input$show_cooling,     rv$last <- reactive("cooling"))
    observeEvent(input$display_data,     rv$last <- reactive("data"))
    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Download options",
        downloadButton(ns("dl_object"), "Download full object"),
        downloadButton(ns("dl_data"),   "Download data (CSV)"),
        easyClose = TRUE
      ))
    })

    # single output placeholder
    output$main_output <- renderUI({
      req(rv$last)
      switch(rv$last(),
             traj    = plotOutput(ns("error_plot"),   width = "100%", height = "400px"),
             rmse    = verbatimTextOutput(ns("rmse_out")),
             summary = plotOutput(ns("summary_plot"), width = "100%", height = "400px"),
             cooling = plotOutput(ns("cooling_plot"), width = "100%", height = "400px"),
             data    = tableOutput(ns("data_preview"))
      )
    })

    # plot/render handlers
    output$error_plot   <- renderPlot({ plot_error(rv$result) })
    output$rmse_out     <- renderPrint({ get_rmse(rv$result) })
    output$summary_plot <- renderPlot({ plot_summary(rv$result) })
    output$cooling_plot <- renderPlot({ plot_cooling(rv$result) })
    output$data_preview <- renderTable({
      head(as.data.frame(rv$result$data), 10)
    }, rownames = TRUE)

    # download handlers
    output$dl_object <- downloadHandler(
      filename = "discourse_object.rds",
      content  = function(file) saveRDS(rv$result, file)
    )
    output$dl_data <- downloadHandler(
      filename = "optimized_data.csv",
      content  = function(file) write.csv(as.data.frame(rv$result$data), file, row.names = FALSE)
    )
  })
}

## Use in your app:
# ui     <- fluidPage(mod_optim_vec_ui("optim_vec"))
# server <- function(input, output, session) mod_optim_vec_server("optim_vec")
