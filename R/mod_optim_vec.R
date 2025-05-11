#' @import shiny
#' @importFrom shinyjs useShinyjs disable enable
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_table
NULL

mod_optim_vec_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # Wrap both panels in a no-wrap container
    div(style = "white-space: nowrap;",

        # LEFT PANEL: inline-block shrink‑wrap to table width
        div(style = "display:inline-block; vertical-align:top;",
            wellPanel(
              h4("Reported Summary Statistics"),
              numericInput(ns("N"),  name_with_info(
                "Sample Size",
                "The length of the target vectors.")
                , 864, min = 5, step = 1),
              p(name_with_info(
                "Descriptive Statistics",
                "The variables' name, mean, minimum, maximum, and if integer/continous")),
              div(style = "width:100%; overflow-x:auto;",
                  rHandsontableOutput(ns("param_table"), width = "100%")
              ),
              fluidRow(
                column(6, actionButton(ns("add_row"),    "Add row",    class = "btn-sm btn-block")),
                column(6, actionButton(ns("remove_row"), "Remove row", class = "btn-sm btn-block"))
              )
            ),
            wellPanel(
              h4("Algorithm Hyperparameters"),
              numericInput(
                ns("tolerance"),
                name_with_info(
                  "Tolerance",
                  "The threshold for the weighted objective function value below which the optimization will stop."),
                value = 1e-12,
                min   = 0,
                step  = 1e-12,
                width = "100%"
              ),
              numericInput(ns("max_iter"), name_with_info(
                "Max Iterations",
                "The maximum number of iterations the algorithm will run each time it restarts and for each variable."), 1e5,   min = 1,    step = 1000),
              numericInput(ns("init_temp"), name_with_info(
                "Initial Temperature",
                "The starting temperature for the simulated annealing, which sets the initial likelihood of accepting worse solutions in the first start."),
                1,     step = 0.01),
              numericInput(ns("cooling_rate"), name_with_info(
                "Cooling Rate",
                "The factor by which the temperature is multiplied after each iteration, governing how quickly the algorithm reduces its acceptance of worse solutions."),
                (1e5-10)/1e5, min = 0, max = 1, step = 0.0001),
              numericInput(ns("max_starts"), name_with_info(
                "Max Starts",
                "The maximum number of times the optimization algorithm will restart from the current best solution using reduced inital temperatures."),     3,     min = 1,    step = 1),
              checkboxInput(ns("parallel"), name_with_info(
                "Run in Parallel",
                "Enable the algorithm to execute on a parallel backend for improved performance."), TRUE),
              tags$hr(),
              h5(name_with_info(
                "Weights of Objective Function",
                "The  weights multiplied with each term in the objective function; adjusting these values can steer the optimization toward preferred trade‑offs and improve performance.")),
              div(style = "width:100%; overflow-x:auto;",
                  rHandsontableOutput(ns("weight_table"), width = "100%")
              ),
              actionButton(ns("estimate_weights"), name_with_info(
                "Estimate Weights",
                "Automatically compute the objective function weights from each term’s initial contribution, using their means and standard deviations."), class = "btn-info btn-block")
            )
        ), # end left panel

        # RIGHT PANEL: inline-block, sits next to left until wrapping necessary
        div(style = "display:inline-block; vertical-align:top; margin-left:20px; width: calc(100% - auto);",
            div(style = "margin-bottom:10px;",
                actionButton(ns("run"), name_with_info(
                  "Run Optimization",
                  "Executes DISCOURSE: Data-Simulation via iterative stochastic combinatorial optimization using reported summary estimates."), class = "btn-primary")
            ),

            h4("Optimization Output"),
            textOutput(ns("status_text")),

            h5(name_with_info(
                "Objective Function Value",
                "The minimum weighted value of the objective function attained by the optimization.")),
            tableOutput(ns("best_errors")),

            fluidRow(
              column(12,
                     actionButton(ns("show_error_traj"), name_with_info(
                       "Plot Errors",
                       "Displays the evolution of the objective function value across the iterations for the last restart."),  class = "btn-sm"),
                     actionButton(ns("show_rmse"), name_with_info(
                       "Get RMSE",
                       "Calculate the unweighted Root Mean Square Error between the reported summary statistics and those derived from the simulated data."),      class = "btn-sm"),
                     actionButton(ns("show_summary"), name_with_info(
                       "Plot Summary",
                       "Displays the (un)standardized differences between the reported summary statistics and those derived from the simulated data."), class = "btn-sm"),
                     actionButton(ns("show_cooling"), name_with_info("Plot Cooling",
                                                                     "Displays the temperature evolution of the simualted annealing across the iterations for the last restart."),   class = "btn-sm"),
                     actionButton(ns("display_data"),    name_with_info("Display data",
                                                                        "Displays the head of the simulated data frame."),      class = "btn-sm"),
                     actionButton(ns("download"), name_with_info("Download",
                                                                 "Downloads either the simulated data frame or the full discourse.object including all inputs/outputs."),          class = "btn-sm")
              )
            ),
            br(),
            div(style = "overflow:auto;", uiOutput(ns("main_output")))
        ) # end right panel

    ) # end no‑wrap container
  )
}

mod_optim_vec_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # INITIAL PARAMETERS
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
      status = "ready",  # "ready", "running", "done"
      last   = NULL
    )

    # PARAMETER TABLE
    output$param_table <- renderRHandsontable({
      tbl <- rhandsontable(rv$params, rowHeaders = NULL)
      hot_col(tbl, "Integer", type = "checkbox")
    })
    observeEvent(input$param_table, {
      rv$params <- hot_to_r(input$param_table)
    })

    # ADD / REMOVE ROWS
    observeEvent(input$add_row, {
      n <- nrow(rv$params) + 1
      rv$params <- rbind(rv$params, data.frame(
        Variable = paste0("V", n),
        Mean     = 4,
        SD       = 1,
        Min      = 0,
        Max      = 9,
        Integer  = TRUE,
        stringsAsFactors = FALSE
      ))
    })
    observeEvent(input$remove_row, {
      if (nrow(rv$params) > 1)
        rv$params <- rv$params[-nrow(rv$params), ]
    })

    # DYNAMIC COOLING RATE
    observeEvent(input$max_iter, {
      updateNumericInput(session, "cooling_rate",
                         value = (input$max_iter - 10) / input$max_iter
      )
    })

    # WEIGHTS TABLE
    weight_df <- reactiveVal(data.frame(
      Variable   = paste0("V", 1:5),
      WeightMean = rep(1.00, 5),
      WeightSD   = rep(1.00, 5),
      stringsAsFactors = FALSE
    ))
    observeEvent(rv$params, {
      df_p  <- rv$params
      old_w <- weight_df()                       # existing weights
      # build a new frame with exactly the same Variables
      new_w <- data.frame(Variable   = df_p$Variable,
                          stringsAsFactors = FALSE)
      # merge in any old weights; leave new ones as NA
      new_w <- merge(new_w, old_w, by = "Variable", all.x = TRUE)
      # fill any brand‑new rows with defaults
      new_w$WeightMean[is.na(new_w$WeightMean)] <- 1
      new_w$WeightSD  [is.na(new_w$WeightSD)]   <- 1
      weight_df(new_w)
    })
    output$weight_table <- renderRHandsontable({
      tbl <- rhandsontable(weight_df(), rowHeaders = NULL, width = "100%") %>%
        hot_table(stretchH = "all") %>%
        # set display names, but underlying keys stay the same
        hot_col("WeightMean",  title = "Weight of Means",  format = "0.000") %>%
        hot_col("WeightSD",    title = "Weight of SDs",    format = "0.000")
      tbl
    })
    observeEvent(input$estimate_weights, {
      df <- rv$params; req(df)
      w_list <- weights_vec(
        N           = input$N,
        target_mean = df$Mean,
        target_sd   = df$SD,
        range       = rbind(df$Min, df$Max),
        obj_weight  = c(1,1),
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

    # INITIAL BUTTON STATES
    shinyjs::disable("run")
    lapply(c("show_error_traj","show_rmse","show_summary","show_cooling","display_data","download"),
           shinyjs::disable
    )

    # ENABLE RUN WHEN PARAMS VALID
    observe({
      df <- rv$params
      ok <- all(
        nzchar(df$Variable),
        !is.na(df$Mean), !is.na(df$SD),
        !is.na(df$Min),  !is.na(df$Max)
      )
      if (ok) shinyjs::enable("run") else shinyjs::disable("run")
    })

    # RUN OPTIMIZATION
    observeEvent(input$run, {
      rv$status <- "running"
      shinyjs::disable("run")
      lapply(c("show_error_traj","show_rmse","show_summary","show_cooling","display_data","download"),
             shinyjs::disable
      )

      df  <- rv$params
      wdf <- hot_to_r(input$weight_table)
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

      rv$status <- "done"
      shinyjs::enable("run")
      lapply(c("show_error_traj","show_rmse","show_summary","show_cooling","display_data","download"),
             shinyjs::enable
      )
    })

    # STATUS TEXT
    output$status_text <- renderText({
      if (rv$status == "running") "Optimization is running..." else ""
    })

    # BEST‑ERRORS TABLE
    output$best_errors <- renderTable({
      vars <- rv$params$Variable
      if (rv$status != "done") {
        df <- as.data.frame(as.list(rep("hold", length(vars))), stringsAsFactors = FALSE)
        colnames(df) <- vars
        return(df)
      }
      bes  <- unlist(rv$result$best_error)
      disp <- ifelse(bes < input$tolerance, "converged", format(bes))
      df <- as.data.frame(as.list(disp), stringsAsFactors = FALSE)
      colnames(df) <- rv$params$Variable[seq_along(disp)]
      df
    }, rownames = FALSE)

    # ACTION BUTTONS
    observeEvent(input$show_error_traj, rv$last <- reactive("traj"))
    observeEvent(input$show_rmse,       rv$last <- reactive("rmse"))
    observeEvent(input$show_summary,    rv$last <- reactive("summary"))
    observeEvent(input$show_cooling,    rv$last <- reactive("cooling"))
    observeEvent(input$display_data,    rv$last <- reactive("data"))
    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Download",
        downloadButton(ns("dl_object"), "Full object"),
        downloadButton(ns("dl_data"),   "Data CSV"),
        easyClose = TRUE
      ))
    })

    # MAIN OUTPUT
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

    # RENDER HANDLERS
    output$error_plot   <- renderPlot({ plot_error(rv$result) })
    output$rmse_out     <- renderPrint({ get_rmse(rv$result) })
    output$summary_plot <- renderPlot({ plot_summary(rv$result) })
    output$cooling_plot <- renderPlot({ plot_cooling(rv$result) })
    output$data_preview <- renderTable({
      head(as.data.frame(rv$result$data), 10)
    }, rownames = TRUE)

    # DOWNLOAD HANDLERS
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
