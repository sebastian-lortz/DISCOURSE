#' @import shiny
#' @importFrom shinyjs useShinyjs disable enable
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_table
NULL

mod_optim_vec_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
    .rhandsontable .ht_master .wtHolder,
    .rhandsontable .ht_master .handsontable {
      overflow: visible !important;
    }
    .rhandsontable .ht_master .htDropdownMenu,
    .rhandsontable .ht_master .htContextMenu {
      z-index: 9999 !important;
    }
    .rhandsontable .ht_master .htDropdownMenu .dropdown-menu {
      max-height: none !important;
      white-space: normal !important;
    }
    .rhandsontable .ht_master .htDropdownMenu table {
      width: auto !important;
    }
    .selectize-dropdown,
    .selectize-dropdown-content {
      overflow: visible !important;
      z-index: 9999 !important;
      max-height: none !important;
      white-space: normal !important;
    }
    .selectize-dropdown,
    .selectize-dropdown-content {
      overflow: visible !important;
      max-height: none !important;
      white-space: normal !important;
      z-index: 9999 !important;
    }
  "))
    ),
    div(style = "white-space: nowrap;",
        div(
          style = "display:inline-block; vertical-align:top; margin-right:10px;",
          wellPanel(
              h4("Reported Summary Statistics"),
              numericInput(ns("N"),  name_with_info(
                "Sample Size",
                "The length of the target vectors.")
                , 864, min = 5, step = 1),
              selectInput(
                ns("mod"),
                label = name_with_info(
                  "Data Format",
                  "The module you would like to optimize the data for. Choose lme if you would like to enter repeated meaures."
                ),
                choices  = c("LM (long format)" = "lm", "LME (wide format)" = "lme"),
                selected = "LM (long format)",
                width    = "100%"
              ),
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
            )
          ),
            div(
              style = "display:inline-block; vertical-align:top; margin-right:20px;",
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
                1,  min = 0, max = 1,   step = 0.01),
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
                "The  weights multiplied with each term in the objective function; adjusting these values can steer the optimization toward preferred trade offs and improve performance.")),
              div(style = "width:100%; overflow-x:auto; margin-bottom:10px",
                  rHandsontableOutput(ns("weight_table"), width = "100%")
              ),
              actionButton(ns("estimate_weights"), name_with_info(
                "Estimate Weights",
                "Automatically compute the objective function weights from each term's initial contribution, using their means and standard deviations."), class = "btn-info btn-block")
            )
        ),
        div(style = "display:inline-block; vertical-align:top; margin-left:20px;",
            h4("Optimization Output"),
            fluidRow(
              column(
                width = 12,
                div(style = "display:inline-block;",
                    actionButton(ns("run"),       "Run Optimization", class = "btn-primary")
                ),
                div(style = "display:inline-block; margin-left:10px;",
                    actionButton(ns("proceed_lm"),  "Proceed with LM",   class = "btn-sm btn-info", style = "margin-left:10px;")
                ),
                div(style = "display:inline-block; margin-left:10px;",
                    actionButton(ns("proceed_lme"), "Proceed with LME",  class = "btn-sm btn-info", style   = "margin-left:10px;")
                )
              )
            ),
            div(
              id    = ns("processing_msg"),
              style = "display:none; margin:10px; font-weight:bold; color:#337ab7;",
              "Processing, please wait ..."
            ),
            textOutput(ns("status_text")),

            h5(name_with_info(
                "Objective Function Value",
                "The minimum weighted value of the objective function attained by the optimization.")),
            tableOutput(ns("best_errors")),
            fluidRow( style = "margin-bottom: 10px;",
              column(12,
                     actionButton(ns("plot_summary"),    name_with_info("Plot Summary","Plot summary differences"), class = "btn-sm"),
                     actionButton(ns("plot_error"),      name_with_info("Plot Errors","Show objective value trajectory"), class = "btn-sm"),
                     actionButton(ns("plot_cooling"),    name_with_info("Plot Cooling","Show temperature schedule"), class = "btn-sm"),
                     actionButton(ns("get_rmse"),        name_with_info("Get RMSE","Compute unweighted RMSE"), class = "btn-sm"),
                     actionButton(ns("display_data"),    name_with_info("Display Data","Show head of simulated data"), class = "btn-sm"),
                     actionButton(ns("download"),        name_with_info("Download","Download data or full object"), class = "btn-sm")
              )
            ),
            div(style = "overflow:visible; margin-top:20px;", uiOutput(ns("main_output"))),
        )

    )
  )
}

mod_optim_vec_server <- function(id, root_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      params = NULL,
      result = NULL,
      status = "ready",
      last   = NULL,
      dirty  = TRUE,
      for_lm = NULL,
      for_lme= NULL
    )

    observeEvent(input$mod, {
      if (input$mod == "lm") {
        rv$params <- data.frame(
          Variable = paste0("V", 1:5),
          Mean     = c(10.53, 0.27, 2.49, -0.64, 0.03),
          SD       = c( 7.82,  0.45,11.68, 1.85,  2.05),
          Min      = c( 0,     0,  -24,  -3,    -3),
          Max      = c(36,     1,   24,   3,     3),
          Integer  = rep(TRUE, 5),
          stringsAsFactors = FALSE
        )
      } else {
        rv$params <- data.frame(
          Variable = c("V1","V2","V3_0","V3_1","V3_2","V3_3",
                       "V4_0","V4_1","V4_2","V4_3"),
          Mean     = c(28.75,0.46,0.00,0.50,0.70,0.80,
                       240.57,261.07,270.02,274.57),
          SD       = c(11.98,0.50,0.00,0.00,0.00,0.00,
                       40.36, 33.26, 26.85, 26.33),
          Min      = c(14, 0, 0, 0.5, 0.7, 0.8, 95, 148, 184, 178),
          Max      = c(62, 1, 0, 0.5, 0.7, 0.8,298, 309, 304, 307),
          Integer  = c(rep(TRUE,2), rep(FALSE,4), rep(TRUE,4)),
          stringsAsFactors = FALSE
        )
      }
    }, ignoreNULL = FALSE)

    observeEvent({
      list(
      input$N,
      input$param_table,
      input$add_row,
      input$remove_row,
      input$mod,
      input$tolerance,
      input$max_iter,
      input$init_temp,
      input$cooling_rate,
      input$max_starts,
      input$parallel,
      input$weight_table,
      input$estimate_weights)
    }, {
      rv$dirty <- TRUE
      rv$last <- NULL
      for (btn in c(
        "plot_error","get_rmse",
        "plot_summary","plot_cooling",
        "display_data","download", "proceed_lm","proceed_lme"
      )) {
        shinyjs::disable(btn)
      }
    })

    output$param_table <- renderRHandsontable({
      tbl <- rhandsontable(rv$params, rowHeaders = NULL)
      hot_col(tbl, "Integer", type = "checkbox")
    })
    observeEvent(input$param_table, {
      rv$params <- hot_to_r(input$param_table)
    })

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

    observeEvent(input$max_iter, {
      updateNumericInput(session, "cooling_rate",
                         value = max(0, min(1, (input$max_iter - 10)/input$max_iter))
      )
    })

    weight_df <- reactiveVal(data.frame(
      Variable   = paste0("V", 1:5),
      WeightMean = rep(1.00, 5),
      WeightSD   = rep(1.00, 5),
      stringsAsFactors = FALSE
    ))

    observeEvent(rv$params, {
      df_p  <- rv$params
      old_w <- weight_df()
      new_w <- data.frame(Variable   = df_p$Variable,
                          stringsAsFactors = FALSE)
      new_w <- merge(new_w, old_w, by = "Variable", all.x = TRUE)
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

    shinyjs::disable("run")
    lapply(c("plot_error","get_rmse","plot_summary","plot_cooling","display_data","download","proceed_lm","proceed_lme"),
           shinyjs::disable
    )

    observe({
      df <- rv$params
      ok <- all(
        nzchar(df$Variable),
        !is.na(df$Mean), !is.na(df$SD),
        !is.na(df$Min),  !is.na(df$Max)
      )
      if (ok) shinyjs::enable("run") else shinyjs::disable("run")
    })

    observeEvent(input$run, {
      shinyjs::show("processing_msg")    # show the banner
      on.exit(shinyjs::hide("processing_msg"), add = TRUE)  # ensure it hides even on error
      rv$status <- "running"
      shinyjs::disable("run")
      lapply(c("N", "mod", "param_table", "add_row", "remove_row",
               "tolerance", "max_iter", "init_temp", "cooling_rate",
               "max_starts", "parallel", "weight_table", "estimate_weights",
                "plot_error","get_rmse","plot_summary","plot_cooling",
               "display_data","download", "proceed_lm","proceed_lme"),
             shinyjs::disable
      )
      df  <- rv$params
      wdf <- hot_to_r(input$weight_table)
      for (tbl in c("param_table","weight_table")) {
        shinyjs::runjs(
          sprintf('$("#%s .ht_master").css({"pointer-events":"none","opacity":0.5});',
                  ns(tbl))
        )
      }
      rv$result <- optim_vec(
        N            = input$N,
        target_mean  = stats::setNames(df$Mean, df$Variable),
        target_sd    = stats::setNames(df$SD,   df$Variable),
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
      rv$dirty <- FALSE
      shinyjs::enable("run")
      lapply(c("N", "mod", "param_table", "add_row", "remove_row",
               "tolerance", "max_iter", "init_temp", "cooling_rate",
               "max_starts", "parallel", "weight_table", "estimate_weights",
               "plot_error","get_rmse","plot_summary","plot_cooling",
               "display_data","download", "proceed_lm","proceed_lme"),
             shinyjs::enable
      )
      for (tbl in c("param_table","weight_table")) {
        shinyjs::runjs(
          sprintf('$("#%s .ht_master").css({"pointer-events":"auto","opacity":1});',
                  ns(tbl))
        )
      }
  })

    output$status_text <- renderText({
      if (rv$dirty) return(NULL)
      if (rv$status == "running") "Optimization is running..." else ""
    })

    output$best_errors <- renderTable({
      if (rv$dirty) return(NULL)
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

    observeEvent(input$plot_error, rv$last <- reactive("traj"))
    observeEvent(input$get_rmse,       rv$last <- reactive("rmse"))
    observeEvent(input$plot_summary,    rv$last <- reactive("summary"))
    observeEvent(input$plot_cooling,    rv$last <- reactive("cooling"))
    observeEvent(input$display_data,    rv$last <- reactive("data"))
    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Download",
        downloadButton(ns("dl_object"), "Full object"),
        downloadButton(ns("dl_data"),   "Data CSV"),
        easyClose = TRUE
      ))
    })

    observeEvent(input$proceed_lm, {
      req(!rv$dirty, rv$result)
      rv$for_lm <- rv$result$data
      root_session$userData$lm_data(rv$result$data)
      updateNavbarPage(
        root_session,
        inputId  = "main",
        selected = "Linear Regression"
      )
    })

    observeEvent(input$proceed_lme, {
      req(!rv$dirty, rv$result)
      rv$for_lme <- rv$result$data
      root_session$userData$lme_data(rv$result$data)
      updateNavbarPage(
        root_session,
        inputId  = "main",
        selected = "Mixed-Effect Linear Regression"
      )
    })

    output$main_output <- renderUI({
      if (rv$dirty) return(NULL)
      req(rv$last)
      switch(
        rv$last(),
        traj = tagList(
          plotOutput(ns("error_plot"), width = "600px", height = "400px"),
          fluidRow(
            { run_id <- isolate(input$run_select %||% rv$params$Variable[1]); idx <- which(rv$params$Variable == run_id) ; NULL},
            column(
              width = 3,
              selectizeInput(
                ns("run_select"),
                name_with_info("Variable", "Select which variable's (i.e. run's) error trajectory to plot."),
                choices = rv$params$Variable,
                selected = run_id,
                options = list(dropdownParent = 'body'),
                width = "100px"
              ),
            ),
            column(
              width = 3,
              numericInput(
                ns("iter_select"),
                name_with_info(
                  "Start Iteration",
                  "Plot errors beginning at this iteration."
                ),
                value = isolate(input$iter_select %||% 1),
                min   = 1,
                max   = length(
                  rv$result$track_error[[idx]]
                ),
                step  = 100,
                width = "100px"
              )
            )
          )
        ),
        rmse    = verbatimTextOutput(ns("rmse_out")),
        summary = tagList(
          plotOutput(ns("summary_plot"), width = "600px", height = "400px"),
          fluidRow(
            column(
              width = 6,
              checkboxInput(
                ns("std"),
                name_with_info(
                  "Standardized",
                  "Select whether the differences between simulated and target values are standardized."
                ),
                TRUE,
                width = "100%"
              ),
            ))),
        cooling = plotOutput(ns("cooling_plot"), width = "600px", height = "400px"),
        data    = tableOutput(ns("data_preview"))
      )
    })

    output$error_plot <- renderPlot({
      if (rv$dirty) return(NULL)
      req(input$run_select)
      plot_error(
        discourse_obj = rv$result,
        run           = which(rv$params$Variable == input$run_select),
        first_iter = as.integer(input$iter_select)
      )
    })
    output$rmse_out     <- renderPrint({
      if (rv$dirty) return(NULL)
      get_rmse(rv$result) })
    output$summary_plot <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_summary(rv$result,
                   standardised = input$std) })
    output$cooling_plot <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_cooling(rv$result) })
    output$data_preview <- renderTable({
      if (rv$dirty) return(NULL)
      utils::head(as.data.frame(rv$result$data), min(nrow(rv$result$data),15))
    }, rownames = TRUE)
    output$dl_object <- downloadHandler(
      filename = "discourse_object.rds",
      content  = function(file) {
        req(!rv$dirty)
        saveRDS(rv$result, file)
      }
    )
    output$dl_data <- downloadHandler(
      filename = "optimized_data.csv",
      content  = function(file) {
        req(!rv$dirty)
        utils::write.csv(as.data.frame(rv$result$data), file, row.names = FALSE)
      }
    )
  })
}
