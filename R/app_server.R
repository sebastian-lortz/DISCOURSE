#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  observeEvent(input$show_workflow, {
    updateNavbarPage(session, "main", selected = "High Level Workflow")
  })

  observeEvent(input$go_to_tab, {
    updateNavbarPage(session, "main", selected = input$go_to_tab)
  })

  session$userData$lm_data <- reactiveVal(NULL)
  session$userData$lme_data <- reactiveVal(NULL)

  mod_optim_vec_server("optim_vec", root_session = session)
  mod_optim_lm_server("optim_lm", root_session = session)
  mod_optim_lme_server("optim_lme", root_session = session)
  mod_optim_aov_server("optim_aov")

}
