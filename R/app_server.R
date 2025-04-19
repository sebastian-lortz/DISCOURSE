#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  ##––– navigation for the workflow button
  observeEvent(input$show_workflow, {
    updateNavbarPage(session, "main", selected = "High Level Workflow")
  })

  observeEvent(input$go_to_tab, {
    updateNavbarPage(session, "main", selected = input$go_to_tab)
  })
  ##––– render the DESCRIPTION info
  # in app_server.R
  output$pkg_desc <- renderUI({
    # 1) find DESCRIPTION: dev mode or installed
    desc_path <-
      if      (file.exists("DESCRIPTION"))        "DESCRIPTION"
    else if (file.exists("../DESCRIPTION"))     "../DESCRIPTION"
    else                                       system.file("DESCRIPTION", package = "discourse")

    if (!file.exists(desc_path)) {
      return(tags$em("DESCRIPTION file not found."))
    }

    # 2) read and show literally
    lines <- readLines(desc_path, warn = FALSE)
    tags$pre(paste(lines, collapse = "\n"))
  })


}
