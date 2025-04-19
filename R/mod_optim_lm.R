#' optim_lm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optim_lm_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' optim_lm Server Functions
#'
#' @noRd 
mod_optim_lm_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_optim_lm_ui("optim_lm_1")
    
## To be copied in the server
# mod_optim_lm_server("optim_lm_1")
