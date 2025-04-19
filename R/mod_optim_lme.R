#' optim_lme UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optim_lme_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' optim_lme Server Functions
#'
#' @noRd 
mod_optim_lme_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_optim_lme_ui("optim_lme_1")
    
## To be copied in the server
# mod_optim_lme_server("optim_lme_1")
