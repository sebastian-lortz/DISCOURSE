#' optim_vec UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optim_vec_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' optim_vec Server Functions
#'
#' @noRd 
mod_optim_vec_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_optim_vec_ui("optim_vec_1")
    
## To be copied in the server
# mod_optim_vec_server("optim_vec_1")
