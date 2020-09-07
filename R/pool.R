##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mitml_list
.pool <- function(mitml_list){

  testEstimates(mitml_list) %>%
    getElement('estimates') %>%
    as_tibble() %>%
    clean_names()

}
