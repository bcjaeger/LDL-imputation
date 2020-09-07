##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param filename
##'
##'
##'

exclude <- function(data, ...,
                    starter_label = 'Study participants'){

  .exclusions <- rlang::enexprs(...)

  .data_excluded <- data
  .n_included <- nrow(.data_excluded)
  .n_excluded <- nrow(.data_excluded) - .n_included

  .table <- tibble(label = as.character(starter_label),
                   n_included = as.integer(.n_included),
                   n_excluded = as.integer(.n_excluded))

  for(i in seq_along(.exclusions)){

    .data_excluded <- dplyr::filter(.data_excluded, !!.exclusions[[i]])

    .label <- names(.exclusions)[i]

    if(.label == '' | is.null(.label)) .label <- paste('Exclusion', i)

    .n_excluded <- .n_included - nrow(.data_excluded)
    .n_included <- nrow(.data_excluded)

    .table <- dplyr::add_row(.table,
                             label = .label,
                             n_included = .n_included,
                             n_excluded = .n_excluded)

  }

  list(data  = .data_excluded,
       table = .table)

}

