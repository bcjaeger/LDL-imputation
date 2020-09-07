##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param use_survey
##' @param use_non_hdl
##' @param log_triglyceride
impute_jomo <- function(data,
                        use_survey,
                        use_non_hdl,
                        log_triglyceride) {

  # much much slower, but more accurate
  # formula_vars_lhs <- data %>%
  #   select_if(~any(is.na(.x))) %>%
  #   names()
  #
  # formula_vars_rhs <- data %>%
  #   select(-all_of(formula_vars_lhs),
  #          -ldl_observed,
  #          -strata,
  #          -psu,
  #          -wts) %>%
  #   names()

  # much faster
  formula_vars_lhs <- 'ldl_to_impute'

  formula_vars_rhs <- data %>%
    select_if(~!any(is.na(.x))) %>%
    select(-ldl_observed, -strata, -psu, -wts)

  formula_vars_lhs %<>% glue_collapse(sep = ' + ')
  formula_vars_rhs %<>% glue_collapse(sep = ' + ')

  formula <- glue("{formula_vars_lhs} ~ {formula_vars_rhs}")

  if(use_survey) formula <- glue("{formula} + wts + (1|strata)")

  if(use_non_hdl) data %<>%
    mutate(non_hdl = chol_total_mgdl - chol_hdl_mgdl)

  if(log_triglyceride) data$triglycerides_mgdl %<>% log()

  jomo_imp <- jomoImpute(
    mutate(data, across(where(is.character), as.factor)),
    formula = as.formula(formula),
    group = if(use_survey) 'psu' else NULL,
    n.burn = 500,
    n.iter = 100,
    m = 10
  )

  mitmlComplete(jomo_imp)

}
