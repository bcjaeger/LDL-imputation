##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nhanes

make_rand_forest <- function(nhanes) {

  data <- nhanes$pop_one



  rf_spec <- rand_forest(min_n = 3, trees = 1000) %>%
    set_engine('ranger', importance = 'permutation') %>%
    set_mode('regression')

  preproc <- recipe(chol_ldl_mgdl_sampson ~ .,
                    data = data) %>%
    step_rm(chol_ldl_mgdl, exam_status, fasted, seqn) %>%
    step_meanimpute(all_numeric()) %>%
    step_modeimpute(all_nominal()) %>%
    step_dummy(all_nominal(), one_hot = FALSE) %>%
    step_interact(terms = ~
                    chol_total_mgdl:age +
                    chol_total_mgdl:chol_hdl_mgdl +
                    chol_total_mgdl:starts_with('bp_') +
                    chol_total_mgdl:starts_with('meds'))

  rf_data <- juice(prep(preproc, data = data))

  rf_fit <- fit(rf_spec,
                formula = chol_ldl_mgdl_sampson ~ .,
                data = rf_data)

  rf_fit$fit$variable.importance %>%
    enframe() %>%
    arrange(desc(value)) %>%
    print(n=25)

  cbind(pred = rf_fit$fit$predictions,
        obsr = rf_data$chol_ldl_mgdl_sampson) %>%
    as_tibble()

}
