##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nhanes
##' @param miss_perc
make_analysis_data <- function(nhanes) {

  data_analysis <- nhanes[c("pop_one", "pop_two")] %>%
    map(
      ~ mutate(.x, ldl_to_impute = chol_ldl_mgdl_sampson) %>%
        rename(ldl_observed = chol_ldl_mgdl_sampson) %>%
        select(
          psu,
          strata,
          starts_with('wts'),
          starts_with('ldl'),
          starts_with('meds'),
          starts_with('bp'),
          triglycerides_mgdl,
          chol_hdl_mgdl,
          chol_total_mgdl,
          age,
          sex,
          race_ethnicity,
          hba1c_perc,
          egfr_ckdepi,
          diabetes,
          smk_current,
          ever_had_ascvd,
          ascvd_risk_pcr
        )
    )

  # harmonize names for weights

  data_analysis$pop_one %<>%
    rename(wts = wts_af_2yr) %>%
    select(-starts_with('wts_'))

  data_analysis$pop_two %<>%
    rename(wts = wts_mec_2yr) %>%
    select(-starts_with('wts_'))


  data_analysis


}
