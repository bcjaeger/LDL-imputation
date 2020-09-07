##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param analysis
##' @param miss_perc
sim_run <- function(analysis, miss_perc) {


  # ampute (create missing values for ldl)
  data_amputed <- analysis %>%
    map(~{
      out <- .x
      index <- sample(nrow(.x), round(nrow(.x) * miss_perc / 100))
      out$ldl_to_impute[index] <- NA_real_
      out
    })

  data_imputed <- expand.grid(
    use_survey = c(TRUE, FALSE),
    use_non_hdl = c(TRUE, FALSE),
    log_triglyceride = c(TRUE, FALSE)
  ) %>%
    as_tibble() %>%
    mutate(
      data_p1_obs = list(data_amputed$pop_one),
      data_p1_imp = pmap(
        .l = list(data_p1_obs, use_survey, use_non_hdl, log_triglyceride),
        .f = impute_jomo
      ),
      .before = 1
    )

  designs_imputed <- data_imputed %>%
    transmute(
      use_survey,
      use_non_hdl,
      log_triglyceride,
      design = map(
        .x = data_p1_imp,
        .f = ~ svydesign(ids = ~ psu, strata = ~strata,
                         weights = ~wts, nest = TRUE,
                         data = imputationList(as.list(.x)))
      )
    )


  ldl_mean <- designs_imputed %>%
    mutate(
      mean_ldl = map(
        .x = design,
        .f = ~ .pool(with(.x, svymean(~ldl_to_impute)))
      )
    ) %>%
    unnest(cols = mean_ldl)

  ldl_bias <- designs_imputed %>%
    mutate(
      design = map(design, subset, ldl_observed != ldl_to_impute),
      bias = map(
        .x = design,
        .f = ~ .pool(
          with(.x, svymean(~I(ldl_observed - ldl_to_impute)))
        )
      )
    ) %>%
    unnest(cols = bias)

  ldl_highdiff <- designs_imputed %>%
    mutate(
      design = map(design, subset, ldl_observed != ldl_to_impute),
      highdiff = map(
        .x = design,
        .f = ~ .pool(
          with(
            data = .x,
            expr = svymean(
              ~I(as.numeric(abs(ldl_observed - ldl_to_impute) > 20))
            )
          )
        )
      )
    ) %>%
    unnest(cols = highdiff)

  list(
    mean = ldl_mean,
    bias = ldl_bias,
    highdiff = ldl_highdiff
  ) %>%
    map_dfr(select, use_survey:std_error, -design, .id = 'measure')

  # just for inspection
  # ldl_scatter <- designs_imputed %>%
  #   mutate(
  #     design = map(design, subset, ldl_observed != ldl_to_impute),
  #     design = map(design, ~.x$designs[[1]]$variables)
  #   ) %>%
  #   unnest(cols = design) %>%
  #   ggplot(aes(x=ldl_observed, y=ldl_to_impute))+
  #   geom_point() +
  #   geom_abline(intercept = 0, slope = 1) +
  #   facet_grid(use_survey ~ log_triglyceride)

}


