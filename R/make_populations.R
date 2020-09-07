##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_nhanes_populations <- function(exams,
                                    filename) {

  nhanes_init <- read_csv(filename)

  nhanes_population_two <- nhanes_init %>%
    filter(exam %in% exams) %>%
    split(.$exam) %>%
    map(
      ~exclude(
        .x,
        `Non-zero survey weight` = wts_mec_2yr > 0,
        `Aged 18 and older` = age >= 18,
        `Complete Interview and exam` = exam_status == 'interview and exam'
      )
    ) %>%
    enframe(name = 'exam') %>%
    unnest_wider(col = value)

  nhanes_population_two_exclusions <- nhanes_population_two %>%
    select(exam, table) %>%
    unnest(cols = table)

  nhanes_population_two_data <- nhanes_population_two %>%
    select(data) %>%
    unnest(cols = data)

  nhanes_population_one <- nhanes_population_two_data %>%
    split(.$exam) %>%
    map(~exclude(
      .x,
      `Valid data on total and HDL cholesterol` =
        !is.na(chol_hdl_mgdl) & !is.na(chol_total_mgdl),
      `Assigned to morning session` = !is.na(wts_af_2yr),
      `Fasted 8 to 24 hours` = fasted_hours >= 8 & fasted_hours <= 24,
      `Valid data on triglycerides` = !is.na(triglycerides_mgdl)
    )) %>%
    enframe(name = 'exam') %>%
    unnest_wider(col = value)

  nhanes_population_one_exclusions <- nhanes_population_one %>%
    select(exam, table) %>%
    unnest(cols = table)

  nhanes_population_one_data <- nhanes_population_one %>%
    select(data) %>%
    unnest(cols = data)

  list(
    pop_one = nhanes_population_one_data,
    pop_two = nhanes_population_two_data,
    exclusions = bind_rows(
      nhanes_population_one_exclusions,
      nhanes_population_two_exclusions
    )
  )

}
