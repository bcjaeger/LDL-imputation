

the_plan <- drake_plan(

  nhanes = make_nhanes_populations(exams = c('2013-2014', '2015-2016'),
                                   filename = '../NHANES_derived.csv'),

  analysis = make_analysis_data(nhanes),

  rslts_50 = sim_run(analysis, miss_perc = 50)


)







