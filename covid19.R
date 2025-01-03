#write a function that will run in the shiny app
calc_hosp = function(data){
  data %>%
    mutate(Percent_Hospitalized_With_Covid = (`Covid19 Hospital Admissions (per 100k)` / `Covid Cases (per 100k)`) * 100)
}
