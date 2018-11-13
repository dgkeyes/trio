pre.surveys %>%
     select(training_year, training_type, training_id, how_long_have_you_managed_the_budget_s_for_a_federal_trio_program_at_your_current_institution_whole_number_only) %>%
     filter(training_id == "2018 In person") %>%
     set_names("training_year", 
               "training_type", 
               "training_id",
               "years_experience") %>%
     mutate(years_calculated = case_when(
          years_experience == "three years" ~ "3",
          years_experience == "6 months" ~ ".5",
          years_experience == "1 month" ~ ".08",
          years_experience == "7 days" ~ "0",
          years_experience == "less than 1 year" ~ "",
          years_experience == "1 year" ~ "1",
          TRUE ~ years_experience,
     ))
