
# Packages ----------------------------------------------------------------

library(googlesheets)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

# Pre ---------------------------------------------------------------------

pre.surveys.2018 <- gs_title("2018 TRIO Training Pre-Survey (Responses)")
pre.surveys.2018 <- gs_read(pre.surveys.2018) %>%
     clean_names() %>%
     mutate(if_no_how_many_other_staff_members_work_in_your_department_numerical_value_only = as.character(if_no_how_many_other_staff_members_work_in_your_department_numerical_value_only))


pre.surveys.2017.sheet <- gs_title("TRIO training data 2017 - merged")
pre.surveys.2017 <- gs_read(pre.surveys.2017.sheet, ws = "Pre") %>%
     clean_names() %>%
     mutate(if_no_how_many_other_staff_members_work_in_your_department_numerical_value_only = as.character(if_no_how_many_other_staff_members_work_in_your_department_numerical_value_only))

pre.surveys <- bind_rows(pre.surveys.2018, pre.surveys.2017)

pre.surveys %>% 
     select(how_long_has_your_institution_been_a_recipient_of_a_federal_trio_program_round_to_the_nearest_whole_number_of_years) %>% 
     unique()


# Post --------------------------------------------------------------------



post.surveys.2017.march <- gs_url("https://docs.google.com/spreadsheets/d/1pFKHHCSabHYB0AEsB6_Jf9Qj97gnatGGRfn2j1jW9KU/edit#gid=133904600")

post.surveys.2017.march <- gs_read(post.surveys.2017.march) %>%
     clean_names()

post.surveys.2017.may <- gs_url("https://docs.google.com/spreadsheets/d/18csE-KoqaZlKZRfmT3S26zJXmUa-gkeOwF7S9doOC3o/edit#gid=787154103")

post.surveys.2017.may <- gs_read(post.surveys.2017.may) %>%
     clean_names()

post.surveys.2017.august <- gs_url("https://docs.google.com/spreadsheets/d/13HStU4A4G1i5yG8uAIDNgWnQkMxcLL2r3FOpwJEfKXI/edit#gid=1665067623")

post.surveys.2017.august <- gs_read(post.surveys.2017.august) %>%
     clean_names()

post.surveys.2017.sheet <- gs_title("TRIO training data 2017 - merged")

post.surveys.2017 <- gs_read(post.surveys.2017.sheet, ws = "Post") %>%
     clean_names() 


post.surveys.2018 <- gs_title("2018 TRIO Training Post-Survey (Responses)")

post.surveys.2018 <- gs_read(post.surveys.2018) %>%
     clean_names()

post.surveys <- bind_rows(post.surveys.2018,
                          post.surveys.2017.march,
                          post.surveys.2017.may,
                          post.surveys.2017.august)


# Calculate months --------------------------------------------------------

session.order <- c("Online (March 2017)",
                   "Hybrid (May 2017)",
                   "In person (July 2017)",
                   "Online (March 2018)",
                   "Hybrid (May 2018)",
                   "In person (June 2018)",
                   "In person (July 2018)",
                   "In person (August 2018)")

calculate_date_and_training_type <- function(dataset) {
     dataset <- dataset %>%
          mutate(training_date = mdy_hms(timestamp)) %>%
          mutate(training_month = month(training_date)) %>%
          mutate(training_day = day(training_date)) %>%
          mutate(training_year = year(training_date)) %>%
          mutate(training_month = case_when(
               (training_day == 30 | training_day == 31) & training_year == 2018 ~ 8,
               TRUE ~ training_month
          )) %>% 
          mutate(training_month_text = month.name[training_month]) %>% 
          mutate(training_type = case_when(
               month(training_date) == 3 | month(training_date) == 4 ~ "Online",
               month(training_date) == 5 ~ "Hybrid",
               TRUE ~ "In person"
          )) %>%
          mutate(training_id = paste0(training_type,
                                     " (",
                                     training_month_text, 
                                     " ",
                                     training_year,
                                     ")")) %>% 
          mutate(training_id = factor(training_id, levels = session.order))
          
}

pre.surveys <- calculate_date_and_training_type(pre.surveys)

post.surveys <- post.surveys %>% 
     mutate(training_date = mdy_hms(timestamp)) %>%
     mutate(training_month = month(training_date)) %>%
     mutate(training_day = day(training_date)) %>%
     mutate(training_year = year(training_date)) %>%
     mutate(training_month_text = month.name[training_month]) %>% 
     mutate(training_type = case_when(
          month(training_date) == 3 | month(training_date) == 4 ~ "Online",
          month(training_date) == 5 ~ "Hybrid",
          TRUE ~ "In person"
     )) %>%
     mutate(training_id = case_when(
          training_year == 2017 & training_type == "Online" ~ "Online (March 2017)",
          training_year == 2017 & training_type == "Hybrid" ~ "Hybrid (May 2017)",
          training_year == 2017 & training_type == "In person" ~ "In person (July 2017)",
          training_year == 2018 & training_type == "Online" ~ "Online (March 2018)",
          training_year == 2018 & training_type == "Hybrid" ~ "Hybrid (May 2018)",
          training_year == 2018 & 
               training_type == "In person" & 
               training_month_text == "June" ~ "In person (June 2018)",
          training_year == 2018 & 
               training_type == "In person" & 
               training_month_text == "July" ~ "In person (July 2018)",
          training_year == 2018 & 
               training_type == "In person" & 
               training_month_text == "August" ~ "In person (August 2018)"
     ))
     



# Knowledge growth --------------------------------------------------------


knowledge.post <- post.surveys %>%
     select(contains("how_would_you_rate_your_current_level_of_knowledge"), training_type, training_year, training_id) %>%
     gather(key = "item", value = "rating", -training_year, -training_type, -training_id) %>%
     mutate(rating = parse_number(rating)) %>%
     mutate(item = str_replace(item, "how_would_you_rate_your_current_level_of_knowledge_about_the_following_aspects_of_the_training_you_just_completed_", "")) %>%
     mutate(item = str_replace_all(item, "_", " ")) %>%
     mutate(item = str_to_title(item)) %>%
     mutate(timing = "Post")

knowledge.pre <- pre.surveys %>%
     select(contains("currently_how_would_you_rate_your_levels_of_knowledge_and_understanding"), training_type, training_year, training_id) %>%
     gather(key = "item", value = "rating", -training_year, -training_type, -training_id) %>%
     mutate(rating = parse_number(rating)) %>%
     mutate(item = str_replace(item, "currently_how_would_you_rate_your_levels_of_knowledge_and_understanding_of_the_following_aspects_of_the_training_you_are_about_to_receive_", "")) %>%
     mutate(item = str_replace_all(item, "_", " ")) %>%
     mutate(item = str_to_title(item)) %>%
     mutate(timing = "Pre")

knowledge.growth <- bind_rows(knowledge.pre, knowledge.post) %>%
     mutate(item = str_replace(item, "Omb", "OMB")) %>%
     mutate(item = str_replace(item, "Trio", "TRIO")) %>%
     mutate(item = str_replace(item, "Asa", "As a")) %>%
     mutate(item = str_replace(item, "Difference S", "Differences")) %>%
     mutate(item = str_replace(item, "It C", "IT C")) %>%
     group_by(training_type, training_year, training_id, timing, item) %>%
     summarize(avg_rating = round(mean(rating, na.rm = TRUE), 1)) %>%
     ungroup() %>%
     # mutate(item = str_wrap(item, width = 40)) %>%
     mutate(item = str_replace(item, "Doing Budget Management Through Internal Controls And Financial Management", "Managing Budget Through Internal Controls And Financial Management")) %>%
     mutate(item = str_replace(item, "Application Of Governing Rules Of TRIO Program Components And Trio As A Whole", "Application Of Governing Rules Of TRIO Program Components And Trio As a Whole")) %>%
     mutate(item = str_replace(item, "Right And Responsibilities Of Institutions That Agree To Sponsor TRIO Grants" , "Rights And Responsibilities Of Institutions That Agree To Sponsor TRIO Grants")) %>%
     mutate(item = str_replace(item, "Difference Between Allowable And Non Allowable Costs" , "Differences Between Allowable And Non Allowable Costs")) %>%
     mutate(timing = factor(timing, levels = c("Pre", "Post"))) %>%
     mutate(popup_text = paste(timing, 
                               ": ",
                               avg_rating,
                               sep = "")) 

rm(knowledge.pre, knowledge.post)


# Institutions ------------------------------------------------------------


# gs_title("TRIO participants") %>% 
#      gs_download(to = "data/institutions.xlsx")

institutions <- read_excel("data/institutions.xlsx") %>% 
     select(training, Longitude, Latitude) %>% 
     set_names(c("training", "lon", "lat")) %>% 
     mutate(lon = as.numeric(lon)) %>% 
     mutate(lat = as.numeric(lat)) %>% 
     mutate(training = factor(training, levels = c("2018 Online",
                                                   "2018 Hybrid",
                                                   "2018 In Person (June)",
                                                   "2018 In Person (July)",
                                                   "2018 In Person (August)")))

session.order
