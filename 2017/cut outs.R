# geom_text(data = filter(knowledge.change.no.all, when == "Post"),
#           aes(label = rating),
#           nudge_x = .2) +
# geom_text(data = filter(knowledge.change.no.all, when == "Pre"),
#           aes(label = rating),
#           nudge_x = -.2) +

budget.decisions.level <- data.frame(table(pre.data[9]))
budget.decisions.level <- budget.decisions.level[-4,]

years.managing.budget <- summarize(pre.data, test = mean(as.numeric(years_managing_budget))) 

summary(pre.data$other_staff_members_in_dept)

#### Training ratings - overall ####

training.ratings.overall <- post.data[28:38]
training.ratings.overall <- summarize_all(training.ratings.overall, funs(mean(., na.rm = TRUE)))
training.ratings.overall$type_of_training <- "All"

training.ratings.overall.online <- filter(post.data[28:38], post.data$type_of_training == "Online")
training.ratings.overall.online <- summarize_all(training.ratings.overall.online, funs(mean(., na.rm = TRUE)))
training.ratings.overall.online$type_of_training <- "Online"

training.ratings.overall.f2f <- filter(post.data[28:38], post.data$type_of_training == "Face to face")
training.ratings.overall.f2f <- summarize_all(training.ratings.overall.f2f, funs(mean(., na.rm = TRUE)))
training.ratings.overall.f2f$type_of_training <- "Face to face"

training.ratings.overall<- bind_rows(training.ratings.overall, training.ratings.overall.online, training.ratings.overall.f2f)

training.ratings.overall %>%
     gather(type_of_training, dktest, value)


#

create.training.ratings  <- function (dataset, start.col, end.col) {
     
     # Create questions vector 
     question <- colnames(dataset[start.col:end.col])
     
     
     # Get averages for each question
     rating <- colMeans(dataset[start.col:end.col], na.rm = TRUE)
     rating <- round(rating, digits = 1)
     # print(rating)
     
     # Add type of training
     for (i in start.col:end.col) {
          training_type <- "All"
     }
     
     
     training.ratings.all <- data.frame(question, training_type, rating)
     print(training.ratings.all)
     
     dataset.online <- filter(dataset, type_of_training == "Online")
     
     # Create questions vector 
     
     question <- colnames(dataset.online[start.col:end.col])
     # print(questions)
     
     # Get averages for each question
     rating <- colMeans(dataset.online[start.col:end.col], na.rm = TRUE)
     rating <- round(rating, digits = 1)
     # print(rating)
     
     # Add type of training
     
     training_type <- "Online"
     
     
     
     training.ratings.online <- data.frame(question, training_type, rating)
     
     
     
     training.ratings <<- bind_rows(training.ratings.all, training.ratings.online)
     
}

create.training.ratings(post.data, 28, 38)






#### Create training ratings data frame ####

## All

# Create questions vector 
ratings_questions <- colnames(post.data[28:38])

# Add type of training
training_type <- "All"

# Get averages for each question
rating <- colMeans(post.data[28:38], na.rm = TRUE)
rating <- round(rating, digits = 1)

training.ratings.all <- data.frame(ratings_questions, training_type, rating)

## Online

# Add type of training
training_type <- "Online"

# Get averages for each question
rating <- colMeans(post.data.online[28:38], na.rm = TRUE)
rating <- round(rating, digits = 1)

training.ratings.online <- data.frame(ratings_questions, training_type, rating)


## Hybrid

# Add type of training
training_type <- "Hybrid"

# Get averages for each question
rating <- colMeans(post.data.hybrid[28:38], na.rm = TRUE)
rating <- round(rating, digits = 1)

training.ratings.hybrid <- data.frame(ratings_questions, training_type, rating)

## In person

# Add type of training
training_type <- "In person"

# Get averages for each question
rating <- colMeans(post.data.in.person[28:38], na.rm = TRUE)
rating <- round(rating, digits = 1)

training.ratings.in.person <- data.frame(ratings_questions, training_type, rating)

## Merge all

training.ratings <- bind_rows(training.ratings.all, training.ratings.online, training.ratings.hybrid, training.ratings.in.person)

rm(c(training.ratings.all, training.ratings.online, training.ratings.hybrid, training.ratings.in.person))



temp <- post.data %>%
     summarise_at(vars(post_rating_budget_controls:post_rating_eligible_expenses), mean, na.rm = TRUE) %>%
     summarise_all(round, digits = 2) %>%
     gather() %>%
     set_names(c("question", "rating"))
