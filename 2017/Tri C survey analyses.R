#### Packages ####

library("tidyverse")
library ("googlesheets")
library("ggmap")
library("stringr")
library("ggplot2")
library("data.table")
library("PBSmapping")
library("scales")

#### Get data ####

training.data.sheet <- gs_title("TRIO training data - merged - edited")

pre.data <- gs_read(training.data.sheet, ws = "Pre")
post.data <- gs_read(training.data.sheet, ws = "Post")

online.post.data <- filter(post.data, session == "March")
in.person.post.data <- filter(post.data, session == "August")
hybrid.post.data <- filter(post.data, session == "May")

online.pre.data <- filter(pre.data, session == "March")
in.person.pre.data <- filter(pre.data, session == "August")
hybrid.pre.data <- filter(pre.data, session == "May")


#### Themes ####

mna.red <- "#F05356"
mna.blue <- "#2BAADF"
mna.green <- "#BFD958"
mna.dark.gray <- "#545454"
mna.medium.gray <- "#a8a8a8"
mna.light.gray <- "#eeeeee"


mna.base.theme <- theme(
     panel.grid.minor = element_blank(),
     panel.background = element_rect(fill = "transparent",colour = NA),
     plot.background = element_rect(fill = "transparent",colour = NA),
     text = element_text(family="Arial", color = mna.dark.gray, size = 13),
     axis.text = element_text(family="Arial", color = mna.dark.gray),
     axis.title = element_blank(),
     axis.ticks = element_blank(),
     panel.grid.major = element_line(color = mna.light.gray)
)


mna.bar.chart.theme <- mna.base.theme + theme(
     axis.text.x = element_blank(),
     panel.grid.major.y = element_blank(),
     panel.grid.major.x = element_blank(),
     strip.background = element_blank(),
     strip.text = element_text(face = "bold"),
     legend.position = "none"
)

mna.bar.chart.theme.faceted <- mna.base.theme + theme(
     strip.background = element_blank(),
     strip.text = element_text(face = "bold"),
     legend.position = "none"
)

mna.column.chart.theme <- mna.base.theme + theme(
     axis.text.y = element_blank(),
     panel.grid.major.y = element_blank(),
     panel.grid.major.x = element_blank()
)

mna.map.theme <- mna.base.theme + theme(
     axis.line = element_blank(),
     axis.line.y = element_blank(),
     axis.title.y = element_blank(),
     axis.title.x = element_blank(),
     axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     axis.ticks = element_blank(),
     panel.grid = element_blank(),
     panel.border = element_blank(),
     panel.grid.major = element_blank()
)

#### Demographics ####

table(pre.data$attended_previous_trio_training)
table(pre.data$previous_training_year)

table(pre.data$main_budget_decision_maker)
decision.level <- data.frame(table(pre.data$decision_level))
colnames(decision.level) <- c("decision.level", "number")
decision.level$decision.level <- factor(decision.level$decision.level, levels = c("Not sure", "Low", "Medium", "High"))

ggplot(decision.level, aes(x = decision.level, y = number)) +
     geom_col(fill = mna.blue) +
     geom_text(label = decision.level$number, nudge_y = 4, color = mna.blue) +
     coord_flip() +
     mna.bar.chart.theme

ggsave("decision making level.png", height = 3)

mean(pre.data$trio_recipient_years, na.rm = TRUE)

mean(pre.data$years_managing_budget, na.rm = TRUE)
min(pre.data$years_managing_budget, na.rm = TRUE)

mean(pre.data$other_staff_members_in_dept, na.rm = TRUE)

#### Institution locations ####


institutions <- data.frame(post.data$institution)
colnames(institutions) <- "institution.name"
institutions <- filter(institutions, !is.na(institution.name))
temp <- geocode(as.character(institutions$institution.name))
institutions$lon <- temp$lon
institutions$latn <- temp$lat

us.map = map_data("state")
setnames(us.map, c("X","Y","PID","POS","region","subregion"))
us.map = clipPolys(us.map, xlim=xlim,ylim=ylim, keepExtra=TRUE)

ggplot() +
     geom_polygon(data = us.map,
                  aes(X,Y,group=PID),
                  fill="#ffffff", 
                  color = "#a9a9a9",
                  alpha=0.5) +
     geom_point(data = institutions,
                color = mna.blue,
                aes(x = lon, y = latn)) +
    coord_map() +
     mna.map.theme
     
     ggsave("institution maps.png")

#### Training ratings ####

## Online

training.rating.questions.online <- c(
     "COSO integrated internal control framework",
     "Uniform Guidance",
     "Educational Opportunity Center",
     "Educational Talent Search",
     "Student Support Services & McNair",
     "TRIO Budget I",
     "TRIO Budget II",
     "Upward Bound and Upward Bound Math and Science",
     "Veterans Upward Bound"
)

# Filter to March and May
training.ratings.data.online <- filter(post.data, session == "March" | session == "May")

# COSO
training.ratings.online <- colMeans(training.ratings.data.online[18], na.rm = TRUE)

# Uniform guidance
training.ratings.online <- c(training.ratings.online, colMeans(training.ratings.data.online[24], na.rm = TRUE))

# All others

# Narrow down dataset to just those in March
training.ratings.data.online <- filter(training.ratings.data.online, session == "March")
training.ratings.online <- c(training.ratings.online, colMeans(training.ratings.data.online[18:26], na.rm = TRUE))

# Remove duplicates
training.ratings.online <- training.ratings.online[-3]
training.ratings.online <- training.ratings.online[-7]

training.ratings.online <- data.frame(training.rating.questions.online, training.ratings.online) 
training.ratings.online$format <- "Online"
colnames(training.ratings.online) <- c("question", "rating", "format")


rm(training.ratings.data, training.ratings.data.online, training.rating.questions)

## In person

training.ratings.in.person.data <- filter(post.data, session == "May" | session == "August")

# Remove COSO and uniform guidance questions

training.ratings.in.person.data <- training.ratings.in.person.data[-18]
training.ratings.in.person.data <- training.ratings.in.person.data[-23]

# Do means for courses given in May and August in person
training.ratings.in.person <- colMeans(training.ratings.in.person.data[18:24], na.rm = TRUE)

# Reset dataset and do COSO and Uniform guidance for those who took it in person

training.ratings.in.person.data <- filter(post.data, session == "August")

# COSO 
training.ratings.in.person <- c(training.ratings.in.person, colMeans(training.ratings.in.person.data[18], na.rm = TRUE))

# Uniform guidance
training.ratings.in.person <- c(training.ratings.in.person, colMeans(training.ratings.in.person.data[24], na.rm = TRUE))

training.rating.questions.in.person <- c(
     "Educational Opportunity Center",
     "Educational Talent Search",
     "Student Support Services & McNair",
     "TRIO Budget I",
     "TRIO Budget II",
     "Upward Bound and Upward Bound Math and Science",
     "Veterans Upward Bound",
     "COSO integrated internal control framework",
     "Uniform Guidance"
)

training.ratings.in.person <- data.frame(training.rating.questions.in.person, training.ratings.in.person)
training.ratings.in.person$format <- "In person"
colnames(training.ratings.in.person) <- c("question", "rating", "format")

# Merge training rating dataframes

training.ratings <- bind_rows(training.ratings.in.person, training.ratings.online)
     
training.ratings$rating <- round(training.ratings$rating, digits = 1)

# Remove stuff

rm(training.ratings.in.person, training.ratings.in.person.data, training.ratings.online, training.rating.questions.in.person, training.rating.questions.online)

# Plot

ggplot(filter(training.ratings, format == "In person"), aes(x = question, y = rating, group = format, fill = factor(format))) +
     geom_col() +
     geom_text(color = "white",
               nudge_y = -.35,
               aes(label = rating)) +
     scale_fill_manual(values = c(mna.red)) +
     coord_flip() +
     # facet_wrap(~format) + 
     mna.bar.chart.theme

ggsave(file = "training ratings - in person.png")

ggplot(filter(training.ratings, format == "Online"), aes(x = question, y = rating, group = format, fill = factor(format))) +
     geom_col() +
     geom_text(color = "white",
               nudge_y = -.35,
               aes(label = rating)) +
     scale_fill_manual(values = c(mna.green)) +
     coord_flip() +
     # facet_wrap(~format) + 
     mna.bar.chart.theme

ggsave(file = "training ratings - online.png")

ggplot(training.ratings, aes(x = question, y = rating, group = format, fill = factor(format))) +
     geom_col() +
     geom_text(color = "white",
               nudge_y = -.35,
               aes(label = rating)) +
     scale_fill_manual(values = c(mna.red, mna.green)) +
     scale_x_discrete(labels = function(rating) str_wrap(rating, width = 30)) +
     coord_flip() +
     facet_wrap(~format) + 
     mna.bar.chart.theme

ggsave(file = "training ratings - both.png")


#### Knowledge change ####

knowledge.change.questions <- c(
     "Managing budget through internal controls and financial management",
     "Relevance of OMB circulars",
     "Checks and balances for eligible expenses",
     "Authorization control",
     "Application of governing rules of TRIO program components and TRIO as a whole",
     "Legislative history of TRIO",
     "Student/participant eligibility",
     "Difference between allowable and non-allowable costs",
     "Right and responsibilities of institutions that agree to sponsor TRIO grants",
     "Legislation, regulation, and budget specific to each TRIO program"
)



# All
knowledge.change.ratings.post.all <- colMeans(post.data[6:15], na.rm = TRUE)
knowledge.change.ratings.post.all <- data.frame(knowledge.change.ratings.post.all)
knowledge.change.ratings.post.all$format <- "All"
knowledge.change.ratings.post.all$when <- "Post"
colnames(knowledge.change.ratings.post.all) <- c("rating", "format", "when") 

knowledge.change.ratings.pre.all <- colMeans(pre.data[13:22], na.rm = TRUE)
knowledge.change.ratings.pre.all <- data.frame(knowledge.change.ratings.pre.all)
knowledge.change.ratings.pre.all$format <- "All"
knowledge.change.ratings.pre.all$when <- "Pre"
colnames(knowledge.change.ratings.pre.all) <- c("rating", "format", "when") 

# In person
knowledge.change.ratings.post.in.person <- colMeans(in.person.post.data[6:15], na.rm = TRUE)
knowledge.change.ratings.post.in.person <- data.frame(knowledge.change.ratings.post.in.person)
knowledge.change.ratings.post.in.person$format <- "In person"
knowledge.change.ratings.post.in.person$when <- "Post"
colnames(knowledge.change.ratings.post.in.person) <- c("rating", "format", "when") 

knowledge.change.ratings.pre.in.person <- colMeans(in.person.pre.data[13:22], na.rm = TRUE)
knowledge.change.ratings.pre.in.person <- data.frame(knowledge.change.ratings.pre.in.person)
knowledge.change.ratings.pre.in.person$format <- "In person"
knowledge.change.ratings.pre.in.person$when <- "Pre"
colnames(knowledge.change.ratings.pre.in.person) <- c("rating", "format", "when") 

# Online

knowledge.change.ratings.post.online <- colMeans(online.post.data[6:15], na.rm = TRUE)
knowledge.change.ratings.post.online <- data.frame(knowledge.change.ratings.post.online)
knowledge.change.ratings.post.online$format <- "Online"
knowledge.change.ratings.post.online$when <- "Post"
colnames(knowledge.change.ratings.post.online) <- c("rating", "format", "when") 

knowledge.change.ratings.pre.online <- colMeans(online.pre.data[13:22], na.rm = TRUE)
knowledge.change.ratings.pre.online <- data.frame(knowledge.change.ratings.pre.online)
knowledge.change.ratings.pre.online$format <- "Online"
knowledge.change.ratings.pre.online$when <- "Pre"
colnames(knowledge.change.ratings.pre.online) <- c("rating", "format", "when") 

# Hybrid

knowledge.change.ratings.post.hybrid <- colMeans(hybrid.post.data[6:15], na.rm = TRUE)
knowledge.change.ratings.post.hybrid <- data.frame(knowledge.change.ratings.post.hybrid)
knowledge.change.ratings.post.hybrid$format <- "Hybrid"
knowledge.change.ratings.post.hybrid$when <- "Post"
colnames(knowledge.change.ratings.post.hybrid) <- c("rating", "format", "when") 

knowledge.change.ratings.pre.hybrid <- colMeans(hybrid.pre.data[13:22], na.rm = TRUE)
knowledge.change.ratings.pre.hybrid <- data.frame(knowledge.change.ratings.pre.hybrid)
knowledge.change.ratings.pre.hybrid$format <- "Hybrid"
knowledge.change.ratings.pre.hybrid$when <- "Pre"
colnames(knowledge.change.ratings.pre.hybrid) <- c("rating", "format", "when") 

# Merge 

knowledge.change <- bind_rows(knowledge.change.ratings.post.all,
                              knowledge.change.ratings.pre.all,
                              knowledge.change.ratings.post.in.person,
                              knowledge.change.ratings.pre.in.person,
                              knowledge.change.ratings.post.hybrid,
                              knowledge.change.ratings.pre.hybrid,
                              knowledge.change.ratings.post.online,
                              knowledge.change.ratings.pre.online)

knowledge.change$rating <- round(knowledge.change$rating, digits = 1)
knowledge.change$question <- knowledge.change.questions


# Get rid of stuff
rm(knowledge.change.ratings.post.all,
   knowledge.change.ratings.pre.all,
   knowledge.change.ratings.post.in.person,
   knowledge.change.ratings.pre.in.person,
   knowledge.change.ratings.post.hybrid,
   knowledge.change.ratings.pre.hybrid,
   knowledge.change.ratings.post.online,
   knowledge.change.ratings.pre.online)

# Plot

knowledge.change$position <- ifelse(knowledge.change$when == "Pre", "1", "2")
knowledge.change.no.all <- filter(knowledge.change, format != "All")

ggplot(knowledge.change.no.all, 
     aes(x = position, y = rating, group = format, 
           color = factor(format))) + 
     geom_line() +
     scale_y_continuous(limits = c(1, 4), 
                        breaks = c(1.5, 2, 2.5, 3, 3.5)) +
     scale_color_manual(values = c(mna.blue, mna.red, mna.green, mna.dark.gray)) +
     guides(fill = guide_legend(title = NULL)) +
     scale_x_discrete(labels = c("Pre", "Post"), 
                      limits = c(1, 2)) +
     facet_wrap(~question,
                ncol = 5,
                labeller = labeller(question = label_wrap_gen(width = 20))) +
     labs(color = "Training Format") + 
     mna.bar.chart.theme.faceted

ggsave(file = "knowledge change.png", height = 7.5, width = 10, units = "in")


     
     
     

write.csv(file = "knowledge.change.csv", knowledge.change)


#### Follow up and recommend ####

followup.session <- post.data %>%
     group_by(type_of_training) %>%
     count(followup_session) %>%
     filter(!is.na(followup_session)) %>%
     set_names(c("training.type", "response", "number")) %>%
     mutate(pct = round(number / sum(number), digits = 2))

followup.session$response <- factor(followup.session$response, levels = c("No", "Maybe", "Yes"))

ggplot(followup.session, aes(x = response, y = pct, group = training.type, color = factor(training.type), fill = factor(training.type))) +
     geom_col() +
     geom_text(label = percent(followup.session$pct), nudge_y = .06) +
     facet_wrap(~training.type, ncol = 1) +
     coord_flip() +
     scale_y_continuous(labels = percent) +
     scale_color_manual(values = c(mna.blue, mna.red, mna.green, mna.dark.gray)) +
     scale_fill_manual(values = c(mna.blue, mna.red, mna.green, mna.dark.gray)) +
     mna.bar.chart.theme

ggsave("followup session.png")

# Recommend

recommend.to.colleagues <- post.data %>%
     group_by(type_of_training) %>%
     count(recommend_training) %>%
     filter(!is.na(recommend_training)) %>%
     set_names(c("training.type", "response", "number")) %>%
     mutate(pct = round(number / sum(number), digits = 2))
    
recommend.to.colleagues$response <- factor(recommend.to.colleagues$response, levels = c("No", "Maybe", "Yes"))

ggplot(recommend.to.colleagues, aes(x = response, y = pct, group = training.type, color = factor(training.type), fill = factor(training.type))) +
     geom_col() +
     geom_text(label = percent(recommend.to.colleagues$pct), nudge_y = .06) +
     facet_wrap(~training.type, ncol = 1) +
     coord_flip() +
     scale_y_continuous(labels = percent) +
     scale_color_manual(values = c(mna.blue, mna.red, mna.green, mna.dark.gray)) +
     scale_fill_manual(values = c(mna.blue, mna.red, mna.green, mna.dark.gray)) +
     mna.bar.chart.theme 

ggsave("recommend to colleagues.png")
