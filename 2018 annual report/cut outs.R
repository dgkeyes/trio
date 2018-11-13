
<!-- `r fig_nums("course_ratings", "Overall Course Ratings")` -->
     ```{r, include = F, fig.height=9}

# Hiding this for now

ggplot(course.ratings, aes(x = training_id,
                           y = avg_rating,
                           fill = factor(training_year))) +
     geom_bar(stat = "identity", position = "dodge") +
     geom_text(aes(label = avg_rating), hjust = -.5) +
     coord_flip() +
     scale_y_continuous(limits = c(0, 4)) +
     mna.bar.chart.theme +
     theme(legend.position = "bottom",
           legend.title = element_blank()) +
     scale_fill_manual(values = c(mna.blue, mna.red)) +
     facet_wrap(~course, ncol = 2) +
     labs(caption = "n = 238")


```