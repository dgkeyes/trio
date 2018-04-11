Comparing current session to previous online one

```{r course_ratings_plot_online_comparison, include = TRUE}
p <- ggplot(filter(course.ratings, training_type == "Online"), 
            aes(x = course, 
                y = avg_rating,
                fill = factor(training_year),
                text = popup_text)) +
     geom_bar(stat = "identity",
              position = "dodge") +
     coord_flip() +
     scale_y_continuous(limits = c(0, 4)) +
     scale_fill_manual(values = c(mna.red, mna.blue)) +
     mna.bar.chart.theme

ggplotly(p, tooltip = "popup_text")

```


Comparing current session to all previous
```{r course_ratings_plot_all_comparison, include = TRUE, fig.height = 15}
p <- ggplot(course.ratings, aes(x = course, 
                                y = avg_rating,
                                text = popup_text)) +
     geom_bar(stat = "identity", fill = mna.blue) +
     # geom_text(aes(label = avg_rating),
     #           nudge_y = .25,
     #           color = mna.blue) +
     coord_flip() +
     facet_wrap(~training_year + ~training_type, ncol = 1) +
     scale_y_continuous(limits = c(0, 4)) +
     mna.bar.chart.theme

ggplotly(p, tooltip = "popup_text")
```
