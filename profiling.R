library(profvis)
profvis({ runApp('./app.R') }  
        , prof_output = '../')


profvis(prof_input = '../file463c17c730d0.Rprof') 
p <- profvis(prof_input = '/path_to_save_output/file108f93bff877b.Rprof')
htmlwidgets::saveWidget(p, "/path_to_save_output/profile.html")






data <- read.csv("/Users/paul/Documents/pro/shiny-seq/data/single_cell/mayumi/TBPT/legend.csv")

ggplot(data, aes(x = v1, y = v2, fill = cMix)) +
  geom_tile(color = "white") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(x = NULL, y = NULL)



p <- ggplot(data, aes(x = v1, y = v2, fill = cMix)) +
  geom_tile(color = "white") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"  # Remove the legend
  ) +
  labs(x = NULL, y = NULL)

# Convert the ggplot2 plot to an interactive plotly plot
interactive_plot <- ggplotly(p)

# Display the interactive plot



interactive_plot <- plot_ly(
  data = data,
  x = ~v1,
  y = ~v2,
  z = ~cMix,
  type = "heatmap",
  colors = cMix,
  showscale = FALSE # Remove the legend
) %>%
  layout(
    xaxis = list(title = NULL),
    yaxis = list(title = NULL)
  )
