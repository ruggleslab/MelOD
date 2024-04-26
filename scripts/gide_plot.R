library(plotly)

# Function to create a boxplot
create_boxplot <- function(data, x_var, y_var) {
  plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = 'box', color = ~get(x_var))
}

# Function to create a boxplot with all points
create_boxplot_with_points <- function(data, x_var, y_var) {
  plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = 'box', color = ~get(x_var), boxpoints = 'all')
}
