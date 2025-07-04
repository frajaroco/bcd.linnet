library(ggplot2)
library(dplyr)

plot.boxcounTest <- function(B) {

  # Crear un data frame para ggplot
  data <- data.frame(B$B_sim)
  
  # Crear el gráfico
    ggplot(data, aes(x = B$B_sim)) +
    geom_histogram(aes(y = after_stat(density)), color = "gray", bins = 30) +
    geom_density(color = "red", linewidth = 1) +
    labs(x = expression(B), y = "Density") +
    geom_vline(xintercept = B$Q1, linetype = "dashed", color = "blue") +
    geom_vline(xintercept = B$Q3, linetype = "dashed", color = "blue") +
    geom_vline(xintercept = B$B_calc, linetype = "dashed", color = "red")
}



