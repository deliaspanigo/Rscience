library(plotly)

# Crear datos de ejemplo con un factor de varios niveles
set.seed(123)
boxplot_data <- data.frame(x = rep(1, 100), y = rnorm(100), grupo = factor(rep(letters[1:4], each = 25)))

# Datos de ejemplo para el violin plot
violinplot_data <- data.frame(x = rep(1, 100), y = rnorm(100), grupo = factor(rep(letters[1:4], each = 25)))

# Crear el objeto figura
fig <- plot_ly()

# Añadir el boxplot al objeto figura
fig <- fig %>%
  add_trace(
    data = boxplot_data,
    type = "box",
    boxpoints = FALSE,
    x = ~grupo,
    y = ~y,
    name = "Boxplot"
  )

# Añadir el violin plot al objeto figura
fig <- fig %>%
  add_trace(
    data = violinplot_data,
    type = "violin",
    points = "all",
    x = ~grupo,
    y = ~y,
    name = "Violinplot"
  )

# Añadir los puntos con jitter al violin plot
fig <- fig %>%
  add_markers(
    data = violinplot_data,
    x = ~grupo,
    y = ~y,
    type = "scatter",
    mode = "markers",
    name = "Puntos"#,
    #jitter = 0.4  # Ajusta el valor según lo deseado
  )

# Establecer el título y los ejes
fig <- fig %>%
  layout(
    title = "Boxplot, Violinplot y Puntos con Jitter para Múltiples Niveles",
    xaxis = list(title = "Grupo"),
    yaxis = list(title = "Valor Y")
  )

# Mostrar el gráfico
fig
