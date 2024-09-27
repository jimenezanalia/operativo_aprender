
###Para crear el Theme formato Paper

library(ggplot2)


# Crear un tema llamado paper_theme
paper_theme <- function() {
  theme_minimal(base_size = 12) +  # Tamaño base de 10 puntos
    theme(
      panel.background = element_rect(fill = "grey90", color = NA),  # Fondo gris sin recuadro
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white"),
      legend.position = "right",
      legend.title = element_text(size = 10),  # Leyenda tamaño 10
      legend.text = element_text(size = 9),     # Texto de leyenda tamaño 9
      axis.title = element_text(size = 12),     # Títulos de ejes tamaño 12
      axis.text = element_text(size = 10),       # Texto de ejes tamaño 10
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Título del gráfico
      plot.margin = margin(10, 10, 10, 10)       # Márgenes
    )
}

# Establecer el tema
theme_set(paper_theme())

# Colores de la paleta Set1
set1_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A65628", "#984EA3", "#999999")

# Ejemplo de gráfico
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(title = "Relación entre Peso y Millas por Galón",
       x = "Peso (1000 lbs)",
       y = "Millas por Galón") +
  scale_color_manual(values = set1_colors) +  # Usar colores de Set1
  guides(color = guide_legend(title = "Cilindrada"))
