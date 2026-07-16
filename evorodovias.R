library("dplyr")
library("ggplot2")
library("plotly")

# Top 10 de cada ano
top10 <- dados %>%
  group_by(ano, br) %>%
  summarise(
    acidentes = n(),
    .groups = "drop"
  ) %>%
  group_by(ano) %>%
  slice_max(
    acidentes,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

# Mantém todas as rodovias que apareceram em pelo menos um Top 10
heatmap <- top10 %>%
  complete(
    br,
    ano,
    fill = list(acidentes = 0)
  ) %>%
  pivot_wider(
    names_from = ano,
    values_from = acidentes,
    values_fill = 0
  )

# Matriz
matriz <- as.matrix(heatmap[, -1])
rownames(matriz) <- paste0("BR-", heatmap$br)

plot_ly(
  x = colnames(matriz),
  y = rownames(matriz),
  z = matriz,
  type = "heatmap",
  colorscale = list(
    c(0, 1),
    c("white", "blue")
  ),
  text = matriz,
  texttemplate = "%{text}",
  hovertemplate = paste(
    "Rodovia: %{y}<br>",
    "Ano: %{x}<br>",
    "Acidentes: %{z}<extra></extra>"
  )
) %>%
  layout(
    title = "As 10 Rodovias com Maior Número de Acidentes (2021–2025)",
    xaxis = list(title = "Ano"),
    yaxis = list(title = "Rodovia")
  )

top10
