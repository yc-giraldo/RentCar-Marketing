# CASO 3 - RENTCAR MARKETING
# PUNTO 1. RECONOCIMIENTO DE LA BASE

library(tidyverse)

# Importar base original
datos <- read_csv(
  "Base_datos_caso_3.csv",
  show_col_types = FALSE
)
# Revisar dimensiones
dim(datos)
glimpse(datos)


# PUNTO 1 - REVISION DE CALIDAD DE DATOS

# 1. Revisar valores faltantes
datos |>
  summarise(
    across(everything(), ~sum(is.na(.)))
  ) |>
  print(width = Inf)

# 2. Revisar filas duplicadas
sum(duplicated(datos))

# 3. Revisar fechas duplicadas
sum(duplicated(datos$Week))

# 4. Revisar periodo de observacion
range(datos$Week)

# 5. Estadisticas de ventas y ordenes
datos |>
  summarise(
    n = n(),
    media_sales = mean(Sales, na.rm = TRUE),
    min_sales = min(Sales, na.rm = TRUE),
    max_sales = max(Sales, na.rm = TRUE),
    media_orders = mean(Orders, na.rm = TRUE),
    min_orders = min(Orders, na.rm = TRUE),
    max_orders = max(Orders, na.rm = TRUE)
  ) |>
  print()


# Grafica de ventas semanales
grafica_ventas <- datos |>
  ggplot(aes(x = Week, y = Sales)) +
  geom_line(color = "#2E3B5F", linewidth = 0.8) +
  labs(
    title = "Evolucion semanal de las ventas",
    x = "Semana",
    y = "Ventas"
  ) +
  theme_minimal()
#Mostrar grafica
print(grafica_ventas)

# Guardar grafica como imagen
ggsave(
  "Grafica_Ventas.png",
  plot = grafica_ventas,
  width = 10,
  height = 6,
  dpi = 300
)


# =====================================
# TABLA DE PUBLICIDAD COMO IMAGEN PNG
# =====================================

library(gridExtra)
library(grid)

# Preparar tabla
tabla_imagen <- rangos_publicidad |>
  mutate(
    medida = case_when(
      grepl("_minimo$", variable) ~ "Minimo",
      grepl("_maximo$", variable) ~ "Maximo",
      grepl("_negativos$", variable) ~ "Negativos"
    ),
    variable = sub(
      "_(minimo|maximo|negativos)$",
      "",
      variable
    )
  ) |>
  pivot_wider(
    names_from = medida,
    values_from = resultado
  )
# Crear la tabla grafica
grafica_tabla <- gridExtra::tableGrob(
  tabla_imagen,
  rows = NULL
)
# Guardar como PNG
png(
  "Tabla_Publicidad.png",
  width = 2800,
  height = 2400,
  res = 180
)
grid::grid.newpage()
grid::grid.draw(grafica_tabla)
dev.off()


# =====================================
# PUNTO 2 - AGREGACION DE AMAZON
# =====================================

# Crear seis variables agregadas
datos_amazon <- datos |>
  mutate(
    Amazon_DSP_Cost =
      rowSums(pick(matches("^DSP.*\\.Cost$"))),

    Amazon_DSP_Impressions =
      rowSums(pick(matches("^DSP.*\\.Impression$"))),

    Amazon_Search_Cost =
      rowSums(pick(matches("^Search.*\\.Cost$"))),

    Amazon_Search_Impressions =
      rowSums(pick(matches("^Search.*\\.Impression$"))),

    Amazon_Sponsored_Cost =
      rowSums(pick(matches("^Sponsored.*\\.Cost$"))),

    Amazon_Sponsored_Impressions =
      rowSums(pick(matches("^Sponsored.*\\.Impression$")))
  )

# Mostrar primeras seis semanas
datos_amazon |>
  select(Week, starts_with("Amazon_")) |>
  slice_head(n = 6) |>
  print(width = Inf)
# Verificar manualmente las dos primeras semanas
verificacion <- datos_amazon |>
  slice_head(n = 2) |>
  select(Week, starts_with("Amazon_"))

print(verificacion, width = Inf)

# Guardar nueva base procesada
write_csv(
  datos_amazon,
  "Base_RentCar_Amazon.csv"
)

# =====================================
# VERIFICACION DEL PUNTO 2
# =====================================

# Comparar las seis sumas con los datos originales
verificacion_sumas <- tibble(
  variable = c(
    "DSP Cost", "DSP Impressions",
    "Search Cost", "Search Impressions",
    "Sponsored Cost", "Sponsored Impressions"
  ),
  coincide = c(
    isTRUE(all.equal(
      datos_amazon$Amazon_DSP_Cost[1:2],
      rowSums(datos[1:2, grepl("^DSP.*\\.Cost$", names(datos))])
    )),
    isTRUE(all.equal(
      datos_amazon$Amazon_DSP_Impressions[1:2],
      rowSums(datos[1:2, grepl("^DSP.*\\.Impression$", names(datos))])
    )),
    isTRUE(all.equal(
      datos_amazon$Amazon_Search_Cost[1:2],
      rowSums(datos[1:2, grepl("^Search.*\\.Cost$", names(datos))])
    )),
    isTRUE(all.equal(
      datos_amazon$Amazon_Search_Impressions[1:2],
      rowSums(datos[1:2, grepl("^Search.*\\.Impression$", names(datos))])
    )),
    isTRUE(all.equal(
      datos_amazon$Amazon_Sponsored_Cost[1:2],
      rowSums(datos[1:2, grepl("^Sponsored.*\\.Cost$", names(datos))])
    )),
    isTRUE(all.equal(
      datos_amazon$Amazon_Sponsored_Impressions[1:2],
      rowSums(datos[1:2, grepl("^Sponsored.*\\.Impression$", names(datos))])
    ))
  )
)

print(verificacion_sumas)


# =====================================
# TABLA PNG - PUNTO 2
# =====================================

library(gridExtra)
library(grid)

# Tabla de las primeras dos semanas
tabla_punto2 <- datos_amazon |>
  select(Week, starts_with("Amazon_")) |>
  slice_head(n = 2) |>
  mutate(
    Week = as.character(Week),
    across(starts_with("Amazon_"), ~round(.x, 2))
  )

# Crear imagen
imagen_punto2 <- tableGrob(
  tabla_punto2,
  rows = NULL
)

png(
  "Tabla_Agregacion_Amazon.png",
  width = 3000,
  height = 650,
  res = 180
)

grid.newpage()
grid.draw(imagen_punto2)
dev.off()

# =====================================
# TABLA PNG - PUNTO 2
# =====================================

library(gridExtra)
library(grid)

# Tabla de las primeras dos semanas
tabla_punto2 <- datos_amazon |>
  select(Week, starts_with("Amazon_")) |>
  slice_head(n = 2) |>
  mutate(
    Week = as.character(Week),
    across(starts_with("Amazon_"), ~round(.x, 2))
  )
# Crear imagen
imagen_punto2 <- tableGrob(
  tabla_punto2,
  rows = NULL
)

png(
  "Tabla_Agregacion_Amazon.png",
  width = 3000,
  height = 650,
  res = 180
)


grid.newpage()

grid.draw(imagen_punto2)

dev.off()