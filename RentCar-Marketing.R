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

# Seleccionar columnas de publicidad
columnas_publicidad <- datos |>
  select(matches("\\.(Cost|Impression)$"))

# Calcular rangos y valores negativos
rangos_publicidad <- columnas_publicidad |>
  summarise(
    across(
      everything(),
      list(
        minimo = ~min(.x, na.rm = TRUE),
        maximo = ~max(.x, na.rm = TRUE),
        negativos = ~sum(.x < 0, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "resultado"
  )

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

#PUNTO 3
head(trends_nonbrand)
head(trends_brand)


# PUNTO 3 - GOOGLE TRENDS

trends_nonbrand <- read_csv(
  "GoogleTrends_NonBrand.csv",
  show_col_types = FALSE
)

trends_brand <- read_csv(
  "GoogleTrends_Brand.csv",
  show_col_types = FALSE
)

head(trends_nonbrand)
head(trends_brand)

# =====================================
# PUNTO 3 - INTEGRAR GOOGLE TRENDS
# =====================================
library(tidyverse)

datos_amazon <- read_csv(
  "Base_RentCar_Amazon.csv",
  show_col_types = FALSE
)

dim(datos_amazon)

library(tidyverse)
library(lubridate)

# Importar archivos de Google Trends
trends_brand <- read_csv(
  "GoogleTrends_Brand.csv",
  show_col_types = FALSE
)

trends_nonbrand <- read_csv(
  "GoogleTrends_NonBrand.csv",
  show_col_types = FALSE
)

# Preparar las fechas y los indices
brand <- trends_brand |>
  transmute(
    Mes = as.Date(paste0(substr(Time, 1, 7), "-01")),
    Brand = as.numeric(.data[[names(trends_brand)[2]]])
  )

nonbrand <- trends_nonbrand |>
  transmute(
    Mes = as.Date(paste0(substr(Time, 1, 7), "-01")),
    NonBrand = as.numeric(.data[[names(trends_nonbrand)[2]]])
  )

# Unir las dos series de Google Trends
trends_completo <- brand |>
  left_join(nonbrand, by = "Mes")

# Integrar con la base de Amazon
datos_ampliados <- datos_amazon |>
  mutate(
    Week = as.Date(Week),
    Mes = floor_date(Week, "month")
  ) |>
  left_join(trends_completo, by = "Mes")

# Comprobar dimensiones
dim(datos_ampliados)

# Revisar primeras observaciones
datos_ampliados |>
  select(Week, Brand, NonBrand) |>
  head(10) |>
  print()

# Verificar valores faltantes
sum(is.na(datos_ampliados$Brand))
sum(is.na(datos_ampliados$NonBrand))

# Comprobar que no cambie el numero de filas
stopifnot(nrow(datos_ampliados) == nrow(datos_amazon))


# =====================================
# PUNTO 3 - VARIABLES ECONOMICAS FRED
# =====================================

# Importar solicitudes de desempleo
icsa <- read_csv(
  "ICSA.csv",
  show_col_types = FALSE
)

# Importar precio de gasolina
gasolina <- read_csv(
  "GASREGW.csv",
  show_col_types = FALSE
)

# Revisar estructura y fechas
head(icsa)
head(gasolina)

tail(icsa)
tail(gasolina)

# =====================================
# PUNTO 3 - INTEGRAR VARIABLES FRED
# =====================================

# Preparar ICSA
icsa_limpio <- icsa |>
  transmute(
    Week = as.Date(observation_date) + 8,
    ICSA = as.numeric(ICSA)
  )

# Preparar gasolina mensual
gasolina_limpia <- gasolina |>
  transmute(
    Mes = as.Date(observation_date) %m+% months(1),
    GASREGW = as.numeric(GASREGW)
  )

# Unir con la base ampliada
datos_finales <- datos_ampliados |>
  left_join(icsa_limpio, by = "Week") |>
  left_join(gasolina_limpia, by = "Mes")

# Verificar dimensiones
dim(datos_finales)

# Verificar valores faltantes
datos_finales |>
  summarise(
    faltantes_ICSA = sum(is.na(ICSA)),
    faltantes_GASREGW = sum(is.na(GASREGW))
  ) |>
  print()

# Mostrar primeras observaciones
datos_finales |>
  select(Week, Sales, Orders,
         Brand, NonBrand, ICSA, GASREGW) |>
  head(10) |>
  print()

# Comprobar que se mantienen las semanas
stopifnot(nrow(datos_finales) == 135)
# =====================================
# GUARDAR BASE FINAL - PUNTO 3
# =====================================

write_csv(
  datos_finales,
  "Base_RentCar_Final.csv"
)

# =====================================
# PUNTO 3 - AJUSTE TEMPORAL Y VALIDACION
# =====================================

# Usar tendencias del mes anterior
trends_rezagados <- trends_completo |>
  mutate(Mes = Mes %m+% months(1))

# Reconstruir la base con controles rezagados
datos_finales <- datos_amazon |>
  mutate(
    Week = as.Date(Week),
    Mes = floor_date(Week, "month")
  ) |>
  left_join(trends_rezagados, by = "Mes") |>
  left_join(icsa_limpio, by = "Week") |>
  left_join(gasolina_limpia, by = "Mes")

# Verificar dimensiones
dim(datos_finales)

# Verificar duplicados
sum(duplicated(datos_finales$Week))

# Verificar faltantes
datos_finales |>
  summarise(
    N = n(),
    Faltantes_Brand = sum(is.na(Brand)),
    Faltantes_NonBrand = sum(is.na(NonBrand)),
    Faltantes_ICSA = sum(is.na(ICSA)),
    Faltantes_GASREGW = sum(is.na(GASREGW))
  ) |>
  print()

# Guardar la base actualizada
write_csv(
  datos_finales,
  "Base_RentCar_Final.csv"
)


# =====================================
# PUNTO 4 - EXPLORACION Y DESCRIPTIVOS
# =====================================

library(tidyverse)
library(lubridate)
library(ggplot2)

# Cargar base final del punto 3
datos_p4 <- read_csv(
  "Base_RentCar_Final.csv",
  show_col_types = FALSE
)

# Preparar fechas
datos_p4 <- datos_p4 |>
  mutate(Week = as.Date(Week)) |>
  arrange(Week)

# Comprobar dimensiones
dim(datos_p4)

# Revisar variables disponibles
names(datos_p4)

# Revisar faltantes en los controles
datos_p4 |>
  summarise(
    Brand = sum(is.na(Brand)),
    NonBrand = sum(is.na(NonBrand)),
    ICSA = sum(is.na(ICSA)),
    GASREGW = sum(is.na(GASREGW))
  ) |>
  print()


# =====================================
# PUNTO 4 - TABLA DESCRIPTIVA
# =====================================

# Variables solicitadas por el profesor
variables_p4 <- c(
  "Sales", "Orders",
  "Amazon_DSP_Cost",
  "Amazon_DSP_Impressions",
  "Amazon_Search_Cost",
  "Amazon_Search_Impressions",
  "Amazon_Sponsored_Cost",
  "Amazon_Sponsored_Impressions",
  "SP500", "WEI",
  "Brand", "NonBrand",
  "ICSA", "GASREGW"
)

# Verificar que todas existan
stopifnot(all(variables_p4 %in% names(datos_p4)))

# Calcular estadisticas descriptivas
tabla_descriptiva <- datos_p4 |>
  select(all_of(variables_p4)) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor"
  ) |>
  group_by(Variable) |>
  summarise(
    N = sum(!is.na(Valor)),
    Media = mean(Valor, na.rm = TRUE),
    Desviacion = sd(Valor, na.rm = TRUE),
    Minimo = min(Valor, na.rm = TRUE),
    Maximo = max(Valor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Variable = factor(
      Variable,
      levels = variables_p4
    )
  ) |>
  arrange(Variable) |>
  mutate(
    Variable = as.character(Variable),
    across(
      c(Media, Desviacion, Minimo, Maximo),
      ~round(.x, 2)
    )
  )

# Mostrar resultados
print(tabla_descriptiva, n = Inf)

# Guardar tabla como CSV
write_csv(
  tabla_descriptiva,
  "Punto4_Tabla_Descriptiva.csv"
)


# =====================================
# PUNTO 4 - TABLA DESCRIPTIVA PNG
# =====================================

library(gridExtra)
library(grid)

# Formato de tabla
tema_tabla <- ttheme_minimal(
  base_size = 10,
  core = list(
    fg_params = list(col = "#333038"),
    bg_params = list(
      fill = c("#FAF5F7", "#F2DCE5"),
      col = NA
    )
  ),
  colhead = list(
    fg_params = list(
      col = "white",
      fontface = "bold"
    ),
    bg_params = list(
      fill = "#6F2946",
      col = NA
    )
  )
)

# Crear tabla grafica
imagen_descriptiva <- tableGrob(
  tabla_descriptiva,
  rows = NULL,
  theme = tema_tabla
)

# Exportar tabla descriptiva en PNG
png(
  "Punto4_Tabla_Descriptiva.png",
  width = 1900,
  height = 1050,
  res = 150
)

grid::grid.newpage()
grid::grid.draw(imagen_descriptiva)
dev.off()


# =====================================
# PUNTO 4 - SERIES TEMPORALES
# =====================================

library(ggplot2)
library(scales)

# Grafica de ventas semanales
grafica_sales <- ggplot(
  datos_p4,
  aes(x = Week, y = Sales)
) +
  geom_line(
    color = "#6F2946",
    linewidth = 1
  ) +
  labs(
    title = "Evolución semanal de las ventas",
    subtitle = "RentCar Marketing | 2024 - 2026",
    x = "Semana",
    y = "Ventas"
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      color = "#6F2946"
    ),
    plot.subtitle = element_text(
      color = "#B87591"
    ),
    panel.grid.minor = element_blank()
  )

# Guardar grafica de ventas
ggsave(
  "Punto4_Ventas_Semanales.png",
  plot = grafica_sales,
  width = 11,
  height = 6,
  dpi = 300
)

# Grafica de ordenes semanales
grafica_orders <- ggplot(
  datos_p4,
  aes(x = Week, y = Orders)
) +
  geom_line(
    color = "#B87591",
    linewidth = 1
  ) +
  labs(
    title = "Evolución semanal de las órdenes",
    subtitle = "RentCar Marketing | 2024 - 2026",
    x = "Semana",
    y = "Órdenes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      color = "#6F2946"
    ),
    plot.subtitle = element_text(
      color = "#B87591"
    ),
    panel.grid.minor = element_blank()
  )

# Guardar grafica de ordenes
ggsave(
  "Punto4_Ordenes_Semanales.png",
  plot = grafica_orders,
  width = 11,
  height = 6,
  dpi = 300
)

# Mostrar graficas
print(grafica_sales)
print(grafica_orders)


# =====================================
# PUNTO 4 - COSTOS POR CANAL AMAZON
# =====================================

# Preparar datos de costos
costos_amazon <- datos_p4 |>
  select(
    Week,
    Amazon_DSP_Cost,
    Amazon_Search_Cost,
    Amazon_Sponsored_Cost
  ) |>
  pivot_longer(
    cols = -Week,
    names_to = "Canal",
    values_to = "Costo"
  ) |>
  mutate(
    Canal = recode(
      Canal,
      Amazon_DSP_Cost = "Amazon DSP",
      Amazon_Search_Cost = "Amazon Search",
      Amazon_Sponsored_Cost = "Amazon Sponsored"
    )
  )

# Grafica de costos por canal
grafica_costos <- ggplot(
  costos_amazon,
  aes(x = Week, y = Costo, color = Canal)
) +
  geom_line(linewidth = 0.85) +
  scale_color_manual(
    values = c(
      "Amazon DSP" = "#6F2946",
      "Amazon Search" = "#B87591",
      "Amazon Sponsored" = "#D6A5B8"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Evolución de la inversión publicitaria",
    subtitle = "Costos semanales por canal de Amazon | 2024 - 2026",
    x = "Semana",
    y = "Costo publicitario",
    color = "Canal"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      color = "#6F2946"
    ),
    plot.subtitle = element_text(
      color = "#B87591"
    ),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Guardar grafica PNG
ggsave(
  "Punto4_Costos_Amazon.png",
  plot = grafica_costos,
  width = 11,
  height = 6,
  dpi = 300
)

print(grafica_costos)


# =====================================
# PUNTO 4 - DIAGRAMAS DE DISPERSION
# =====================================

# Preparar datos
dispersion_amazon <- datos_p4 |>
  select(
    Sales,
    Amazon_DSP_Cost,
    Amazon_Search_Cost,
    Amazon_Sponsored_Cost
  ) |>
  pivot_longer(
    cols = -Sales,
    names_to = "Canal",
    values_to = "Costo"
  ) |>
  mutate(
    Canal = recode(
      Canal,
      Amazon_DSP_Cost = "Amazon DSP",
      Amazon_Search_Cost = "Amazon Search",
      Amazon_Sponsored_Cost = "Amazon Sponsored"
    )
  )

# Crear graficos de dispersion
grafica_dispersion <- ggplot(
  dispersion_amazon,
  aes(x = Costo, y = Sales)
) +
  geom_point(
    color = "#B87591",
    alpha = 0.65,
    size = 1.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#6F2946",
    fill = "#F2DCE5"
  ) +
  facet_wrap(
    ~Canal,
    scales = "free_x",
    ncol = 3
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  labs(
    title = "Relación entre ventas e inversión publicitaria",
    subtitle = "Análisis exploratorio por canal de Amazon",
    x = "Costo publicitario",
    y = "Ventas"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      color = "#6F2946"
    ),
    plot.subtitle = element_text(
      color = "#B87591"
    ),
    strip.text = element_text(
      face = "bold",
      color = "#6F2946"
    ),
    panel.grid.minor = element_blank()
  )

# Exportar PNG
ggsave(
  "Punto4_Dispersion_Amazon.png",
  plot = grafica_dispersion,
  width = 13,
  height = 5.5,
  dpi = 300
)

print(grafica_dispersion)


# =====================================
# PUNTO 4 - COVARIANZAS Y CORRELACIONES
# =====================================

library(tidyverse)
library(ggplot2)

# Seleccionar variables del analisis
datos_matrices <- datos_p4 |>
  select(all_of(variables_p4))

# Matriz de covarianzas
matriz_cov <- cov(
  datos_matrices,
  use = "pairwise.complete.obs"
)

# Matriz de correlaciones
matriz_cor <- cor(
  datos_matrices,
  use = "pairwise.complete.obs"
)

# Guardar resultados numericos
write.csv(
  matriz_cov,
  "Punto4_Matriz_Covarianzas.csv"
)

write.csv(
  matriz_cor,
  "Punto4_Matriz_Correlaciones.csv"
)

# Covarianzas de Sales con las demas variables
cov_sales <- tibble(
  Variable = names(matriz_cov["Sales", ]),
  Covarianza = as.numeric(matriz_cov["Sales", ])
) |>
  filter(Variable != "Sales")

  # Formatear covarianzas sin notacion cientifica
cov_sales <- cov_sales |>
  mutate(
    Covarianza = format(
      round(Covarianza, 2),
      scientific = FALSE,
      big.mark = ".",
      decimal.mark = ",",
      trim = TRUE
    )
  )

# Crear tabla grafica de covarianzas
tabla_cov <- gridExtra::tableGrob(
  cov_sales,
  rows = NULL,
  theme = tema_tabla
)

png(
  "Punto4_Covarianzas_Sales.png",
  width = 2000,
  height = 1400,
  res = 170
)

grid::grid.newpage()
grid::grid.draw(tabla_cov)
dev.off()

# Preparar correlaciones para la grafica
cor_larga <- as.data.frame(matriz_cor) |>
  rownames_to_column("Variable1") |>
  pivot_longer(
    cols = -Variable1,
    names_to = "Variable2",
    values_to = "Correlacion"
  )


# Matriz de correlaciones con valores numericos
grafica_cor <- ggplot(
  cor_larga,
  aes(x = Variable1, y = Variable2, fill = Correlacion)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = sprintf("%.2f", Correlacion)),
    color = "#333038",
    size = 2.4
  ) +
  scale_fill_gradient2(
    low = "#B87591",
    mid = "#FAF5F7",
    high = "#6F2946",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlación"
  ) +
  coord_equal() +
  labs(
    title = "Matriz de correlaciones",
    subtitle = "Variables de demanda, publicidad y controles",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      color = "#6F2946"
    ),
    plot.subtitle = element_text(color = "#B87591"),
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      size = 8
    ),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

# Guardar la imagen actualizada
ggsave(
  "Punto4_Matriz_Correlaciones.png",
  plot = grafica_cor,
  width = 14,
  height = 11,
  dpi = 300
)

print(grafica_cor)


# =====================================
# PUNTO 5 - REGRESIONES SIMPLES MCO
# =====================================

library(tidyverse)

# Cargar base final
datos_p5 <- read_csv(
  "Base_RentCar_Final.csv",
  show_col_types = FALSE
)

# Modelo 1 - Amazon DSP
modelo_dsp <- lm(
  Sales ~ Amazon_DSP_Cost,
  data = datos_p5
)

# Modelo 2 - Amazon Search
modelo_search <- lm(
  Sales ~ Amazon_Search_Cost,
  data = datos_p5
)

# Modelo 3 - Amazon Sponsored
modelo_sponsored <- lm(
  Sales ~ Amazon_Sponsored_Cost,
  data = datos_p5
)

# Resultados de cada modelo
summary(modelo_dsp)
summary(modelo_search)
summary(modelo_sponsored)

# Intervalos de confianza del 95%
confint(modelo_dsp, level = 0.95)
confint(modelo_search, level = 0.95)
confint(modelo_sponsored, level = 0.95)


# =====================================
# PUNTO 5 - COMPARACION DE REGRESIONES
# =====================================

library(gridExtra)
library(grid)

# Extraer resultados de cada modelo
extraer_resultados <- function(modelo, canal) {

  coef <- summary(modelo)$coefficients
  intervalo <- confint(modelo)[2, ]

  tibble(
    Canal = canal,
    Intercepto = coef[1, 1],
    Coeficiente = coef[2, 1],
    Error_Estandar = coef[2, 2],
    Valor_p = coef[2, 4],
    IC_Inf = intervalo[1],
    IC_Sup = intervalo[2],
    R2 = summary(modelo)$r.squared,
    R2_Ajustado = summary(modelo)$adj.r.squared
  )
}

# Unir los tres modelos
tabla_regresiones <- bind_rows(
  extraer_resultados(modelo_dsp, "Amazon DSP"),
  extraer_resultados(modelo_search, "Amazon Search"),
  extraer_resultados(modelo_sponsored, "Amazon Sponsored")
)

# Guardar resultados numericos originales
write_csv(
  tabla_regresiones,
  "Punto5_Regresiones_Simples.csv"
)

# Dar formato a la tabla para el informe
tabla_formato <- tabla_regresiones |>
  mutate(
    across(
      c(Intercepto, Coeficiente, Error_Estandar,
        IC_Inf, IC_Sup, R2, R2_Ajustado),
      ~format(
        round(.x, 3),
        scientific = FALSE,
        big.mark = ".",
        decimal.mark = ",",
        trim = TRUE
      )
    ),
    Valor_p = if_else(
      Valor_p < 0.001,
      "< 0,001",
      format(
        round(Valor_p, 4),
        decimal.mark = ",",
        trim = TRUE
      )
    )
  )

# Crear tabla PNG
tabla_p5 <- gridExtra::tableGrob(
  tabla_formato,
  rows = NULL,
  theme = tema_tabla
)

png(
  "Punto5_Tabla_Regresiones.png",
  width = 2800,
  height = 950,
  res = 180
)

grid::grid.newpage()
grid::grid.draw(tabla_p5)
dev.off()

# Mostrar tabla en R
print(tabla_regresiones)


# =====================================
# PUNTO 6 - REGRESIONES MULTIPLES
# =====================================

library(tidyverse)

# Cargar base final
datos_p6 <- read_csv(
  "Base_RentCar_Final.csv",
  show_col_types = FALSE
)

# Seleccionar variables para los modelos
variables_p6 <- c(
  "Sales",
  "Orders",
  "Amazon_DSP_Cost",
  "Amazon_Search_Cost",
  "Amazon_Sponsored_Cost",
  "SP500",
  "WEI",
  "Brand",
  "NonBrand",
  "ICSA",
  "GASREGW"
)

# Usar una muestra comun sin faltantes
datos_modelos <- datos_p6 |>
  select(all_of(variables_p6)) |>
  drop_na()

# Comprobar observaciones
nrow(datos_modelos)

# Modelo multiple de ventas
modelo_multiple_sales <- lm(
  Sales ~
    Amazon_DSP_Cost +
    Amazon_Search_Cost +
    Amazon_Sponsored_Cost +
    SP500 +
    WEI +
    Brand +
    NonBrand +
    ICSA +
    GASREGW,
  data = datos_modelos
)

# Modelo multiple de ordenes
modelo_multiple_orders <- lm(
  Orders ~
    Amazon_DSP_Cost +
    Amazon_Search_Cost +
    Amazon_Sponsored_Cost +
    SP500 +
    WEI +
    Brand +
    NonBrand +
    ICSA +
    GASREGW,
  data = datos_modelos
)

# Mostrar resultados
summary(modelo_multiple_sales)
summary(modelo_multiple_orders)

# Intervalos de confianza del 95%
confint(modelo_multiple_sales)
confint(modelo_multiple_orders)

# =====================================
# PUNTO 6 - TABLA COMPARATIVA
# =====================================

library(tidyverse)
library(gridExtra)
library(grid)

# Extraer coeficientes de cada modelo
extraer_p6 <- function(modelo, nombre) {
  resultado <- summary(modelo)$coefficients

  tibble(
    Variable = rownames(resultado),
    Modelo = nombre,
    Coeficiente = resultado[, 1],
    Error_Estandar = resultado[, 2],
    Valor_p = resultado[, 4]
  )
}

# Unir resultados
tabla_p6 <- bind_rows(
  extraer_p6(modelo_multiple_sales, "Sales"),
  extraer_p6(modelo_multiple_orders, "Orders")
)

# Guardar resultados originales
write_csv(
  tabla_p6,
  "Punto6_Regresiones_Multiples.csv"
)

# Dar formato a la tabla
tabla_p6_formato <- tabla_p6 |>
  mutate(
    Coeficiente = format(
      round(Coeficiente, 4),
      scientific = FALSE,
      big.mark = ".",
      decimal.mark = ",",
      trim = TRUE
    ),
    Error_Estandar = format(
      round(Error_Estandar, 4),
      scientific = FALSE,
      big.mark = ".",
      decimal.mark = ",",
      trim = TRUE
    ),
    Valor_p = if_else(
      Valor_p < 0.001,
      "< 0,001",
      format(
        round(Valor_p, 4),
        decimal.mark = ",",
        trim = TRUE
      )
    )
  )

# Crear imagen con el tema del punto 4
imagen_p6 <- gridExtra::tableGrob(
  tabla_p6_formato,
  rows = NULL,
  theme = tema_tabla
)

png(
  "Punto6_Tabla_Regresiones.png",
  width = 2200,
  height = 1800,
  res = 170
)

grid::grid.newpage()
grid::grid.draw(imagen_p6)
dev.off()

print(tabla_p6)


# =====================================
# PUNTO 6 - RESULTADOS COMPLEMENTARIOS
# =====================================

# Resumen de los modelos
resumen_p6 <- tibble(
  Modelo = c("Sales", "Orders"),
  N = c(nobs(modelo_multiple_sales),
        nobs(modelo_multiple_orders)),
  R2 = c(summary(modelo_multiple_sales)$r.squared,
         summary(modelo_multiple_orders)$r.squared),
  R2_Ajustado = c(
    summary(modelo_multiple_sales)$adj.r.squared,
    summary(modelo_multiple_orders)$adj.r.squared
  ),
  Valor_p_Global = c(
    pf(summary(modelo_multiple_sales)$fstatistic[1],
       summary(modelo_multiple_sales)$fstatistic[2],
       summary(modelo_multiple_sales)$fstatistic[3],
       lower.tail = FALSE),
    pf(summary(modelo_multiple_orders)$fstatistic[1],
       summary(modelo_multiple_orders)$fstatistic[2],
       summary(modelo_multiple_orders)$fstatistic[3],
       lower.tail = FALSE)
  )
)

print(resumen_p6)

write_csv(
  resumen_p6,
  "Punto6_Resumen_Modelos.csv"
)

# Guardar intervalos del 95 %
intervalos_p6 <- bind_rows(
  as_tibble(
    confint(modelo_multiple_sales),
    rownames = "Variable"
  ) |> mutate(Modelo = "Sales"),
  as_tibble(
    confint(modelo_multiple_orders),
    rownames = "Variable"
  ) |> mutate(Modelo = "Orders")
)

write_csv(
  intervalos_p6,
  "Punto6_Intervalos_Confianza.csv"
)


# =====================================
# PUNTO 7 - DOS CAMINOS PARA EXAMINAR LOS PICOS DE TEMPORADA ALTA
# =====================================

# Camino A: Transformación logarítmica de la variable dependiente (Sales y Orders)
modelo_log_sales <- lm(
  log(Sales) ~ Amazon_DSP_Cost + Amazon_Search_Cost + Amazon_Sponsored_Cost +
    SP500 + WEI + Brand + NonBrand + ICSA + GASREGW,
  data = datos_p7
)

modelo_log_orders <- lm(
  log(Orders) ~ Amazon_DSP_Cost + Amazon_Search_Cost + Amazon_Sponsored_Cost +
    SP500 + WEI + Brand + NonBrand + ICSA + GASREGW,
  data = datos_p7
)

summary(modelo_log_sales)
summary(modelo_log_orders)

# Camino B: Variables indicadoras (Dummies) de temporada alta
# Identificación de semanas con picos superiores al percentil 85 de ventas
umbral_pico <- quantile(datos_p7$Sales, 0.85)

datos_p7 <- datos_p7 |>
  mutate(
    Temporada_Alta = if_else(Sales > umbral_pico, 1, 0)
  )

modelo_dummy_sales <- lm(
  Sales ~ Amazon_DSP_Cost + Amazon_Search_Cost + Amazon_Sponsored_Cost +
    SP500 + WEI + Brand + NonBrand + ICSA + GASREGW + Temporada_Alta,
  data = datos_p7
)

modelo_dummy_orders <- lm(
  Orders ~ Amazon_DSP_Cost + Amazon_Search_Cost + Amazon_Sponsored_Cost +
    SP500 + WEI + Brand + NonBrand + ICSA + GASREGW + Temporada_Alta,
  data = datos_p7
)

summary(modelo_dummy_sales)
summary(modelo_dummy_orders)
