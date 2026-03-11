# Librerías

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(viridisLite)
library(viridis)
library(writexl)
library(scales)
library(ggrepel)
library(colorspace)
library(lme4)
library(BayesMortalityPlus)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(stringi)
library(sf)
library(ggplot2)
library(ggrepel)
library(scales)
library(viridis)
library(cowplot)
library(gridExtra)
library(grid)





# 1️⃣ Cargar datos ------------------------------------------------------
defunciones <- read_excel("/Users/alevalenzuelasaldia/Desktop/REGION_DEF.xlsx", sheet = "DEF")
rate_data <- read_excel("/Users/alevalenzuelasaldia/Desktop/REGION_DEF.xlsx", sheet = "GRUPO_ETARIO_REGION")
regiones_shp <- st_read("/Users/alevalenzuelasaldia/Desktop/gadm41_CHL_shp(2)/gadm41_CHL_1.shp")

# 2️⃣ Procesar datos por tramo etario -----------------------------------
# Clasificar edad en tramos
clasificar_tramo <- function(edad) {
  case_when(
    edad < 25 ~ "Menor de 25",
    between(edad, 25, 29) ~ "25 - 29",
    between(edad, 30, 34) ~ "30 - 34",
    between(edad, 35, 39) ~ "35 - 39",
    between(edad, 40, 44) ~ "40 - 44",
    between(edad, 45, 49) ~ "45 - 49",
    between(edad, 50, 54) ~ "50 - 54",
    between(edad, 55, 59) ~ "55 - 59",
    between(edad, 60, 64) ~ "60 - 64",
    edad >= 65 ~ "65 y más",
    TRUE ~ "Desconocido"
  )
}

tramos_etarios <- c("Menor de 25", "25 - 29", "30 - 34", "35 - 39",
                    "40 - 44", "45 - 49", "50 - 54", "55 - 59",
                    "60 - 64", "65 y más")


# Agregar tramos etarios
defunciones <- defunciones %>%
  mutate(Tramo_Etario = clasificar_tramo(EDAD_CANT))

# Agrupar por año y tramo etario
fallecidas_por_tramo <- defunciones %>%
  group_by(AÑO, Tramo_Etario) %>%
  summarise(NUMERO_DE_FALLECIDAS = n(), .groups = "drop") %>%
  mutate(Tramo_Etario = factor(Tramo_Etario, levels = tramos_etarios)) %>%
  arrange(AÑO, Tramo_Etario)


# Agrupar por región, año y tramo etario
fallecidas_por_region_tramo <- defunciones %>%
  group_by(NOMBRE_REGION, AÑO, Tramo_Etario) %>%
  summarise(NUMERO_DE_FALLECIDAS = n(), .groups = "drop")

# Completar combinaciones faltantes
combinaciones <- expand.grid(NOMBRE_REGION = unique(defunciones$NOMBRE_REGION),
                             AÑO = unique(defunciones$AÑO),
                             Tramo_Etario = tramos_etarios)

fallecidas_completo <- combinaciones %>%
  left_join(fallecidas_por_region_tramo, by = c("NOMBRE_REGION", "AÑO", "Tramo_Etario")) %>%
  replace_na(list(NUMERO_DE_FALLECIDAS = 0)) %>%
  mutate(Tramo_Etario = factor(Tramo_Etario, levels = tramos_etarios)) %>%
  arrange(NOMBRE_REGION, AÑO, Tramo_Etario)

# Total anual de muertes
fallecidas_anual <- defunciones %>%
  filter(!is.na(AÑO)) %>%
  count(AÑO, name = "Total_Fallecidas")

# 3️⃣ Calcular SMR y tasas indirectas -----------------------------------
# Calcular muertes esperadas
rate_data <- rate_data %>%
  mutate(
    TasacrudaReferencial = if_else(TasacrudaReferencial == 0, 0.00001, TasacrudaReferencial),
    MuertesEsperadas = PoblacionVigente * TasacrudaReferencial
  )

# SMR por año
SMR_anual <- rate_data %>%
  group_by(AÑO) %>%
  summarise(
    TotalMuertesEsperadas = sum(MuertesEsperadas, na.rm = TRUE),
    TotalMuertesObservadas = sum(NUMERO_DE_FALLECIDAS, na.rm = TRUE),
    SMR = TotalMuertesObservadas / TotalMuertesEsperadas,
    .groups = "drop"
  )


# SMR por región y año
SMR_region_anual <- rate_data %>%
  group_by(NOMBRE_REGION, AÑO) %>%
  summarise(
    TotalMuertesEsperadas = sum(MuertesEsperadas, na.rm = TRUE),
    TotalMuertesObservadas = sum(NUMERO_DE_FALLECIDAS, na.rm = TRUE),
    SMR = TotalMuertesObservadas / TotalMuertesEsperadas,
    .groups = "drop"
  )

# SMR por región, año y tramo etario
SMR_region_tramo <- rate_data %>%
  mutate(Tramo_Etario = factor(Tramo_Etario, levels = tramos_etarios)) %>%
  group_by(NOMBRE_REGION, AÑO, Tramo_Etario) %>%
  summarise(
    TotalMuertesEsperadas = sum(MuertesEsperadas, na.rm = TRUE),
    TotalMuertesObservadas = sum(NUMERO_DE_FALLECIDAS, na.rm = TRUE),
    SMR = TotalMuertesObservadas / TotalMuertesEsperadas,
    .groups = "drop"
  )

# SMR por año y tramo etario
SMR_año_tramo <- rate_data %>%
  mutate(Tramo_Etario = factor(Tramo_Etario, levels = tramos_etarios)) %>%
  group_by(AÑO, Tramo_Etario) %>%
  summarise(
    TotalMuertesEsperadas = sum(MuertesEsperadas, na.rm = TRUE),
    TotalMuertesObservadas = sum(NUMERO_DE_FALLECIDAS, na.rm = TRUE),
    SMR = TotalMuertesObservadas / TotalMuertesEsperadas,
    .groups = "drop"
  )

SMR_region_tramo_max <- SMR_region_tramo %>%
  group_by(NOMBRE_REGION, Tramo_Etario) %>%
  summarise(
    Max_SMR = max(SMR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Max_SMR))

### GRAFICO DE BARRA DE DEFUNCIONES #####
ggplot(fallecidas_anual, aes(x = factor(AÑO), y = Total_Fallecidas)) +
  geom_col(fill = "cyan3", width = 0.7) +
  geom_text(
    aes(label = comma(Total_Fallecidas)),
    vjust = -0.3,            # altura de la etiqueta sobre la barra
    size  = 4,               # tamaño del texto
    color = "#2E2E2E"        # color del texto
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Número total de muertes año",
    x = "Año",
    y = "Frecuencia Absoluta"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title  = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2E2E2E"),
    axis.title.x = element_text(size = 14, face = "bold", color = "#2E2E2E"),
    axis.title.y = element_text(size = 14, face = "bold", color = "#2E2E2E"),
    axis.text.x  = element_text(size = 12, angle = 0,  color = "#2E2E2E"),
    axis.text.y  = element_text(size = 12,               color = "#2E2E2E")
  )

## BOXPLOTS ####
# Boxplot de SMR por Tramo Etario y Año
ggplot(SMR_region_tramo, aes(x = Tramo_Etario, y = SMR)) +
  geom_boxplot(
    aes(fill = factor(AÑO)),
    position = position_dodge(width = 0.8),
    outlier.shape = NA,
    width = 0.6,
    alpha = 0.8
  ) +
  geom_point(
    data = SMR_region_tramo %>% filter(Tramo_Etario == "Menor de 25"),
    aes(x = Tramo_Etario, y = SMR, color = factor(AÑO)),
    position = position_jitter(width = 0.2, height = 0),
    size = 2,
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_y_continuous(
    limits = c(0, 15),  # Limita el eje Y hasta 15
    expand = expansion(mult = c(0.05, 0.1))  # Expansión para mayor claridad
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Razón de Mortalidad Estandarizada por Tramo Etario y Año",
    x = "Tramo Etario",
    y = "Razón Mortalidad Estandarizada",
    fill = "Año",
    color = "Año"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


######### HISTOGRAMA POR EDAD QUINQUENALES D ELA SUMA DE LOS AÑOS 
### boxplots por region


df_box <- SMR_region_anual %>%
  filter(
    AÑO %in% 2011:2018,
    !str_detect(NOMBRE_REGION, regex("metropolitana", ignore_case = TRUE)),
    !is.na(SMR)
  )


ggplot(
  df_box,
  aes(
    x = SMR,
    y = reorder(NOMBRE_REGION, SMR, FUN = median, na.rm = TRUE)
  )
) +
  geom_boxplot(
    fill = "cadetblue2",
    color = "cadetblue",
    linewidth = 0.7,
    outlier.shape = 16,      # deja outliers del boxplot
    outlier.size = 1.6,
    outlier.alpha = 0.6
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  labs(
    title = "Razón de Mortalidad Estandarizada",
    x = "Razón de Mortalidad Estandarizada (SMR)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9)
  )



# --- 0) Preparar datos (todas los años) ------------------------------------
df_edad <- defunciones %>%
  filter(!is.na(EDAD_CANT)) %>%
  mutate(EDAD_CANT = as.integer(EDAD_CANT)) %>%
  filter(EDAD_CANT >= 0, EDAD_CANT <= 110)

edad_max <- max(df_edad$EDAD_CANT, na.rm = TRUE)

# Estadísticos para líneas guía
stats_ed <- df_edad %>%
  summarise(
    media   = mean(EDAD_CANT, na.rm = TRUE))
# === Histograma en tramos de 5 años (más compacto) ======================
bw <- 5
ggplot(df_edad, aes(x = EDAD_CANT)) +
  geom_histogram(binwidth = bw, boundary = 0, closed = "left",
                 fill = "cyan3", color = "white", size = 0.2) +
  geom_vline(xintercept = stats_ed$media,   linetype = "dotted", color = "#2E2E2E") +
  scale_x_continuous(breaks = seq(0, edad_max + (bw - edad_max %% bw) %% bw, by = bw)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribución de defunciones por Cancer Cervicouterino por Edad (todos los años)",
    subtitle = paste0("Media = ", round(stats_ed$media, 1)),
    x = "Edad (en años por quinquenales)",
    y = "Número de fallecidas por Cancer Cervicouterino"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#### GRAFICO DE PROPORCION #####
breaks_q5 <- c(seq(0, 80, by = 5), Inf)
labels_q5 <- c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)", "[30,35)",
               "[35,40)", "[40,45)", "[45,50)", "[50,55)", "[55,60)", "[60,65)",
               "[65,70)", "[70,75)", "[75,80)", "[80+]")

df_q5 <- defunciones %>%
  filter(AÑO %in% AÑOS, !is.na(EDAD_CANT)) %>%
  mutate(EDAD_CANT = as.integer(EDAD_CANT)) %>%
  filter(EDAD_CANT >= 0, EDAD_CANT <= 110) %>%
  mutate(
    tramo = cut(EDAD_CANT, breaks = breaks_q5, right = FALSE, labels = labels_q5)
  ) %>%
  count(tramo, name = "Defunciones") %>%
  complete(tramo = factor(labels_q5, levels = labels_q5), fill = list(Defunciones = 0)) %>%
  mutate(
    pct = Defunciones / sum(Defunciones),
    lab = paste0(Defunciones, "\n(", percent(pct, accuracy = 0.01), ")")
  )

ggplot(df_q5, aes(x = tramo, y = Defunciones, group = 1)) +
  geom_line(linetype = "dotted", linewidth = 1) +
  geom_point(size = 4, shape = 21, fill = "cyan3", color = "black", stroke = 1) +
  geom_text(aes(label = lab), vjust = -0.6, size = 3.6) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Defunciones acumuladas (2011–2018) por edades quinquenales",
    x = "Edades Quinquenales",
    y = "Número de fallecidas por cáncer cervicouterino"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0),
    panel.grid.minor = element_blank()
  )


# === Barras discretas por edad (si quiere 100% discreto) ================
conteo_edad <- df_edad %>%
  count(EDAD_CANT, name = "Defunciones")

conteo_q5 <- df_edad %>%
  mutate(
    tramo_q5 = cut(
      EDAD_CANT,
      breaks = seq(0, edad_max + 5, by = 5),
      right = FALSE
    )
  ) %>%
  count(tramo_q5, name = "Defunciones")

ggplot(df_edad, aes(x = EDAD_CANT)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    closed   = "left",
    fill     = "cyan3",
    color    = NA        # <- elimina las líneas blancas
  ) +
  geom_vline(
    xintercept = stats_ed$media,
    linetype = "dotted",
    color = "#2E2E2E"
  ) +
  scale_x_continuous(
    breaks = seq(0, edad_max, by = 5),
    limits = c(0, edad_max),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0)
  ) +
  labs(
    title = "Defunciones por edad (todos los años)",
    subtitle = paste0("Media = ", round(stats_ed$media, 1)),
    x = "Edad",
    y = "Frecuencia Absoluta"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x  = element_text(face = "bold"),
    axis.title.y  = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

geom_histogram(
  binwidth = 1,
  boundary = 0,
  closed   = "left",
  fill     = "cyan3",
  alpha    = 0.9,
  color    = NA
)




###### GRAFICO QUINQUENAL ####
# --- Tramos quinquenales desde 15 años ---
breaks_q5  <- c(seq(15, 80, by = 5), Inf)         # [15-20), [20-25), ..., [80, Inf)
labels_q5  <- c("15–19","20–24","25–29","30–34","35–39","40–44",
                "45–49","50–54","55–59","60–64","65–69","70–74",
                "75–79","80+")

# --- Datos: conteo por año (2011..2018) y tramo quinquenal ---
dist_q5_cnt <- defunciones %>%
  filter(!is.na(EDAD_CANT), !is.na(AÑO), AÑO %in% 2011:2018) %>%
  mutate(
    edad = pmin(pmax(as.integer(EDAD_CANT), 0L), 110L),
    # descartar menores de 15 explícitamente
    edad = ifelse(edad < 15, NA_integer_, edad)
  ) %>%
  filter(!is.na(edad)) %>%
  mutate(
    tramo_q5 = cut(edad, breaks = breaks_q5, right = FALSE, labels = labels_q5)
  ) %>%
  count(AÑO, tramo_q5, name = "Fallecidas") %>%
  complete(
    AÑO = 2011:2018,
    tramo_q5 = factor(labels_q5, levels = labels_q5),
    fill = list(Fallecidas = 0)
  ) %>%
  mutate(AÑO = factor(AÑO, levels = 2011:2018))

# --- Paleta manual (8 colores, uno por año) ---
pal8 <- RColorBrewer::brewer.pal(8, "Dark2")
names(pal8) <- levels(dist_q5_cnt$AÑO)

# --- Gráfico ---
ggplot(dist_q5_cnt,
       aes(x = tramo_q5, y = Fallecidas, group = AÑO, color = AÑO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(name = "Año", values = pal8) +
  labs(
    title = "Defunciones por Edad Quinquenales",
    x = "Tramo de edad (años)",
    y = "Número de fallecidas por cáncer cervicouterino"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))
# 4️⃣ Tasa estandarizada indirecta ---------------------------------------
tasa_estandarizada_total <- rate_data %>%
  group_by(AÑO) %>%
  summarise(
    TotalMuertesEsperadas = sum(MuertesEsperadas, na.rm = TRUE),
    TotalPoblacionVigente = sum(PoblacionVigente, na.rm = TRUE),
    TasaEstandarizadaIndirecta = round(TotalMuertesEsperadas / TotalPoblacionVigente, 6),
    .groups = "drop"
  )


tasa_estandarizada_region <- rate_data %>%
  filter(AÑO == 2017) %>%
  group_by(NOMBRE_REGION, AÑO) %>%
  summarise(
    TotalMuertesEsperadas = sum(MuertesEsperadas, na.rm = TRUE),
    TotalPoblacionVigente = sum(PoblacionVigente, na.rm = TRUE),
    TasaEstandarizadaIndirecta = round(TotalMuertesEsperadas / TotalPoblacionVigente, 6),
    .groups = "drop"
  )

# 5️⃣ Mapas con shapefile ------------------------------------------------
# Correspondencia de nombres
correspondencia <- data.frame(
  NOMBRE_LARGO = c("De Aisén del Gral. C. Ibáñez del Campo", "De Antofagasta", "De Arica y Parinacota",
                   "De Atacama", "De Coquimbo", "De La Araucanía", "De Los Lagos", "De Los Ríos",
                   "De Magallanes y de La Antártica Chilena", "De Tarapacá", "De Valparaíso", "De Ñuble",
                   "Del Bíobío", "Del Libertador B. O'Higgins", "Del Maule", "Metropolitana de Santiago"),
  NOMBRE_CORTO = c("Aysén del General Ibañez del Cam", "Antofagasta", "Arica y Parinacota", "Atacama",
                   "Coquimbo", "Araucanía", "Los Lagos", "Los Ríos", "Magallanes y Antártica Chilena",
                   "Tarapacá", "Valparaíso", "Ñuble", "Bío-Bío", "Libertador General Bernardo O'Hi",
                   "Maule", "Santiago Metropolitan")
)

# Unir shapefile con tasas
tasa_estandarizada_region <- tasa_estandarizada_region %>%
  left_join(correspondencia, by = c("NOMBRE_REGION" = "NOMBRE_LARGO"))

mapa_data <- regiones_shp %>%
  left_join(tasa_estandarizada_region, by = c("NAME_1" = "NOMBRE_CORTO"))

# Mapa con círculos proporcionales
mapa_centroides <- st_centroid(mapa_data) %>%
  mutate(
    LONGITUD = st_coordinates(geometry)[, 1],
    LATITUD = st_coordinates(geometry)[, 2]
  )

mapa_data_centroides <- mapa_centroides %>%
  mutate(
    TasaEstandarizadaIndirecta100k = TasaEstandarizadaIndirecta * 100000
  )

SMR_año_tramo <- rate_data %>%
  group_by(AÑO, Tramo_Etario) %>%
  summarise(
    SMR = sum(NUMERO_DE_FALLECIDAS, na.rm = TRUE) / sum(MuertesEsperadas, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(SMR_año_tramo, aes(x = Tramo_Etario, y = SMR, color = factor(AÑO), group = AÑO)) +
  geom_line() +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "SMR promedio por Tramo Etario y Año (Chile total)",
    x = "Tramo Etario",
    y = "Índice SMR",
    color = "Año"
  ) +
  theme_minimal()

ggplot(SMR_año_tramo, aes(x = Tramo_Etario, y = SMR, color = factor(AÑO), group = AÑO)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Tendencia Suavizada del SMR por Tramo Etario y Año (Chile)",
    x = "Tramo Etario",
    y = "Índice SMR",
    color = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


# 6️⃣ Gráficos de SMR ----------------------------------------------------
# SMR por año
ggplot(SMR_anual, aes(x = AÑO, y = SMR)) +
  geom_line(color = "springgreen", size = 1) +
  geom_point(color = "springgreen2", size = 3) +
  labs(title = "SMR por Año", x = "Año", y = "SMR") +
  scale_x_continuous(breaks = seq(min(SMR_anual$AÑO), max(SMR_anual$AÑO), by = 1)) +
  theme_minimal()

# SMR por región y año
ggplot(SMR_region_anual, aes(x = AÑO, y = SMR, color = NOMBRE_REGION, group = NOMBRE_REGION)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "SMR por Región y Año", x = "Año", y = "SMR") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12))

# Paneles por región
ggplot(SMR_region_anual, aes(x = AÑO, y = SMR, color = NOMBRE_REGION)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ NOMBRE_REGION, scales = "free_y") +
  labs(title = "SMR por Región y Año", x = "Año", y = "SMR") +
  theme_minimal()

##### por rango etario y año

ggplot(SMR_region_tramo, aes(x = Tramo_Etario, y = SMR, group = AÑO, color = factor(AÑO))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ NOMBRE_REGION) +
  labs(
    title = "Índice SMR por Región, Tramo Etario y Año",
    x = "Tramo Etario",
    y = "Índice SMR",
    color = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


###############GRAFICO CHILE RATES #################
# 1) Preparar tabla de tasas por región y año (si no la tiene ya)
tasas_region_anio <- rate_data %>%
  group_by(NOMBRE_REGION, AÑO) %>%
  summarise(
    TotalMuertesEsperadas = sum(MuertesEsperadas, na.rm = TRUE),
    TotalPoblacionVigente = sum(PoblacionVigente, na.rm = TRUE),
    TasaEstandarizadaIndirecta = TotalMuertesEsperadas / TotalPoblacionVigente,
    .groups = "drop"
  ) %>%
  # mapear al nombre corto del shapefile
  left_join(correspondencia, by = c("NOMBRE_REGION" = "NOMBRE_LARGO"))

# 2) Función de mapa para un año (sin facet)
plot_mapa_tasa_indirecta <- function(anio_sel,
                                     etiqueta = c("region", "valor", "ambos"),
                                     trans_scale = c("identity", "sqrt", "log10"),
                                     ndigitos = 1) {
  
  etiqueta    <- match.arg(etiqueta)
  trans_scale <- match.arg(trans_scale)
  
  # Datos del año
  df_anio <- tasas_region_anio %>%
    filter(AÑO == anio_sel)
  
  # Unir con shapefile (garantiza las 16 regiones aunque falte alguna tasa)
  mapa_anio <- regiones_shp %>%
    rename(NOMBRE_CORTO = NAME_1) %>%
    left_join(df_anio, by = "NOMBRE_CORTO") %>%
    mutate(
      Tasa100k = 1e5 * TasaEstandarizadaIndirecta
    )
  
  # Centroides para etiquetas
  centros <- mapa_anio %>%
    st_point_on_surface() %>%
    cbind(st_coordinates(.)) %>%
    st_drop_geometry() %>%
    rename(LONGITUD = X, LATITUD = Y)
  
  # Texto de etiquetas
  lab_fmt <- label_number(accuracy = 10^(-ndigitos), big.mark = ",")
  etiquetas <- centros %>%
    mutate(
      lbl = dplyr::case_when(
        etiqueta == "region" ~ NOMBRE_CORTO,
        etiqueta == "valor"  ~ paste0(lab_fmt(Tasa100k)),
        TRUE                 ~ paste0(NOMBRE_CORTO, "\n", lab_fmt(Tasa100k))
      )
    )
  
  # Gráfico
  ggplot() +
    geom_sf(
      data  = mapa_anio,
      aes(fill = Tasa100k),
      color = "white", size = 0.25
    ) +
    scale_fill_viridis_c(
      name   = "Tasa (x100.000)",
      option = "D",
      labels = lab_fmt,
      trans  = trans_scale,
      na.value = "grey90",
      guide = guide_colorbar(
        barheight = unit(80, "pt"),
        barwidth  = unit(10, "pt"),
        ticks.colour = "grey30"
      )
    ) +
    geom_label_repel(
      data = etiquetas,
      aes(x = LONGITUD, y = LATITUD, label = lbl),
      size = 2.6, fill = "white", color = "#1F1F1F", label.size = 0.2,
      segment.color = "grey35", segment.size = 0.25,
      box.padding = 0.25, point.padding = 0.1, min.segment.length = 0,
      arrow = arrow(length = unit(0.012, "npc")),
      max.overlaps = Inf, seed = 123
    ) +
    labs(
      title    = paste0("Tasa estandarizada indirecta por región — ", anio_sel),
      subtitle = "Expresada por 100.000 Mujeres"
    ) +
    coord_sf() +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "#4a4a4a"),
      plot.caption  = element_text(size = 9, colour = "#6b6b6b"),
      panel.grid    = element_blank(),
      panel.background = element_rect(fill = "#FAFBFC", colour = NA),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 10)
    )
}

# === Ejemplos de uso ===
# Solo nombres de región como etiquetas
#plot_mapa_tasa_indirecta(2017, etiqueta = "region", trans_scale = "identity", ndigitos = 1)
# Región + valor (en dos líneas)
plot_mapa_tasa_indirecta(2018, etiqueta = "ambos", trans_scale = "identity", ndigitos = 1)

#################################  region y numero fallecidas lineas 419 a 586 #####################################
# Normalizar nombres para emparejar entre fuentes
norm_nom <- function(x) {
  x %>%
    stringr::str_squish() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper()
}

# ============ PREPARACIÓN ÚNICA (hacer una vez) ============ 
# 1) Shapefile de regiones (con nombre corto)
regiones_sf <- regiones_shp %>%
  st_as_sf() %>%
  transmute(
    NOMBRE_CORTO = NAME_1,                     # ajuste si su campo difiere
    NOMBRE_CORTO_NORM = norm_nom(NAME_1),
    geometry
  )

# 2) IDs estables por región (orden alfabético del nombre corto)
ids_regiones <- regiones_sf %>%
  st_drop_geometry() %>%
  arrange(NOMBRE_CORTO) %>%
  mutate(ID_REGION = row_number())

# 3) Centroides/centros para etiquetas (calculado una vez)
centros_region <- regiones_sf %>%
  st_point_on_surface() %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  rename(LONGITUD = X, LATITUD = Y) %>%
  select(NOMBRE_CORTO, LONGITUD, LATITUD)

# 4) Correspondencia normalizada (largo DEF → corto SHP)
corresp_norm <- correspondencia %>%
  transmute(
    NOMBRE_LARGO_NORM = norm_nom(NOMBRE_LARGO),
    NOMBRE_CORTO,
    NOMBRE_CORTO_NORM = norm_nom(NOMBRE_CORTO)
  )

# 5) Defunciones con nombre normalizado
def_norm <- defunciones %>%
  mutate(NOMBRE_REGION_NORM = norm_nom(NOMBRE_REGION))

# 6) Conteo fallecidas por región y año (tabla maestra)
fallecidas_region_anio <- def_norm %>%
  filter(!is.na(AÑO)) %>%
  count(NOMBRE_REGION_NORM, AÑO, name = "Fallecidas") %>%
  # mapear al nombre corto del shapefile
  left_join(select(corresp_norm, NOMBRE_LARGO_NORM, NOMBRE_CORTO_NORM),
            by = c("NOMBRE_REGION_NORM" = "NOMBRE_LARGO_NORM"))

# ============ FUNCIÓN DEL MAPA (un año, sin facet) ============ 
plot_mapa_fallecidas_anio <- function(anio_sel,
                                      etiqueta = c("id", "conteo", "region_y_conteo"),
                                      trans_scale = c("sqrt", "identity", "log10"),
                                      mostrar_tabla = TRUE) {
  
  etiqueta    <- match.arg(etiqueta)
  trans_scale <- match.arg(trans_scale)
  
  # Datos para el año seleccionado
  df_anio <- fallecidas_region_anio %>%
    filter(AÑO == anio_sel)
  
  # Unir a shapefile (por nombre corto normalizado), añadir IDs
  datos_mapa <- regiones_sf %>%
    left_join(df_anio, by = c("NOMBRE_CORTO_NORM" = "NOMBRE_CORTO_NORM")) %>%
    left_join(ids_regiones, by = "NOMBRE_CORTO") %>%
    mutate(Fallecidas = coalesce(Fallecidas, 0L))
  
  # Centroides + etiquetas
  datos_lab <- centros_region %>%
    left_join(select(datos_mapa %>% st_drop_geometry(),
                     NOMBRE_CORTO, Fallecidas, ID_REGION),
              by = "NOMBRE_CORTO") %>%
    mutate(Fallecidas = coalesce(Fallecidas, 0L),
           etq = dplyr::case_when(
             etiqueta == "id"               ~ as.character(ID_REGION),
             etiqueta == "conteo"           ~ scales::comma(Fallecidas),
             etiqueta == "region_y_conteo"  ~ paste0(NOMBRE_CORTO, "\n", scales::comma(Fallecidas))
           ))
  
  total_anio <- sum(datos_mapa$Fallecidas, na.rm = TRUE)
  
  # Mapa principal
  p_mapa <- ggplot() +
    geom_sf(
      data  = datos_mapa,
      aes(fill = Fallecidas),
      color = "white", size = 0.25
    ) +
    scale_fill_viridis_c(
      name   = "N° fallecidas",
      option = "D",
      labels = scales::comma,
      trans  = trans_scale,
      na.value = "grey90",
      guide = guide_colorbar(
        barheight = unit(80, "pt"),
        barwidth  = unit(10, "pt"),
        ticks.colour = "grey30"
      )
    ) +
    geom_label_repel(
      data = datos_lab,
      aes(x = LONGITUD, y = LATITUD, label = etq),
      size = 3,
      fill = "white", color = "#1F1F1F", label.size = 0.2,
      segment.color = "grey35", segment.size = 0.3,
      box.padding = 0.3, point.padding = 0.1, min.segment.length = 0,
      arrow = arrow(length = unit(0.012, "npc")),
      max.overlaps = Inf, seed = 123
    ) +
    labs(
      title    = paste0("Fallecidas por Región (", anio_sel, ")"),
      subtitle = paste("Total país:", scales::comma(total_anio)),
      caption  = "Fuente: DEIS — Elaboración propia"
    ) +
    coord_sf() +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "#4a4a4a"),
      plot.caption  = element_text(size = 9, colour = "#6b6b6b"),
      panel.grid    = element_blank(),
      panel.background = element_rect(fill = "#FAFBFC", colour = NA),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 10)
    )
  
  if (!mostrar_tabla) return(p_mapa)
  
  # Tabla-leyenda (ID → Región → Fallecidas)
  leyenda_regiones <- datos_mapa %>%
    st_drop_geometry() %>%
    transmute(
      `ID Región`         = ID_REGION,
      `Región`            = NOMBRE_CORTO,
      `N° Fallecidas`     = Fallecidas
    ) %>%
    arrange(`ID Región`)
  
  # (Opcional) envolver nombres largos
  # leyenda_regiones$`Región` <- stringr::str_wrap(leyenda_regiones$`Región`, width = 22)
  
  leyenda_tbl <- tableGrob(
    leyenda_regiones,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 9,
      colhead = list(fg_params = list(fontface = 2, hjust = 0.5)),
      core    = list(fg_params = list(hjust = 0, x = 0.02))
    )
  )
  
  # Componer: mapa + tabla al lado (ancho ajustable)
  plot_grid(
    p_mapa,
    ggdraw() + draw_grob(leyenda_tbl),
    ncol = 2, rel_widths = c(0.66, 0.34)
  )
}

# ======= EJEMPLO =======
plot_mapa_fallecidas_anio(
  anio_sel = 2018,
  etiqueta = "id",          # "id" | "conteo" | "region_y_conteo"
  trans_scale = "identity", # "sqrt" recomendado si hay alta asimetría
  mostrar_tabla = TRUE
)













## 5) Gráfico simple: total anual
## =============================
ggplot(fallecidas_anual, aes(x = factor(AÑO), y = Total_Fallecidas)) +
  geom_col(fill = "cyan3", width = 0.7) +
  geom_text(aes(label = comma(Total_Fallecidas)), vjust = -0.3, size = 4, color = "#2E2E2E") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Número total de muertes por año", x = "Año", y = "Frecuencia Absoluta") +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

## 6) MAPA CHILE: suma 2011–2018 de fallecidas por región
## =========================================================

# Correspondencia (la tuya)
correspondencia <- data.frame(
  NOMBRE_LARGO = c("De Aisén del Gral. C. Ibáñez del Campo", "De Antofagasta", "De Arica y Parinacota",
                   "De Atacama", "De Coquimbo", "De La Araucanía", "De Los Lagos", "De Los Ríos",
                   "De Magallanes y de La Antártica Chilena", "De Tarapacá", "De Valparaíso", "De Ñuble",
                   "Del Bíobío", "Del Libertador B. O'Higgins", "Del Maule", "Metropolitana de Santiago"),
  NOMBRE_CORTO = c("Aysén del General Ibañez del Cam", "Antofagasta", "Arica y Parinacota", "Atacama",
                   "Coquimbo", "Araucanía", "Los Lagos", "Los Ríos", "Magallanes y Antárctica Chilena",
                   "Tarapacá", "Valparaíso", "Ñuble", "Bío-Bío", "Libertador General Bernardo O'Hi",
                   "Maule", "Santiago Metropolitan")
)

# Normalizador robusto para empatar strings
norm_nom <- function(x) {
  x %>%
    str_squish() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper()
}

# 6.1 Fallecidas totales 2011–2018 por región (desde DEF)
fallecidas_region_total <- defunciones %>%
  filter(AÑO %in% AÑOS) %>%
  mutate(NOMBRE_REGION_NORM = norm_nom(NOMBRE_REGION)) %>%
  count(NOMBRE_REGION_NORM, name = "Fallecidas_2011_2018") %>%
  left_join(
    correspondencia %>%
      transmute(NOMBRE_REGION_NORM = norm_nom(NOMBRE_LARGO),
                NOMBRE_CORTO = NOMBRE_CORTO),
    by = "NOMBRE_REGION_NORM"
  )
centros_total <- mapa_total %>%
  st_point_on_surface() %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  rename(LON = X, LAT = Y) %>%
  mutate(
    label = paste0(NOMBRE_CORTO, "\n", comma(Fallecidas_2011_2018))
  )

ggplot(mapa_total) +
  geom_sf(aes(fill = Fallecidas_2011_2018), color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(name = "N° fallecidas (2011–2018)", option = "D", labels = comma) +
  geom_label_repel(
    data = centros_total,
    aes(x = LON, y = LAT, label = label),
    size = 2.6, fill = "white", label.size = 0.2,
    segment.color = "grey35", segment.size = 0.25,
    max.overlaps = Inf, seed = 123
  ) +
  labs(title = "Chile: suma de fallecidas por región (2011–2018)",
       caption = "Fuente: DEIS — Elaboración propia") +
  coord_sf() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )



# 1) Fuerza el NOMBRE_CORTO a ser el del shapefile (exacto) para Magallanes
fallecidas_region_total2 <- fallecidas_region_total2 %>%
  mutate(
    NOMBRE_CORTO = if_else(
      NOMBRE_REGION_NORM == "DE MAGALLANES Y DE LA ANTARTICA CHILENA",
      "Magallanes y Antártica Chilena",   # exacto como el shapefile
      NOMBRE_CORTO
    ),
    NOMBRE_CORTO_NORM = norm_nom(NOMBRE_CORTO)
  )

# 2) Rehacer el mapa
mapa_total <- regiones_base %>%
  left_join(
    fallecidas_region_total2 %>% select(NOMBRE_CORTO_NORM, Fallecidas_2011_2018),
    by = "NOMBRE_CORTO_NORM"
  ) %>%
  mutate(Fallecidas_2011_2018 = coalesce(Fallecidas_2011_2018, 0L))

# 3) Check
anti_join(
  fallecidas_region_total2 %>% filter(!is.na(NOMBRE_CORTO_NORM)),
  regiones_base %>% st_drop_geometry() %>% select(NOMBRE_CORTO_NORM),
  by = "NOMBRE_CORTO_NORM"
)


###############GRAFICO SOLO CHILE #################
etiquetas_personalizadas <- mapa_centroides %>%
  mutate(
    x_nuevo = LONGITUD + ifelse(LONGITUD > mean(LONGITUD), 1.5, -1.5),  # desplazamiento horizontal
    y_nuevo = LATITUD
  )

mapa_data <- mapa_data %>%
  mutate(NOMBRE_REGION = factor(NOMBRE_REGION, 
                                levels = mapa_data %>%
                                  st_centroid() %>%
                                  st_coordinates() %>%
                                  as_tibble() %>%
                                  mutate(NOMBRE_REGION = mapa_data$NOMBRE_REGION) %>%
                                  arrange(desc(Y)) %>%
                                  pull(NOMBRE_REGION)
  ))



# Crear paleta extendida para 16 regiones
colores_paired <- c(brewer.pal(8, "Paired"), brewer.pal(8, "Dark2")[1:8])

ggplot() +
  geom_sf(data = mapa_data, aes(fill = NOMBRE_REGION), color = "white", size = 0.3) +
  scale_fill_manual(values = colores_paired) +
  labs(
    title = "Mapa de Chile por Regiones",
    fill = "Región"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )



