library(dplyr)
library(readr)
library(SPEI)
library(terra)


# 1. CARGAR DATOS
# Cargar los polígonos de tus vegas/bofedales
bofedales <- vect("C:/proyectos/proyecto-llastay/shapefiles/poligono.shp")

# DEFINIR RUTAS (Ruta corregida: doble L en llastay)
ruta_carpeta_pr <- "C:/proyectos/proyecto-llastay/data/data_procesada"
# ====================================================================
# 1. CREACIÓN DEL CLIMA_DF DESDE TUS ARCHIVOS CAMELS-CL
# ====================================================================

# Cargar los datos mensuales que adjuntaste
precip <- read_csv("C:/proyectos/proyecto-llastay/data/data_procesada/precip_cr2met_mon.csv")
pet <- read_csv("C:/proyectos/proyecto-llastay/data/data_procesada/pet_hargreaves_mon.csv")

# Unir ambas tablas asegurando el orden cronológico
# Seleccionamos el año, mes y la columna de tu cuenca (4535002)
clima_df <- precip %>%
  select(year, month, P_mm = `4535002`) %>%
  inner_join(
    pet %>% 
      # AQUÍ ESTÁ LA SOLUCIÓN: Convertir texto a número
      mutate(month = as.numeric(month)) %>% 
      select(year, month, PET_mm = `4535002`), 
    by = c("year", "month")
  ) %>%
  arrange(year, month) # Fundamental para que la serie de tiempo no se mezcle
# ====================================================================
# 2. CÁLCULO DEL SPEI A MÚLTIPLES ESCALAS
# ====================================================================

# Paso A: Calcular el Balance Hídrico (Precipitación - Evapotranspiración)
clima_df$Balance <- clima_df$P_mm - clima_df$PET_mm

# Paso B: Transformar la columna de balance en un objeto de serie de tiempo (ts)
# Según tus CSV, los datos parten en enero de 1979
ts_balance <- ts(clima_df$Balance, start = c(min(clima_df$year), 1), frequency = 12)

# Paso C: Calcular los índices SPEI (Esto tomará menos de 1 segundo)
clima_df$SPEI_3  <- spei(ts_balance, scale = 3)$fitted
clima_df$SPEI_6  <- spei(ts_balance, scale = 6)$fitted
clima_df$SPEI_12 <- spei(ts_balance, scale = 12)$fitted
clima_df$SPEI_24 <- spei(ts_balance, scale = 24)$fitted

# ====================================================================
# 3. FILTRADO TEMPORAL PARA CORRELACIÓN (Método Chávez et al. 2023)
# ====================================================================

# Extraemos solo el mes de Marzo, que encapsula el final de la temporada de crecimiento
spei_marzo <- clima_df %>% filter(month == 3)

# Vemos cómo quedó tu tabla final lista para el join con el NDVI
head(spei_marzo)

# ====================================================================
# 1. EXTRACCIÓN DEL NDVI MENSUAL DE LOS POLÍGONOS
# ====================================================================
# Asumimos que 'bofedales_proj' (tus polígonos) y 'spei_marzo' (tu tabla) 
# ya están cargados en el entorno por tu script anterior.

ndvi_stack <- rast("C:/proyectos/proyecto-llastay/data/data_raw/Raster_Mask_Superior.tif")
library(terra)

# 2. Cargar tus polígonos originales
bofedales <- vect("C:/proyectos/proyecto-llastay/shapefiles/poligono.shp")

# 3. CREAR 'bofedales_proj' proyectando el shapefile para que tenga las 
# mismas coordenadas exactas que tu raster de NDVI
bofedales_proj <- project(bofedales, crs(ndvi_stack))

# 4. Ahora sí, ejecutar la extracción (ya no dará el error de objeto no encontrado)

ndvi_extraccion <- terra::extract(ndvi_stack, bofedales_proj, fun = mean, na.rm = TRUE)
# Extraemos el promedio de NDVI de los píxeles dentro de cada polígono para cada mes

# ====================================================================
# 2. TRANSFORMACIÓN Y CÁLCULO DEL NDVI MÁXIMO (NOV-ENE)
# ====================================================================
# Transformamos la tabla a formato largo para poder filtrar por mes.
# OJO: Deberás ajustar los números en substr() dependiendo de cómo se llamen 
# exactamente las capas en tu Raster_Mask_Superior.tif (Ej: "NDVI_1984_11")

ndvi_largo <- ndvi_extraccion %>%
  pivot_longer(cols = -ID, names_to = "Capa", values_to = "NDVI") %>%
  mutate(
    # Extrae el año y mes del nombre de la capa. Modifica los índices si tu nombre es distinto.
    # Asume formato estándar donde los últimos 7 caracteres son "YYYY_MM"
    Year = as.numeric(substr(Capa, nchar(Capa)-6, nchar(Capa)-3)),
    Month = as.numeric(substr(Capa, nchar(Capa)-1, nchar(Capa)))
  )

# Filtramos los meses de verano y creamos el "Año Fenológico"
ndvi_verano <- ndvi_largo %>%
  filter(Month %in% c(11, 12, 1)) %>%
  # Si el mes es noviembre o diciembre (ej. Nov 1984), lo asignamos al año fenológico siguiente (1985)
  # para que coincida con el Enero y el SPEI de Marzo de esa temporada.
  mutate(Year_Fenologico = ifelse(Month == 1, Year, Year + 1)) %>%
  group_by(ID, Year_Fenologico) %>%
  summarize(NDVI_Max = max(NDVI, na.rm = TRUE),.groups = 'drop') %>%
  rename(year = Year_Fenologico) # Lo llamamos 'year' en minúscula para el join con spei_marzo

# ====================================================================
# 3. UNIÓN CON SPEI Y CÁLCULO DE CORRELACIÓN POR BOFEDAL
# ====================================================================

# Unimos los datos de NDVI máximo con la tabla de SPEI
datos_completos <- ndvi_verano %>%
  inner_join(spei_marzo, by = "year") %>%
  filter(!is.na(NDVI_Max),!is.na(SPEI_3))

# Calculamos la correlación de Pearson para cada bofedal (agrupando por ID)
tabla_correlacion <- datos_completos %>%
  group_by(ID) %>%
  summarize(
    Correlacion_SPEI3 = cor(NDVI_Max, SPEI_3, method = "pearson"),
    .groups = 'drop'
  )

# ====================================================================
# 4. EXTRACCIÓN DE LATITUD Y PREPARACIÓN DE GRÁFICOS
# ====================================================================

# Calculamos el centroide de cada polígono y extraemos su coordenada Y (Latitud)
centroides <- centroids(bofedales_proj)
latitudes <- crds(centroides)[, "y"]

# Añadimos la latitud a nuestra tabla de correlaciones
tabla_correlacion$Latitud <- latitudes

# Añadimos la latitud a los datos completos para poder colorear por ella
datos_completos <- datos_completos %>%
  left_join(tabla_correlacion %>% select(ID, Latitud), by = "ID")

# ====================================================================
# 5. PLOTEO DE RESULTADOS (GGPLOT2)
# ====================================================================

# Gráfico 1: Perfil Latitudinal de las Correlaciones (Estilo Chávez et al. 2023)
plot_perfil <- ggplot(tabla_correlacion, aes(x = Latitud, y = Correlacion_SPEI3)) +
  geom_point(color = "darkgreen", alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() + # Coloca la latitud en el eje Y, simulando el eje Norte-Sur del mapa
  theme_minimal() +
  labs(title = "Perfil Latitudinal: Correlación NDVI Máximo vs SPEI-3",
       subtitle = "Cómo cambia la dependencia a la sequía de Norte a Sur",
       x = "Latitud (°Sur)",
       y = "Coeficiente de Correlación de Pearson (r)")

print(plot_perfil)

# Gráfico 2: Comportamiento del NDVI frente al SPEI-3 agrupado/coloreado por Latitud
plot_dispersion <- ggplot(datos_completos, aes(x = SPEI_3, y = NDVI_Max, color = Latitud)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE, size = 0.5) +
  scale_color_viridis_c(option = "plasma") + # Escala de colores que resalta el gradiente espacial
  theme_minimal() +
  labs(title = "Sensibilidad del Vigor Vegetacional (NDVI) ante la Sequía (SPEI-3)",
       x = "Índice SPEI-3 (Marzo) - Valores negativos indican sequía extrema",
       y = "NDVI Máximo (Noviembre - Enero)",
       color = "Latitud")

print(plot_dispersion)