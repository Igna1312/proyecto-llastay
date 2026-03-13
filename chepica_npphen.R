library(npphen)
library(terra)
library(readr)
library(tidyr)
library(raster)

# 1. CARGAR DATOS
ndvi_stack <- rast("data/NDVI__Final_Chepica_1984_2025.tif")
fechas_df <- read_csv("data/Landsat_NDVI_Dates.csv")
fechas_vector <- as.Date(fechas_df$date)

# 2. LIMPIEZA RÁPIDA (Clamp)
# Esto le dice a R: "Cualquier valor menor a 0, conviértelo en 0".
# values=FALSE hace que no cargue todo a la RAM, sino que lo prepare virtualmente.
ndvi_limpio <- clamp(ndvi_stack, lower=0, values=FALSE)

# 3. EXTRAER VALORES CRUDOS (Enteros)
# Importante: NO dividimos por 10000. Mantenemos el número grande (ej: 8500)
valores_tabla <- terra::extract(ndvi_chepica)
serie_enteros <- as.numeric(ndvi_chepica[1,-1])

# 4. CALCULAR EL PROMEDIO DE TODA LA ZONA
# La función 'global' resume cada capa del mapa en un solo número (el promedio).
# na.rm = TRUE es vital para ignorar las nubes/bordes.

tabla_promedios <- global(ndvi_limpio, fun = "mean", na.rm = TRUE)

# Convertimos esa tabla a un vector simple de números
vector_promedio_zonal <- as.numeric(tabla_promedios$mean)

# 5. GRAFICAR EL PROMEDIO DE LA ZONA (esto incluye zonas áridas, por lo que no representa la productividad de vegas)
PhenKplot(
  x = vector_promedio_zonal,   # Usamos el vector promedio
  dates = fechas_vector,
  h = 2,
  xlab = "Día del ciclo fenológico",
  ylab = "NDVI Promedio Regional",
  rge = c(0, 10000)            # Mantenemos la escala
)

