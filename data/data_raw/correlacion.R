#Correlacionar precipitacion con salud de bofedales
library(npphen)
library(terra)
library(readr)
library(tidyr)
library(raster)
#1. Cargar datos
df_fechas <- read.csv("/Users/ignacio/Desktop/proyecto-llastay/data/data_procesada/Fechas_Limpias_Vegas_1984_2025.csv")
fechas_completas <- as.Date(df_fechas$Fecha)
ndvi_chepica <- rast("/Users/ignacio/Desktop/proyecto-llastay/data/data_procesada/Phen_Unificado_Superior_Matorral.tif")
ndvi_altoandino <- rast("/Users/ignacio/Desktop/proyecto-llastay/data/data_raw/Phen_altoandino.tif")

nlyr(ndvi_chepica)
global(ndvi_chepica, fun="notNA")

#2. Para graficar específicamente las capas de la 17 a la 22
plot(ndvi_chepica[[17:22]])


library(terra)

# el boxplot genera un patron similar al PhenKplot
boxplot(ndvi_altoandino, 
        main = "Ciclo Fenológico Anual (Perfil de 23 períodos)",
        ylab = "NDVI", 
        col = "palegreen", 
        las = 2,           # Gira los nombres de abajo para que no choquen
        cex.axis = 0.7)    # Achica un poco la letra


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

cat("1. Extrayendo píxeles a tabla...\n")
# Esto convierte tus 23 capas en 23 columnas de números
df_anual <- as.data.frame(ndvi_altoandino, na.rm = TRUE)

cat("2. Pivotando los datos al formato largo...\n")
# Transformamos la tabla para que ggplot la entienda
df_largo <- pivot_longer(df_anual, 
                         cols = everything(), 
                         names_to = "Periodo_16_Dias", 
                         values_to = "NDVI")

# Truco vital: Fijamos el orden de las capas para que R no las ordene alfabéticamente (Capa_1, Capa_10, Capa_11...)
df_largo$Periodo_16_Dias <- factor(df_largo$Periodo_16_Dias, levels = unique(df_largo$Periodo_16_Dias))

cat("3. Generando el Boxplot...\n")
# Dibujamos el gráfico con la estética clásica y limpia
ggplot(df_largo, aes(x = Periodo_16_Dias, y = NDVI)) +
  geom_boxplot(fill = "olivedrab3", color = "darkolivegreen", 
               alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
  labs(title = "Variabilidad Espacial del NDVI a lo largo del Año",
       subtitle = "Distribución de valores por cada período de 16 días",
       x = "Período (Capa)",
       y = "NDVI (0 a 10000)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(terra)
library(tidyr)
library(ggplot2)
library(dplyr) # Añadimos dplyr para limpiar los datos más fácil

cat("1. Extrayendo la tabla bruta...\n")
# Extraemos TODO el mapa sin borrar absolutamente nada
df_bruto <- as.data.frame(ndvi_altoandino, na.rm = FALSE)

cat("2. Transformando a formato largo directamente...\n")
# Pasamos las 23 columnas a una sola columna larguísima
df_largo <- pivot_longer(df_bruto, 
                         cols = everything(), 
                         names_to = "Periodo", 
                         values_to = "NDVI")

cat("3. Eliminando la nieve y roca (NAs)...\n")
# Esta es la forma más segura: borramos solo las celdas individuales que son NA
df_limpio <- df_largo %>% drop_na(NDVI)

# Filtramos también posibles errores del satélite (valores negativos o absurdos)
df_limpio <- df_limpio %>% filter(NDVI >= 0 & NDVI <= 10000)

cat("¡Datos rescatados! Hay", nrow(df_limpio), "observaciones válidas.\n")

# Diagnóstico de seguridad:
if(nrow(df_limpio) == 0) {
  stop("Houston, tenemos un problema: R sigue sin ver los números. El raster podría estar corrupto en la memoria.")
}

cat("4. Dibujando el Boxplot definitivo...\n")
# Ordenamos las capas para que no se desordene el eje X
df_limpio$Periodo <- factor(df_limpio$Periodo, levels = unique(df_bruto %>% names()))

# Generamos el gráfico
ggplot(df_limpio, aes(x = Periodo, y = NDVI)) +
  geom_boxplot(fill = "olivedrab3", color = "darkolivegreen", 
               alpha = 0.7, outlier.size = 0.1, outlier.alpha = 0.1) +
  labs(title = "Dinámica Fenológica del Piso Altoandino",
       subtitle = paste("Análisis basado en", nrow(df_limpio), "mediciones válidas"),
       x = "Período (Capa 1 a 23)",
       y = "NDVI (Escala 10.000)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
