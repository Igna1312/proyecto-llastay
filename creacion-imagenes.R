# ==============================================================================
# PROYECTO LLASTAY: ANÁLISIS ESPACIAL Y CAMBIO CLIMÁTICO (1984-2025)
# Script de Análisis Final (Mapas de Anomalías y Línea Base)
# ==============================================================================

# 1. Cargar librerías necesarias
library(terra)
library(npphen)
library(parallel)
library(readr)

# 2. Configurar el paralelismo (Solo guardamos el número de núcleos)
num_cores <- detectCores() - 1

# 3. Definir directorio
directorio_trabajo <- "C:/Users/andre/Documents/proyecto-llastay/data"

# ==============================================================================
# 4. CARGA DE DATOS Y RECONSTRUCCIÓN HÍBRIDA DE FECHAS
# ==============================================================================

# 4.1 Cargar el TIF definitivo (Viene en escala 0 a 10000 para ahorrar espacio)
ndvi_final <- rast("C:/Users/andre/Documents/proyecto-llastay/data/NDVI_Vegas_Final_1984_2025_Escala10000.tif")

# 4.2 Reconstruir las 477 fechas (Método Híbrido Infalible)
df_p1 <- read.csv("C:/Users/andre/Documents/proyecto-llastay/data/Fechas_Parte1_Crudas.csv")
df_p2 <- read.csv("C:/Users/andre/Documents/proyecto-llastay/data/Fechas_Parte2_Crudas.csv")
df_p1 <- subset(df_p1, date != "" & !is.na(date))
df_p2 <- subset(df_p2, date != "" & !is.na(date))

# Matemáticas para la Parte 1
indices_p1 <- as.numeric(as.character(df_p1$date))
calendario_p1 <- seq(as.Date("1984-01-01"), by = "month", length.out = 300)
fechas_p1 <- calendario_p1[indices_p1 + 1] 

# Textos para la Parte 2
textos_p2 <- gsub("^X", "", as.character(df_p2$date))
fechas_p2 <- as.Date(paste0(textos_p2, "_01"), format = "%Y_%m_%d")

fechas_completas <- c(fechas_p1, fechas_p2)

# 4.3 Asignar el tiempo a las capas
time(ndvi_crudo) <- fechas_completas

# ==============================================================================
# 5. VERIFICACIÓN FENOLÓGICA GLOBAL (PhenKplot)
# ==============================================================================

promedios <- global(ndvi_crudo, fun = "mean", na.rm = TRUE)
ndvi_medio <- as.numeric(promedios$mean)

# Dibujamos la curva de densidad (Sin tildes para evitar advertencias)
max_ndvi <- max(ndvi_medio, na.rm = TRUE)
PhenKplot(
  x = ndvi_medio,
  dates = fechas_completas,
  h = 2,           
  rge = c(0, max_ndvi + 0.05), # Zoom dinámico al ecosistema real
  ylab = "NDVI Promedio",
  xlab = "Dia del anio (DOY - Julio a Junio)"
)
title(main = "Curva Fenologica Historica Global (1984-2025)")

# ==============================================================================
# 6. LÍNEA BASE ESPACIAL MULTINÚCLEO (PhenMap)
# ==============================================================================
print("Calculando parámetros fenológicos espaciales (PhenMap)...")

# Le pasamos directamente el 'ndvi_crudo' y ampliamos el rango a 10000
PhenMap(
  s = ndvi_crudo,
  dates = fechas_completas,
  h = 2,
  frequency = "monthly",
  rge = c(0, 10000),           # <-- EL SECRETO: Rango físico real
  nCluster = num_cores,
  outname = "Resultados_PhenMap_1984_2025.tif",
  datatype = "INT2S"           # <-- Ahorramos espacio en disco
)
print("¡Línea base (PhenMap) guardada exitosamente!")

# ==============================================================================
# 7. ANÁLISIS DE ANOMALÍAS Y EXTREMOS CLIMÁTICOS
# ==============================================================================
print("Calculando anomalías para la megasequía de 2019-2020...")

indice_anomalia <- which(fechas_completas >= as.Date("2019-07-01") & fechas_completas <= as.Date("2020-06-01"))
indice_referencia <- 1:length(fechas_completas)

ExtremeAnoMap(
  s = ndvi_crudo,
  dates = fechas_completas,
  h = 2,
  refp = indice_referencia,    
  anop = indice_anomalia,      
  rge = c(0, 10000),           # <-- Rango físico real
  nCluster = num_cores,
  outname = "Resultados_Anomalia_2019_2020.tif",
  datatype = "INT2S"           # <-- Soporta anomalías negativas sin problema
)
print("¡Anomalía 2019-2020 guardada!")



print("Mapeando la frecuencia histórica de anomalías extremas...")

ExtremeAnoMap(
  s = ndvi_crudo,
  dates = fechas_completas,
  h = 2,
  refp = indice_referencia,
  anop = indice_referencia, 
  rge = c(0, 10000),           # <-- Rango físico real
  output = "both",
  rfd = 0.90,                  
  nCluster = num_cores,
  outname = "Resultados_Extremos_1984_2025.tif",
  datatype = "INT2S"           # <-- Perfecto para guardar recuentos numéricos
)
print("¡Mapa de Extremos guardado!")

# 1. DEFINIR COORDENADAS CORRECTAMENTE (X = Longitud, Y = Latitud)
# IMPORTANTE: Revisa si estas son tus coordenadas reales. Las ajusté a formato de grados.
lon_x <- -70.6143  
lat_y <- -31.2599

# 2. Crear el punto espacial y asegurar que tenga la misma proyección que tu mapa

punto_vec <- vect(cbind(lon_x, lat_y), crs="EPSG:4326")
punto_proj <- project(punto_vec, crs(ndvi_final))

# 3. EXTRAER LOS DATOS DEL MAPA
valores_extraidos <- terra::extract(ndvi_final, punto_proj)

# 4. LA LIMPIEZA CRÍTICA: Quitamos la columna 1 (el 'ID') y pasamos a vector numérico
serie_1 <- as.numeric(valores_extraidos[1, -1])

# Verificamos rápidamente si hay datos válidos antes de graficar
cat("Valores extraídos (primeros 5):", head(serie_1), "\n")
cat("Cantidad total de datos:", length(serie_1), "\n")

# 5. GENERAR EL GRÁFICO (Ahora con los datos limpios y las fechas correctas)
PhenKplot(
  x = serie_1, 
  dates = fechas_completas, 
  h = 2, 
  xlab = 'Días del ciclo fenológico', 
  ylab = 'NDVI (Escala x10000)', 
  rge = c(0, 10000)
)
