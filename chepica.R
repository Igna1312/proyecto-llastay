# ==============================================================================
# PROYECTO LLASTAY: FENOLOGÍA HISTÓRICA COMPLETA VEGAS ALTOANDINAS (1984-2025)
# ==============================================================================

library(terra)
library(npphen)
library(dplyr)
# 1. Definir el directorio del proyecto
directorio_datos <- "C:/Users/andre/Documents/proyecto-llastay/data"
setwd(directorio_datos)

# ==============================================================================
# 2. COMPRESIÓN DE LA PARTE 2 (Si aún no la has corrido)
# ==============================================================================

ndvi_total <- rast("C:/Users/andre/Documents/proyecto-llastay/data/NDVI_Vegas_Total_1984_2025.tif")

# 1. Cargamos el nuevo CSV blindado
df_fechas <- read.csv("Fechas_Limpias_Vegas_1984_2025.csv")

# 2. Como ya vienen en formato correcto desde GEE, R las lee de inmediato
fechas_completas <- as.Date(df_fechas$Fecha)

# Verificamos que ya no hay NAs
print(head(fechas_completas))

# Validación de seguridad estricta
if(nlyr(ndvi_final) != length(fechas_completas)) {
  stop(paste("¡Fallo de sincronía! El raster tiene", nlyr(ndvi_total), 
             "capas y el CSV tiene", length(fechas_completas), "fechas."))
} else {
  print("¡Sincronía perfecta! Las 477 capas y fechas coinciden.")
}

# ==============================================================================
# 4. MÁSCARA DE PERSISTENCIA Y CORRECCIÓN NIVAL (PARA LOS 42 AÑOS)
# ==============================================================================

# Máscara histórica: Vega presente al menos el 50% del tiempo total
umbral_vega <- 2000 
frecuencia_total <- app(ndvi_total > umbral_vega, fun = sum, cores = detectCores() - 1)
persistencia_total <- frecuencia_total / nlyr(ndvi_total)
mascara_total <- ifel(persistencia_total > 0.5, 1, NA) 

# Extraemos solo los píxeles de vega
ndvi_vegas_total <- mask(ndvi_total, mascara_total)

# CORRECCIÓN NIVAL: Los NAs dentro de la vega asumen valor 0 (nieve/nubes de invierno)
ndvi_final <- ifel(is.na(ndvi_total) & mascara_total == 1, 0, ndvi_vegas_total)


# ==============================================================================
# 5. EXPORTACIÓN DEL STACK FINAL (ESCALA 10.000)
# ==============================================================================

# 1. Incrustar las fechas en la metadata del Raster
names(ndvi_final) <- as.character(fechas_completas)
time(ndvi_final) <- fechas_completas

# 2. Exportar el archivo físico usando INT2S
# Usamos INT2S (Enteros de 16 bits) que es súper ligero
# Usamos PREDICTOR=2 que es el algoritmo de compresión óptimo para enteros
writeRaster(
  ndvi_final,
  filename = "NDVI__Final_Chepica_1984_2025.tif",
  filetype = "GTiff",
  datatype = "INT2S",  # <--- LA CLAVE: Guarda los números grandes de forma eficiente
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=YES", "TILED=YES", "NUM_THREADS=ALL_CPUS"),
  overwrite = TRUE
)


# 1. Exportacion como CSV
#Calculamos el promedio espacial de toda la vega para cada mes
promedios <- global(ndvi_final, fun = "mean", na.rm = TRUE)

# 2. Extraemos solo los números (que estarán en la escala original de 0 a 10000)
ndvi_medio_vector <- as.numeric(promedios$mean)

# 3. Ahora sí, creamos la tabla emparejando 463 fechas con 463 promedios
tabla_exportar <- data.frame(
  Fecha = fechas_completas, 
  NDVI_Promedio = ndvi_medio_vector
)

# 4. Guardamos el CSV en el disco duro
write.csv(tabla_exportar, "Serie_Temporal_Chepica_1984_2025.csv", row.names = FALSE)

#Calcular el máximo histórico por píxel

# 1. Calculamos el máximo a través de todas las capas (los 477 meses)
# na.rm = TRUE asegura que ignore las nubes o meses sin datos
ndvi_maximo_historico <- max(ndvi_final, na.rm = TRUE)

# 2. Le damos un vistazo rápido en R
plot(ndvi_maximo_historico, main = "Máximo Histórico de NDVI (1984-2025)", col = terrain.colors(100))

# 3. Exportamos el archivo para que lo puedas abrir en ArcGIS Pro
writeRaster(
  ndvi_maximo_historico,
  filename = "NDVI_Maximo_Historico_1984_2025.tif",
  filetype = "GTiff",
  datatype = "INT2S", # Mantenemos el formato ligero
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"),
  overwrite = TRUE
)

print("¡Mapa de máximos exportado exitosamente!")

# ==============================================================================
# 6. Patrón Fenológico Global (PhenKplot)
# ==============================================================================

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
  s = ndvi_final,
  dates = fechas_completas,
  h = 2,
  frequency = "monthly",
  rge = c(0, 10000),           # <-- EL SECRETO: Rango físico real
  nCluster = 2,
  outname = "Resultados_PhenMap_1984_2025.tif",
  datatype = "INT2S"           # <-- Ahorramos espacio en disco
)

# ==============================================================================
# 7. ANÁLISIS DE ANOMALÍAS Y EXTREMOS CLIMÁTICOS
# ==============================================================================

#Calculando anomalías para la megasequía de 2019-2020...")
indice_anomalia <- which(fechas_completas >= as.Date("2019-07-01") & fechas_completas <= as.Date("2020-06-01"))
indice_referencia <- 1:length(fechas_completas)

ExtremeAnoMap(
  s = ndvi_final,
  dates = fechas_completas,
  h = 2,
  refp = indice_referencia,    
  anop = indice_anomalia,      
  rge = c(0, 10000),           # <-- Rango físico real
  nCluster = 2,
  outname = "Resultados_Anomalia_2019_2020.tif",
  datatype = "INT2S"           # <-- Soporta anomalías negativas sin problema
)

#Calculo de anomalias general

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
