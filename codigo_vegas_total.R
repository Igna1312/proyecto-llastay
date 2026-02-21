# ==============================================================================
# PROYECTO LLASTAY: FENOLOGÍA HISTÓRICA COMPLETA VEGAS ALTOANDINAS (1984-2025)
# ==============================================================================

library(terra)
library(npphen)

# 1. Definir el directorio del proyecto
directorio_datos <- "C:/Users/diego/Documents/proyecto-llastay/geomatica"
setwd(directorio_datos)

# ==============================================================================
# 2. COMPRESIÓN DE LA PARTE 2 (Si aún no la has corrido)
# ==============================================================================
print("Iniciando compresión de la Parte 2 (2006-2025)...")

# Encontrar los archivos TIF de la Parte 2
archivos_p2 <- list.files(directorio_datos, pattern = "Parte2.*\\.tif$", full.names = TRUE)

# Crear mosaico virtual y escribir el archivo comprimido
vrt_p2 <- vrt(archivos_p2, "temp_p2.vrt", overwrite = TRUE)
writeRaster(
  vrt_p2,
  filename = "NDVI_Parte2_2006_2025_Comprimido.tif",
  filetype = "GTiff",
  datatype = "INT2S", 
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=YES", "TILED=YES"),
  overwrite = TRUE
)
print("Parte 2 comprimida con éxito.")

# ==============================================================================
# 3. INTEGRACIÓN DE LA SERIE TEMPORAL COMPLETA (1984-2025)
# ==============================================================================
print("Integrando Parte 1 y Parte 2...")

# Cargar ambos archivos físicos ya comprimidos
ndvi_p1 <- rast("NDVI_Parte1_1984_2005_Comprimido.tif")
ndvi_p2 <- rast("NDVI_Parte2_2006_2025_Comprimido.tif")

# UNIR ambas series temporales usando la función 'c' (concatenar capas)
ndvi_total <- c(ndvi_p1, ndvi_p2)
# ==============================================================================
# ==============================================================================
# 3.5 CARGA DE FECHAS EXACTAS (Sincronizadas con GEE)
# ==============================================================================
print("Cargando las 477 fechas exactas...")

# Cargar el nuevo CSV
fechas_df <- read.csv("/Users/diego/Documents/proyecto-llastay/geomatica/Fechas_Exactas_477_Meses.csv")

# Los nombres vienen como "1984_01", así que les agregamos el día "_01" 
# para que la función as.Date los reconozca correctamente como fechas
fechas_completas <- as.Date(paste0(fechas_df$date, "_01"), format = "%Y_%m_%d")

# Validación de seguridad estricta
if(nlyr(ndvi_total) != length(fechas_completas)) {
  stop(paste("¡Fallo de sincronía! El raster tiene", nlyr(ndvi_total), 
             "capas y el CSV tiene", length(fechas_completas), "fechas."))
} else {
  print("¡Sincronía perfecta! Las 477 capas y fechas coinciden.")
}


# ==============================================================================
# 4. MÁSCARA DE PERSISTENCIA Y CORRECCIÓN NIVAL (PARA LOS 42 AÑOS)
# ==============================================================================
print("Calculando persistencia histórica (1984-2025) y corrigiendo etapa nival...")

# Máscara histórica: Vega presente al menos el 50% del tiempo total
umbral_vega <- 2000 
frecuencia_total <- app(ndvi_total > umbral_vega, fun = sum, cores = detectCores() - 1)
persistencia_total <- frecuencia_total / nlyr(ndvi_total)
mascara_total <- ifel(persistencia_total > 0.5, 1, NA) 

# Extraemos solo los píxeles de vega
ndvi_vegas_total <- mask(ndvi_total, mascara_total)

# CORRECCIÓN NIVAL: Los NAs dentro de la vega asumen valor 0 (nieve/nubes de invierno)
ndvi_vegas_con_nieve <- ifel(is.na(ndvi_total) & mascara_total == 1, 0, ndvi_vegas_total)

# Escalamos a NDVI normalizado (0 a 1) para npphen
ndvi_final <- ndvi_vegas_con_nieve / 10000
# ==============================================================================
# 5. EXPORTACIÓN DEL STACK FINAL (CORREGIDO Y ESCALADO)
# ==============================================================================
print("Iniciando la exportación del stack final...")

# 1. Buena práctica: Incrustar las fechas en la metadata del Raster
names(ndvi_final) <- as.character(fechas_completas)
time(ndvi_final) <- fechas_completas

# 2. Escribir el archivo físico definitivo
# Usamos FLT4S (Float32) porque ahora los valores de NDVI tienen decimales (0 a 1)
# Usamos PREDICTOR=3 que es el algoritmo de compresión óptimo para decimales
writeRaster(
  ndvi_final,
  filename = "NDVI_Vegas_Final_1984_2025.tif",
  filetype = "GTiff",
  datatype = "FLT4S", 
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "BIGTIFF=YES", "TILED=YES"),
  overwrite = TRUE
)

print("¡Exportación completada! El archivo NDVI_Vegas_Final_1984_2025.tif está listo.")

# ==============================================================================
# 6. ANÁLISIS GLOBAL Y NPPHEN
# ==============================================================================
print("Generando perfiles fenológicos globales...")

# Calcular el promedio espacial para cada fecha
promedios_corregidos <- global(ndvi_final, fun="mean", na.rm=TRUE)
ndvi_medio_corregido <- as.numeric(promedios_corregidos$mean)

# 5.1 Serie Temporal Completa (Línea)
plot(fechas_completas, ndvi_medio_corregido, 
     type = "l", col = "blue", lwd = 1.2,
     main = "Serie Temporal Completa 1984-2025 (Con Etapa Nival Forzada)",
     ylab = "NDVI Promedio", xlab = "Año")

# 5.2 PhenKplot: Densidad fenológica con el paquete npphen
PhenKplot(
  x = ndvi_medio_corregido,
  dates = fechas_completas,
  h = 2,           # h=2 ajusta el año al Hemisferio Sur
  rge = c(0, 1),   # Rango estricto del índice
  ylab = "NDVI Promedio",
  xlab = "Dia del anio (DOY)"
)
title(main = "Fenología Histórica Vegas Precordillera (1984-2025)")

# (Opcional) Guardar la serie corregida por si la necesitas exportar a Excel
# write.csv(data.frame(Fecha = fechas_completas, NDVI = ndvi_medio_corregido), 
#           "Serie_Temporal_Corregida_1984_2025.csv", row.names = FALSE)