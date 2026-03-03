# Cargar librerías (Añadimos purrr y stringr)
library(terra)   # Hace el 90% del trabajo: lee el TIF, lee el SHP, recorta, proyecta y promedia.
library(npphen)  # Calcula la fenología y hace los gráficos (PhenKplot y PhenMap).
library(purrr)   # Nos da la función walk() para automatizar el ciclo de los 3 pisos.
library(stringr) # Nos da str_to_title() para poner las mayúsculas bonitas en los títulos.

# ==============================================================================
# 1. PREPARACIÓN GENERAL (Se ejecuta SOLO UNA VEZ)
# ==============================================================================
directorio_datos <- "C:/Users/andre/Documents/proyecto-llastay/data"
setwd(directorio_datos)

cat("Cargando y corrigiendo raster base...\n")
ndvi_chepica <- rast("NDVI__Final_Chepica_1984_2025.tif")

# Corrección de coordenadas (Eliminé rotate() porque shift() es la solución que te funcionó)
ndvi_corregido <- shift(ndvi_chepica, dx = -360)
crs(ndvi_corregido) <- "EPSG:4326"

cat("Cargando fechas...\n")
df_fechas <- read.csv("Fechas_Limpias_Vegas_1984_2025.csv")
fechas_completas <- as.Date(df_fechas$Fecha)

# ==============================================================================
# 2. DEFINIR LA FUNCIÓN DE PROCESAMIENTO
# ==============================================================================
# Esta función hará todo el trabajo sucio para el piso que le pasemos
procesar_piso_vegetacional <- function(nombre_piso) {
  
  cat("\n======================================================\n")
  cat(" INICIANDO PROCESAMIENTO PARA PISO:", toupper(nombre_piso), "\n")
  cat("======================================================\n")
  
  # 1. Configurar nombres de archivos dinámicos
  archivo_shp <- paste0("piso_", nombre_piso, ".shp")
  ruta_guardado_tif <- paste0("Raster_Mask_", str_to_title(nombre_piso), ".tif")
  nombre_salida_phenmap <- paste0("Resultados_PhenMap_", str_to_title(nombre_piso), "_1984_2025.tif")
  
  # 2. Cargar el polígono específico
  poligono <- vect(archivo_shp)
  crs(poligono) <- "EPSG:4326"
  
  # 3. Generar máscara del raster
  cat("Recortando raster...\n")
  raster_mask <- crop(ndvi_corregido, poligono, mask = TRUE)
  
  # Validación de seguridad estricta
  if(nlyr(raster_mask) != length(fechas_completas)) {
    stop(paste("¡Fallo de sincronía en", nombre_piso, "! El raster tiene", nlyr(raster_mask), 
               "capas y el CSV tiene", length(fechas_completas), "fechas."))
  } else {
    cat("¡Sincronía perfecta confirmada!\n")
  }
  
  # 4. Comprobación visual (EDA)
  # Cambié poligono_proj por poligono (poligono_proj ya no existe en tu nuevo flujo)
  plot(raster_mask[[1]], main = paste("Vega - Piso", str_to_title(nombre_piso), "(Recortada)"))
  plot(poligono, add = TRUE, border = "black", lwd = 0.5)
  
  r.mean <- mean(raster_mask, na.rm = TRUE)
  plot(r.mean, main = paste("NDVI Promedio Histórico -", str_to_title(nombre_piso)))
  
  r.mean2018 <- mean(raster_mask[[grep("2018", fechas_completas)]], na.rm = TRUE)
  plot(r.mean2018, main = paste("NDVI Promedio 2018 -", str_to_title(nombre_piso)))
  
  # 5. Patrón Fenológico Global (PhenKplot)
  cat("Calculando PhenKplot...\n")
  promedios <- global(raster_mask, fun = "mean", na.rm = TRUE)
  ndvi_medio <- as.numeric(promedios$mean)
  max_ndvi <- max(ndvi_medio, na.rm = TRUE)
  
  PhenKplot(
    x = ndvi_medio,
    dates = fechas_completas,
    h = 2,           
    rge = c(0, max_ndvi + 0.05),
    ylab = "NDVI Promedio",
    xlab = "Día del año (DOY - Julio a Junio)"
  )
  title(main = paste("Curva Fenológica - Piso", str_to_title(nombre_piso), "(1984-2025)"))
  
  # 6. Guardar el raster físico para evitar errores de memoria en PhenMap
  cat("Guardando raster sólido en disco...\n")
  writeRaster(raster_mask, ruta_guardado_tif,  overwrite = TRUE)
  raster_solido <- rast(ruta_guardado_tif)
  
  # 7. Ejecutar PhenMap
  cat("Ejecutando PhenMap (esto puede tomar un rato)...\n")
  PhenMap(
    s = raster_solido,
    dates = fechas_completas,
    h = 2,
    frequency = "monthly",
    rge = c(0, 10000),
    nCluster = 1,  # Sugiero mantenerlo en 1 para evitar tu error previo "serialize() error"
    outname = nombre_salida_phenmap,
    datatype = "INT2S"
  )
  
  cat(">>> PROCESO COMPLETADO EXITOSAMENTE PARA:", toupper(nombre_piso), "<<<\n")
}

# ==============================================================================
# 3. EJECUTAR LA ITERACIÓN CON PURRR
# ==============================================================================
# Definimos los nombres de los pisos (deben coincidir con la parte final de tu .shp)
lista_pisos <- c("superior", "matorral", "bosque")

# walk() es primo de map(), pero se usa cuando NO quieres devolver una tabla, 
# sino que quieres que haga acciones (como guardar archivos y hacer plots)
purrr::walk(lista_pisos, procesar_piso_vegetacional)

cat("\n¡TODOS LOS PISOS VEGETACIONALES HAN SIDO PROCESADOS!\n")

# Generar mismos procesos para el piso inferior

poligono_inferior <- vect("C:/Users/andre/Documents/proyecto-llastay/data/poligono_inferior.shp")

# 3. Confirmar que ambos hablen oficialmente el idioma WGS84 (Lat/Lon)
crs(ndvi_corregido) <- "EPSG:4326"
crs(poligono_inferior) <- "EPSG:4326"

#2. Generar máscara del raster
zona_inferior <- crop(ndvi_corregido, poligono_inferior, mask = TRUE)
# Validación de seguridad estricta
if(nlyr(zona_inferior) != length(fechas_completas)) {
  stop(paste("¡Fallo de sincronía! El raster tiene", nlyr(zona_inferior), 
             "capas y el CSV tiene", length(fechas_completas), "fechas."))
} else {
  print("¡Sincronía perfecta! Las 477 capas y fechas coinciden.")
}
# 5. Comprobación visual (Dibuja la primera capa para confirmar)
plot(zona_inferior[[1]], main = "Vega inferior con Periodo Nival (Recortada)")
plot(poligono_proj, add = TRUE, border = "black", lwd = 0.5)

# EDA de vegas nivales
r.mean <- mean(raster_mask, na.rm = TRUE)
plot(r.mean)

#Tambien se puede ver la media de algun periodo en específico
r.mean2018 <- mean(raster_mask [[grep("2018", fechas_completas)]])
plot(r.mean2018, main = "Vega con Periodo Nival (2018)")


# 6. Patrón Fenológico Global (PhenKplot)

#Calculamos el promedio espacial de toda la vega para cada mes
promedios <- global(raster_mask, fun = "mean", na.rm = TRUE)
ndvi_medio <- as.numeric(promedios$mean)

# Dibujamos la curva de densidad (Sin tildes para evitar advertencias)
max_ndvi <- max(ndvi_medio, na.rm = TRUE)
PhenKplot(
  x = ndvi_medio,
  dates = fechas_completas,
  h = 2,           
  rge = c(0, max_ndvi + 0.05), # Zoom dinámico al ecosistema real
  ylab = "NDVI Promedio",
  xlab = "Dia del año (DOY - Julio a Junio)"
)
title(main = "Curva Fenologica de Vegas Nivales (1984-2025)")

# Guardamos el mapa recortado físicamente en tu carpeta
ruta_guardado <- "C:/Users/andre/Documents/proyecto-llastay/data/Raster_Mask_Superior.tif"
writeRaster(raster_mask, ruta_guardado, overwrite = TRUE)
raster_superior <- rast("C:/Users/andre/Documents/proyecto-llastay/data/Raster_Mask_Superior.tif")

# Generamos PhenMap
PhenMap(
  s = raster_superior,
  dates = fechas_completas,
  h = 2,
  frequency = "monthly",
  rge = c(0, 10000),
  nCluster = 2,         
  outname = "Resultados_PhenMap_1984_2025.tif",
  datatype = "INT2S"
)

phen.map <- rast("Resultados_PhenMap_1984_2025.tif") 
plot(phen.map)
print(phen.map)
