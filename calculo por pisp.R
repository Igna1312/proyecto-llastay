# ==============================================================================
# 1. PREPARACIÓN GENERAL (Se ejecuta SOLO UNA VEZ)
# ==============================================================================
directorio_datos <- "/Users/ignacio/Desktop/proyecto-llastay/data/data_procesada/"
setwd(directorio_datos)

cat("Cargando raster base...\n")
# Cargamos el raster original sin alterarlo con shift()
ndvi_chepica <- rast("NDVI__Final_Chepica_1984_2025.tif")

cat("Cargando fechas...\n")
df_fechas <- read.csv("Fechas_Limpias_Vegas_1984_2025.csv")
fechas_completas <- as.Date(df_fechas$Fecha)

# ==============================================================================
# 2. DEFINIR LA FUNCIÓN DE PROCESAMIENTO
# ==============================================================================
procesar_piso_vegetacional <- function(nombre_piso) {
  
  cat("\n======================================================\n")
  cat(" INICIANDO PROCESAMIENTO PARA PISO:", toupper(nombre_piso), "\n")
  cat("======================================================\n")
  
  # 1. Configurar nombres dinámicos
  if (nombre_piso == "superior") {
    archivo_shp <- "poligono.shp"
  } else {
    archivo_shp <- paste0("piso_", nombre_piso, ".shp")
  }
  
  ruta_guardado_tif <- paste0("Raster_Mask_", str_to_title(nombre_piso), ".tif")
  
  # 2. Cargar el polígono específico y reproyectar (La Regla de Oro)
  poligono <- vect(archivo_shp)
  poligono <- project(poligono, crs(ndvi_chepica))
  
  # 3. Generar máscara del raster
  cat("Recortando raster...\n")
  raster_mask <- crop(ndvi_chepica, poligono, mask = TRUE)
  
  if(nlyr(raster_mask) != length(fechas_completas)) {
    stop(paste("¡Fallo de sincronía en", nombre_piso, "! El raster tiene", nlyr(raster_mask), 
               "capas y el CSV tiene", length(fechas_completas), "fechas."))
  } else {
    cat("¡Sincronía perfecta confirmada!\n")
  }
  
  # 4. Comprobación visual (EDA) - Esto se dibuja en RStudio
  plot(raster_mask[[1]], main = paste("Vega - Piso", str_to_title(nombre_piso), "(Recortada)"))
  plot(poligono, add = TRUE, border = "black", lwd = 0.5)
  
  # Extraemos los valores promedios para los gráficos
  promedios <- global(raster_mask, fun = "mean", na.rm = TRUE)
  ndvi_medio <- as.numeric(promedios$mean)
  max_ndvi <- max(ndvi_medio, na.rm = TRUE)
  
  # 5. GUARDAR: Patrón Fenológico Global (PhenKplot)
  cat("Guardando PhenKplot...\n")
  nombre_phenkplot <- paste0("Curva_Fenologica_", str_to_title(nombre_piso), ".png")
  png(filename = nombre_phenkplot, width = 800, height = 600, res = 100)
  
  PhenKplot(
    x = ndvi_medio,
    dates = fechas_completas,
    h = 2,          
    rge = c(0, max_ndvi + 0.05),
    ylab = "NDVI Promedio",
    xlab = "Día del año (DOY - Julio a Junio)"
  )
  title(main = paste("Curva Fenológica - Piso", str_to_title(nombre_piso), "(1984-2025)"))
  dev.off()
  
  # 6. GUARDAR: Serie de Tiempo (Línea de tiempo histórica)
  cat("Guardando Línea de Tiempo de NDVI...\n")
  nombre_timeline <- paste0("Linea_Tiempo_NDVI_", str_to_title(nombre_piso), ".png")
  
  # Hacemos este gráfico un poco más ancho (1000px) porque son muchos años
  png(filename = nombre_timeline, width = 1000, height = 500, res = 100)
  
  # Dibujamos la línea de tiempo
  plot(x = fechas_completas, y = ndvi_medio, 
       type = "l",           # "l" de línea
       col = "forestgreen",  # Color verde bosque
       lwd = 1.5,            # Grosor de la línea
       main = paste("Evolución del NDVI Promedio - Piso", str_to_title(nombre_piso), "(1984-2025)"),
       xlab = "Año", 
       ylab = "NDVI Promedio",
       panel.first = grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")) # Agrega una grilla de fondo
  
  dev.off()
  cat("¡Gráficos guardados exitosamente!\n")
  
  # 7. Guardar el raster físico
  writeRaster(raster_mask, ruta_guardado_tif, overwrite = TRUE)
}

# ==============================================================================
# 3. EJECUTAR LA ITERACIÓN CON PURRR
# ==============================================================================
# Definimos los nombres de los pisos que queremos procesar
lista_pisos <- c("superior", "matorral", "bosque")

# ¡AQUÍ SUCEDE LA MAGIA!
# walk() toma la lista y le aplica a cada uno la función que acabamos de arreglar
purrr::walk(lista_pisos, procesar_piso_vegetacional)

cat("\n¡TODOS LOS PISOS VEGETACIONALES HAN SIDO PROCESADOS!\n")

# 1. Asegurarnos de que estamos usando el raster físico que guardamos en pasos anteriores
raster_matorral <- rast("Raster_Mask_Matorral.tif")

# 2. Ejecutar PhenMap limpio y sin errores de sintaxis
PhenMap(s = raster_matorral, 
        dates = fechas_completas,   
        h = 2,
        frequency = '16-days',
        nCluster = 7,
        outname = 'Phen_matorral.tif', 
        datatype = 'INT2S',
        rge = c(0, 10000)               
)

PhenMap(s = raster_andino, 
        dates = fechas_completas,   
        h = 2,
        frequency = '16-days',
        nCluster = 7,
        outname = 'Phen_altoandino.tif', 
        datatype = 'INT2S',
        rge = c(0, 10000)               
)

# 1. Cargar los PhenMaps ya calculados
# (Asegúrate de poner los nombres exactos con los que se guardaron)
phen_matorral <- rast("Phen_matorral.tif")
phen_superior <- rast("Phen_altoandino.tif")

cat("Fusionando mapas espacialmente...\n")

# 2. Unir espacialmente (efecto rompecabezas)
phen_unido <- merge(phen_superior, phen_matorral)

# 3. Guardar el gran mapa unificado en tu carpeta
writeRaster(phen_unido, "Phen_Unificado_Superior_Matorral.tif", overwrite = TRUE)

cat("¡Fusión completada con éxito!\n")

# 4. Comprobación visual de la capa 1 (para ver que los polígonos están juntos)
plot(rast("Phen_Unificado_Superior_Matorral.tif"))


#consulta de anomalia 
xy_1 <- cbind(-31139, 70778)  # El durazno
xy_2 <- cbind(-31465, 70632)  # vega cordillerana

serie_1 <- terra::extract(ndvi_stack, xy_1) %>% as.numeric()
serie_2 <- terra::extract(ndvi_chepica, xy_2) %>% as.numeric()
plot(serie_2)


serie_1 <- terra::extract(ndvi_chepica, xy_2) %>% as.numeric()
#quiero q el periodo de referencia sea de 2000 a 2010
#y que el tiempo. aestudiar sea de 2011 a 2024

anom_plant <- ExtremeAnom(x = serie_2,
                          dates = fechas_completas,
                          h =2,
                          refp = c(1:196),
                          anop = c(312:380),
                          rge = c(0, 5000),
                          output = 'both')