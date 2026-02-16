library(npphen)
library(terra)
library(readr)

# 1. CARGAR DATOS
ndvi_stack <- rast("data/Landsat_NDVI_Stack.tif")
fechas_df <- read_csv("data/Landsat_NDVI_Dates.csv")
fechas_vector <- as.Date(fechas_df$date)

# 2. LIMPIEZA RÁPIDA (Clamp)
# Esto le dice a R: "Cualquier valor menor a 0, conviértelo en 0".
# values=FALSE hace que no cargue todo a la RAM, sino que lo prepare virtualmente.
ndvi_limpio <- clamp(ndvi_stack, lower=0, values=FALSE)

# 3. EXTRAER VALORES CRUDOS (Enteros)
# Importante: NO dividimos por 10000. Mantenemos el número grande (ej: 8500)
valores_tabla <- extract(ndvi_limpio)
serie_enteros <- as.numeric(valores_tabla[1,-1])

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

# Diagnóstico: Verificamos que sean números grandes (0-10000)
cat("Ejemplo de datos:", head(serie_enteros), "\n")
cat("Rango de datos:", range(serie_enteros, na.rm=TRUE), "\n")


# 6. EJECUTAR PHENMAP (Ahora sin advertencias)

PhenMap(
  s = ndvi_limpio,            # Usamos el mapa limpio
  dates = fechas_vector,
  h = 2,
  nCluster = 1,               # 1 núcleo para máxima seguridad
  outname = "imagenes/Mapa_Fenologia_Chepica.tif", 
  datatype = "INT2S",
  rge = c(0, 10000)
)

# --- PRUEBA DE CONCEPTO (CROP) ---
# 1. Recortamos un pedacito pequeño del centro (solo para probar)
# Usamos las coordenadas de tu punto central +/- 1000 metros
e <- ext(ndvi_limpio) 
crop_extent <- ext(mean(e[1:2])-1000, mean(e[1:2])+1000, mean(e[3:4])-1000, mean(e[3:4])+1000)
ndvi_mini <- crop(ndvi_limpio, crop_extent)

cat("Probando con un mapa miniatura...\n")

# 2. Corremos PhenMap en el miniatura
PhenMap(
  s = ndvi_mini,
  dates = fechas_vector,
  h = 2,
  nCluster = 3,
  outname = "imagenes/TEST_MINI.tif",
  datatype = "INT2S",
  rge = c(0, 10000)
)

# 3. Si esto termina rápido y genera el archivo, ¡todo está bien!
# Puedes volver a correr el grande con confianza (y paciencia).
plot(rast("imagenes/TEST_MINI.tif")[[1]], main="Prueba Exitosa")
