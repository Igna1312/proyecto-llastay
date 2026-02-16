library(terra)

# 1. Calcular el "Píxel Máximo Histórico"
# Esto crea un solo mapa donde cada píxel tiene el valor más alto de NDVI
# que alcanzó en los 10 años.
cat("Calculando el máximo histórico por píxel...\n")
mapa_maximo <- max(ndvi_stack, na.rm = TRUE)

# 2. Definir el Umbral de "Vega" (CRÍTICO)
# En los Andes, las vegas suelen tener NDVI > 0.20 o 0.25 en su mejor momento.
# El suelo desnudo/roca suele estar bajo 0.10.
# Ajusta este valor (0.20 = 2000 en tu escala INT16) mirando el mapa.
umbral_vega <- 2000 

# 3. Crear la Máscara de la Máxima Extensión
# (1 = Es Vega, NA = No es Vega)
mascara_vegas <- ifel(mapa_maximo >= umbral_vega, 1, NA)

# Visualizar para confirmar que el "dibujo" coincide con la realidad
plot(mascara_vegas, main = "Extensión Máxima de Vegas (Máscara)", 
     col = "green", legend = FALSE)
# Agregamos un fondo gris para entender dónde estamos
plot(mapa_maximo, main = "NDVI Máximo Histórico", alpha=0.5, add=TRUE)

# 4. Calcular el Área Total (Hectáreas)
# terra::expanse calcula el área real considerando la proyección
area_m2 <- expanse(mascara_vegas, unit = "m")
hectareas_totales <- sum(area_m2$area, na.rm = TRUE) / 10000

cat("La extensión máxima de las vegas en el área es de:", round(hectareas_totales, 2), "hectáreas.\n")
