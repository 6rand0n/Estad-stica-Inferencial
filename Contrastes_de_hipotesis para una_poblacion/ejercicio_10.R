# Universidad Autónoma de Aguascalientes
# Centro de Ciencias Básicas
# Departamento de Estadística
# Ing. En Sistemas Computacionales
# Inferencia Estadística
# Francisco Javier Sanchez Vallin

# Ejercicio 10

# Suponga que la opinión prevaleciente entre los analistas de la bolsa de valores 
# es que sólo el 35% de las ofertas públicas de compra resultan en una adquisición 
# real. Un grupo de analistas cree que el porcentaje es en realidad más bajo que eso. 

# Para poner a prueba su suposición, rastrearon 20 ofertas públicas de compra
# y encontraron que solamente 2 de ellas concluyeron en una adquisición. Contraste 
# la hipótesis de que el porcentaje real de adquisiciones es menor que 35%


# Con parametros: Ha: <  y EP: Z
# =======================================================================================

# Se cargan las funciones externas
# tener en cuenta la direccio de los archivos cuando lo ejecuten
source("C:/Users/Javi/Desktop/estadistica/Estad-stica-Inferencial/Contrastes_de_hipotesis para una_poblacion/CH_util_una_poblacion.R")

cat("EJERCICIO 10 – CONTRASTE DE HIPÓTESIS PARA UNA PROPORCIÓN\n\n\n")


# Datos del problema
Ha <- "<"     # Hipótesis alternativa (menor que)
p0 <- 0.35    # Proporción planteada en H0
x <- 2        # Número de éxitos observados
n <- 20       # Tamaño de la muestra
alfa <- 0.05  # Nivel de significancia

cat("DATOS DEL PROBLEMA\n")
cat("Proporción bajo H0 (p0):", p0, "\n")
cat("Éxitos observados (x):", x, "\n")
cat("Tamaño de muestra (n):", n, "\n")
cat("Nivel de significancia (alfa):", alfa, "\n\n")

# ============================================================================
# Ejecución del contraste
resultado <- contraste_prop(Ha, p0, x, n, alfa)


# Resultados paso a paso de los resultados obtenidos
# ============================================================================

# Proporción muestral
cat("PROPORCIÓN MUESTRAL\n")
cat("p̂ =", resultado$p_muestral, "\n")
cat("Interpretación: es la proporción observada en la muestra.\n\n")

# Estadístico de prueba
cat("ESTADÍSTICO DE PRUEBA\n")
cat("Z calculado =", resultado$Z_calculado, "\n")
cat("Interpretación: mide qué tan lejos está la proporción muestral de p0.\n\n")

# Valor crítico
cat("VALOR CRÍTICO\n")
cat("Z crítico =", resultado$Z_critico, "\n")
cat("Interpretación: es el límite de la región de rechazo para alfa =", alfa, "\n\n")

# Decisión estadística
cat("DECISIÓN\n")
if (resultado$rechaza_H0) {
  cat("El estadístico cae en la región crítica.\n")
  cat("→ Se rechaza la hipótesis nula.\n\n")
} else {
  cat("El estadístico NO cae en la región crítica.\n")
  cat("→ No se rechaza la hipótesis nula.\n\n")
}

# Conclusión en el contexto del problema
cat("CONCLUSIÓN\n")

cat("Con un nivel de significancia de", alfa,
    ", no existe evidencia estadística suficiente\n",
    "para afirmar que el porcentaje real de adquisiciones en la población\n",
    "sea menor al 35%, de acuerdo con la información proporcionada por la muestra.\n")


