# =========================================================
# Archivo: Ejercicio17.R
# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
# Oscar Ivan Gomez Ruiz
#
# Solución al Ejercicio 17
# =========================================================
source("C:/Users/Dookie/Documents/Estad-stica-Inferencial/Contrastes de hipotesis para dos poblaciones/IC_Util.R")

# ---------------------------------------------------------
# Ejercicio 17: Contenido de sodio en refrescos
# ---------------------------------------------------------

# Parámetros
n1 <- 10
xbar1 <- 41
var1 <- 11.0

n2 <- 10
xbar2 <- 42
var2 <- 11.9

alpha <- 0.10
tipo_contraste <- "mayor_igual"

# Planteamiento
# Se sospecha que el contenido en Ginger Ale (1) es menor que Cola (2) (mu1 < mu2)
#
# H0: mu1 - mu2 >= 0  vs  Ha: mu1 - mu2 < 0
# Tipo de contraste: Mayor o igual
#
# Obtenemos Z dividiendo la diferencia de medias entre el error estándar
# Z = ( (41 - 42) - 0 ) / ( sqrt(1.1 + 1.19) )
# Z = -0.6608
#
# Valor Crítico:
# Para alpha = 0.10, el valor crítico de Z es ~-1.28.
#
# Regla de decisión:
# Rechazar H0 si Z es menor al Valor Crítico
#
# Como Z (-0.6608) es mayor al valor crítico (-1.28). No se rechaza la hipótesis nula
# 
# (OPCIONAL) Valor P
# valor-p = 0.2546
#
# Conclusión:
# No existe suficiente evidencia estadística con un nivel de significancia del 10%
# para afirmar que el contenido medio de sodio en las bebidas de ginger ale sea
# significativamente menor que en las bebidas de cola.

cat("\n--- RESULTADOS EJERCICIO 17 ---\n")
res_ej17 = contraste_hipotesis_z_2pob(n1, xbar1, var1, n2, xbar2, var2, alpha, tipo_contraste)
print(res_ej17)

