# Universidad Autónoma de Aguascalientes
# Centro de Ciencias Básicas
# Departamento de Estadística
# Ing. En Sistemas Computacionales
# Inferencia Estadística
#
# 2026 04 28
# =====
# Archivo "Ejercicios Contraste de Hipótesis ISC.R"
# Ejercicio 10. Contraste de Hipótesis para una Proporción

# Suponga que la opinión prevaleciente entre los analistas de la bolsa de valores 
# es que sólo el 35% de las ofertas públicas de compra resultan en una adquisición real. 
# Un grupo de analistas cree que el porcentaje es en realidad más bajo que eso. 
# Para poner a prueba su suposición, rastrearon 20 ofertas públicas de compra
# y encontraron que solamente 2 de ellas concluyeron en una adquisición. 
# Contraste la hipótesis de que el porcentaje real de adquisiciones es menor que 35%
#
# Lectura del archivo de utilerías
source("C:/Users/Javi/Desktop/estadistica/Estad-stica-Inferencial/Contrastes_de_hipotesis para una_poblacion/CH_util_una_poblacion.R")

# Definición de parámetros
H_alternativa <- "<"
p0 <- 0.35
x <- 2
n <- 20
alfa <- 0.05


# Ejecución del contraste
resultado <- contraste_prop(H_alternativa, p0, x, n, alfa)


# Resultados
print(resultado)
