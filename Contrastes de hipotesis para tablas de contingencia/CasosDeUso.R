#Todos los demas problemas 

source("CH_contingencia_util.R")

# =====================================================================
# Ejercicio 21. Bondad de ajuste  -  Distribucion Poisson
#
# El numero de solicitudes a un servidor de impresion se supone
# que sigue una distribucion Poisson con lambda = 8.
# Se tomaron 50 observaciones clasificadas en 4 intervalos.
# Verificar la afirmacion con alfa = 0.05.
# =====================================================================

# Limites de los intervalos [inf, sup] (sup = Inf para cola abierta)
lim_inf_21 = c(0,  5,  9, 13)
lim_sup_21 = c(4,  8, 12, Inf)

# Frecuencias observadas por intervalo
obs_21 = c(1, 22, 23, 4)

# Parametros
lambda_21 = 8
n_21      = 50
alfa_21   = 0.05

etiquetas_21 = c("0 a 4", "5 a 8", "9 a 12", "13 o mas")

res_21 = bondad_ajuste_poisson(
  limites_inf = lim_inf_21,
  limites_sup = lim_sup_21,
  observadas  = obs_21,
  lambda      = lambda_21,
  n           = n_21,
  alfa        = alfa_21,
  etiquetas   = etiquetas_21
)

imprimir_resultado_chi2(res_21,
  "Ejercicio 21 - Bondad de ajuste Poisson (lambda = 8)")

cat("Conclusion: ")
if (res_21$rechaza_H0) {
  cat("Con alfa =", alfa_21, "se rechaza H0. Los datos NO son consistentes",
      "con una distribucion Poisson de lambda = 8.\n")
} else {
  cat("Con alfa =", alfa_21, "no se rechaza H0. Los datos son consistentes",
      "con una distribucion Poisson de lambda = 8.\n")
}


# =====================================================================
# Ejercicio 22. Bondad de ajuste  -  Distribucion uniforme (Round Robin)
#
# Un algoritmo Round Robin distribuye peticiones entre 5 servidores.
# Se esperaria una distribucion uniforme (misma probabilidad = 1/5).
# Se analizaron 500 peticiones. Usar alfa = 0.02.
# =====================================================================

obs_22      = c(92, 108, 115, 85, 100)
n_22        = sum(obs_22)            # 500
k_22        = length(obs_22)         # 5 servidores
alfa_22     = 0.02
etiquetas_22 = c("A", "B", "C", "D", "E")

# Bajo H0 (uniforme) cada servidor recibe n/k peticione
esp_22 = rep(n_22 / k_22, k_22)

res_22 = bondad_ajuste(
  observadas = obs_22,
  esperadas  = esp_22,
  alfa       = alfa_22,
  etiquetas  = etiquetas_22
)

imprimir_resultado_chi2(res_22,
  "Ejercicio 22 - Bondad de ajuste uniforme (Round Robin)")

cat("Conclusion: ")
if (res_22$rechaza_H0) {
  cat("Con alfa =", alfa_22, "se rechaza H0. La distribucion de peticiones",
      "NO es uniforme entre los servidores.\n")
} else {
  cat("Con alfa =", alfa_22, "no se rechaza H0. No hay evidencia suficiente",
      "para afirmar que la distribucion difiere de la uniforme.\n")
}

# =====================================================================
# Ejercicio 24. Prueba de independencia  -  Sexo vs Carrera
#
# Se desea saber si hombres y mujeres eligen por igual entre
# tres carreras. 63 estudiantes encuestados. Usar alfa = 0.01.
# =====================================================================

datos_24 = matrix(
  c( 7, 18,
    18, 14,
     3,  3),
  nrow = 3, byrow = TRUE
)
rownames(datos_24) = c("Lic. Urbanismo",
                        "Ing. Bioquimico",
                        "Lic. Matematicas Aplicadas")
colnames(datos_24) = c("Femenino", "Masculino")

alfa_24 = 0.01

res_24 = prueba_independencia(
  tabla_obs = datos_24,
  alfa      = alfa_24
)

imprimir_resultado_chi2(res_24,
  "Ejercicio 24 - Independencia: sexo vs carrera elegida")

cat("Conclusion: ")
if (res_24$rechaza_H0) {
  cat("Con alfa =", alfa_24, "se rechaza H0. La eleccion de carrera",
      "NO es independiente del sexo del estudiante.\n")
} else {
  cat("Con alfa =", alfa_24, "no se rechaza H0. No hay evidencia suficiente",
      "para afirmar que la eleccion de carrera depende del sexo.\n")
}


# =====================================================================
# Ejercicio 25. Prueba de homogeneidad  -  Instructores
#
# Se comparan 3 instructores respecto a la proporcion de aprobados.
# 40 estudiantes por instructor, 120 en total. Usar alfa = 0.05.
# =====================================================================

datos_25 = matrix(
  c(30, 10,
    18, 22,
    28, 12),
  nrow = 3, byrow = TRUE
)
rownames(datos_25) = c("Instructor A", "Instructor B", "Instructor C")
colnames(datos_25) = c("Aprobo", "No aprobo")

alfa_25 = 0.05

res_25 = prueba_homogeneidad(
  tabla_obs = datos_25,
  alfa      = alfa_25
)

imprimir_resultado_chi2(res_25,
  "Ejercicio 25 - Homogeneidad: proporcion de aprobados por instructor")

cat("Conclusion: ")
if (res_25$rechaza_H0) {
  cat("Con alfa =", alfa_25, "se rechaza H0. La proporcion de aprobados",
      "NO es la misma para los tres instructores.\n")
} else {
  cat("Con alfa =", alfa_25, "no se rechaza H0. No hay evidencia suficiente",
      "para afirmar que los instructores difieren en la proporcion de aprobados.\n")
}
