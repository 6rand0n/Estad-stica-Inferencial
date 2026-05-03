# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Ing. En Sistemas Computacionales
# Inferencia Estadistica
#
# Alumno : Angel Azael Fajardo Espino
# Semestre: 6to  Grupo: ISC
# Fecha  : 2026-05-03
# =====
# Archivo: Ejercicios_Contingencia_ISC.R
# Ejercicios de contrastes de hipotesis para tablas de contingencia
# Seccion V  -  Ejercicios 21 al 25
# =====

source("CH_contingencia_util.R")

# =====================================================================
# Ejercicio 23. Prueba de independencia
#
# Cluster con 3 tipos de carga (CPU-bound, I/O-bound, Mixta) y
# 3 niveles de tiempo de respuesta (Rapido, Medio, Lento).
# 90 observaciones. Verificar independencia con alfa = 0.02.
# =====================================================================

# Tabla de contingencia 3x3
datos_23 = matrix(
  c(16,  7,  7,
     6, 15,  9,
     8,  8, 14),
  nrow = 3, byrow = TRUE
)
rownames(datos_23) = c("CPU-bound", "I/O-bound", "Mixta")
colnames(datos_23) = c("Rapido", "Medio", "Lento")

alfa_23 = 0.02

res_23 = prueba_independencia(
  tabla_obs = datos_23,
  alfa      = alfa_23
)

imprimir_resultado_chi2(res_23,
  "Ejercicio 23 - Independencia: tipo de carga vs tiempo de respuesta")

cat("Conclusion: ")
if (res_23$rechaza_H0) {
  cat("Con alfa =", alfa_23, "se rechaza H0. El tiempo de respuesta",
      "NO es independiente del tipo de carga.\n")
} else {
  cat("Con alfa =", alfa_23, "no se rechaza H0. No hay evidencia suficiente",
      "para afirmar que el tiempo de respuesta depende del tipo de carga.\n")
}