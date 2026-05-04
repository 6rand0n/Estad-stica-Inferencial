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
# Se sospecha que el contenido en Ginger Ale (1) es menor que Cola (2) (mu1 < mu2)
# H0: mu1 - mu2 >= 0  vs  Ha: mu1 - mu2 < 0
# Tipo de contraste: Mayor o igual
# ---------------------------------------------------------
cat("\n--- RESULTADOS EJERCICIO 17 ---\n")
res_ej17 = contraste_hipotesis_z_2pob(
  n1 = 10, xbar1 = 41, var1 = 11.0,
  n2 = 10, xbar2 = 42, var2 = 11.9,
  alpha = 0.10,
  tipo_contraste = "mayor_igual"
)
print(res_ej17)


