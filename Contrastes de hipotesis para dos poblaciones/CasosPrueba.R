# =========================================================
# Archivo: CasosPrueba.R
# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
# Oscar Ivan Gomez Ruiz
#
# Casos de prueba de diferentes contrastes
# =========================================================
source("C:/Users/Dookie/Documents/Estad-stica-Inferencial/Contrastes de hipotesis para dos poblaciones/IC_Util.R")

# ---------------------------------------------------------
# Ejercicio 15: Comparación de lectores láser
# H0: mu1 - mu2 = 0  vs  Ha: mu1 - mu2 != 0
# Tipo de contraste: Bilateral
# ---------------------------------------------------------
cat("\n--- EJERCICIO 15 ---")
res_ej15 = contraste_hipotesis_z_2pob(
  n1 = 61, xbar1 = 40, var1 = 24.9,
  n2 = 61, xbar2 = 29, var2 = 22.7,
  alpha = 0.01,
  tipo_contraste = "bilateral"
)
cat(res_ej15$procedimiento)


# ---------------------------------------------------------
# Ejercicio 16: Proveedores de circuitos integrados
# Se sospecha que Prov 1 entrega más defectuosos que Prov 2 (mu1 > mu2)
# H0: mu1 - mu2 <= 0  vs  Ha: mu1 - mu2 > 0
# Tipo de contraste: Menor o igual
# ---------------------------------------------------------
cat("\n--- EJERCICIO 16 ---")
# Extraer medias a partir de los datos crudos proporcionados
datos_prov1 = c(3, 8, 5, 7, 2, 3, 8, 1, 5, 0, 6, 2)
datos_prov2 = c(0, 2, 3, 1, 3, 1, 1, 5, 4, 0)

media_prov1 = mean(datos_prov1)
media_prov2 = mean(datos_prov2)

res_ej16 = contraste_hipotesis_z_2pob(
  n1 = length(datos_prov1), xbar1 = media_prov1, var1 = 6.25,
  n2 = length(datos_prov2), xbar2 = media_prov2, var2 = 6.25,
  alpha = 0.05,
  tipo_contraste = "menor_igual"
)
cat(res_ej16$procedimiento)

# ---------------------------------------------------------
# Ejercicio 18: Proporción de créditos incobrables
# Casados (1): x=102, n=2230 | Unión Libre (2): x=31, n=1130
# Ha: p1 < p2 (contraste tipo "mayor_igual")
# ---------------------------------------------------------
cat("\n--- EJERCICIO 18 ---")
res_18 <- contraste_proporciones_z_2pob(
  n1 = 2230, x1 = 102, 
  n2 = 1130, x2 = 31, 
  alpha = 0.01, 
  tipo_contraste = "mayor_igual"
)
cat(res_18$procedimiento)

# ---------------------------------------------------------
# Ejercicio 19: Variación en calificaciones (F-test)
# Instructor 1: var=89.9, n=10 | Instructor 2: var=52.3, n=10
# Ha: var1 > var2 (contraste tipo "menor_igual")
# ---------------------------------------------------------
cat("\n--- EJERCICIO 19 ---")
res_19 <- contraste_varianzas_f_2pob(
  n1 = 10, var1 = 89.9,
  n2 = 10, var2 = 52.3,
  alpha = 0.10,
  tipo_contraste = "menor_igual"
)
cat(res_19$procedimiento)

# ---------------------------------------------------------
# Ejercicio 20: Varianzas de lecturas láser (F-test)
# Nuevo (1): var=24.9, n=61 | Antiguo (2): var=22.7, n=61
# Ha: var1 != var2 (contraste tipo "bilateral")
# ---------------------------------------------------------
cat("\n--- EJERCICIO 20 ---")
res_20 <- contraste_varianzas_f_2pob(
  n1 = 61, var1 = 24.9,
  n2 = 61, var2 = 22.7,
  alpha = 0.01,
  tipo_contraste = "bilateral"
)
cat(res_20$procedimiento)

