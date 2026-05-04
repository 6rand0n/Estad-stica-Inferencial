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
cat("\n--- RESULTADOS EJERCICIO 15 ---\n")
res_ej15 = contraste_hipotesis_z_2pob(
  n1 = 61, xbar1 = 40, var1 = 24.9,
  n2 = 61, xbar2 = 29, var2 = 22.7,
  alpha = 0.01,
  tipo_contraste = "bilateral"
)
print(res_ej15)


# ---------------------------------------------------------
# Ejercicio 16: Proveedores de circuitos integrados
# Se sospecha que Prov 1 entrega más defectuosos que Prov 2 (mu1 > mu2)
# H0: mu1 - mu2 <= 0  vs  Ha: mu1 - mu2 > 0
# Tipo de contraste: Menor o igual
# ---------------------------------------------------------
cat("\n--- RESULTADOS EJERCICIO 16 ---\n")

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
print(res_ej16)

