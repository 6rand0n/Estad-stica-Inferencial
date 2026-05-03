# Archivo: Ejercicio_7.R
# Resuelve el ejercicio principal de razon de varianzas.
# Carga IC_Util.R, define los datos del ejercicio 7 y muestra el resultado.

source("IC_Util.R")

# Ejercicio 7: intervalo de confianza para sigma1^2 / sigma2^2.
ejercicio7 = ICvar1var2(
	s1.2 = 18,
	n1 = 12,
	s2.2 = 6,
	n2 = 10,
	coefConf = 0.98
)

imprimirResultado(ejercicio7)
