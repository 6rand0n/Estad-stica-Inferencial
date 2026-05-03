# Archivo: CasosDeUso.R
# Usa las funciones de IC_Util.R para resolver los ejercicios 5 y 6.
# Si cambian los datos del ejercicio, solo se modifican los argumentos.

source("IC_Util.R")


# Ejercicio 5: intervalo de confianza para p1 - p2.

ejercicio5 = ICprop1prop2(
	x1 = 16,
	n1 = 90,
	x2 = 15,
	n2 = 92,
	coefConf = 0.95
)

imprimirResultado(ejercicio5)


# Ejercicio 6: intervalo de confianza para mu1 - mu2 con sigmas conocidas.
# Se usa dif.barras porque el enunciado no da ambas medias muestrales.
# z.critico reproduce el valor mostrado en el enunciado.

ejercicio6 = ICmu1mu2Sigmas(
	dif.barras = -9.325,
	n1 = 20,
	n2 = 20,
	sigma1 = 28,
	sigma2 = 32,
	coefConf = 0.99,
	z.critico = 2.6396
)

imprimirResultado(ejercicio6)


# Ejemplo alternativo del ejercicio 6 si se conocen ambas medias.

# ejercicio6_alt = ICmu1mu2Sigmas(
# 	x.barra1 = 120,
# 	x.barra2 = 129.325,
# 	n1 = 20,
# 	n2 = 20,
# 	sigma1 = 28,
# 	sigma2 = 32,
# 	coefConf = 0.99
# )
#
# imprimirResultado(ejercicio6_alt)
