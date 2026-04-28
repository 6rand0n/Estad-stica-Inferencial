# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
#
# Archivo principal para los ejercicios de inferencia estadistica
# =========================================================

source("C:/Users/brand/Downloads/IC_Util.R")

# =========================================================
# Funcion para imprimir resultados
# =========================================================

imprimirResultado = function(resultado) {

	cat("\n========================================\n")
	cat(resultado$metodo, "\n")
	cat("========================================\n")

	cat("\nProcedimiento:\n\n")

	for(i in resultado$procedimiento) {

		cat(i, "\n")
	}

	cat("\nIntervalo:\n")

	cat(
		paste(
			"LIC =",
			round(resultado$intervalo$LIC,4),
			"\n"
		)
	)

	cat(
		paste(
			"LSC =",
			round(resultado$intervalo$LSC,4),
			"\n"
		)
	)
}

# =========================================================
# IC para proporcion
# =========================================================

ej1 = ICprop(
	x = 84,
	n = 90,
	coefConf = 0.99
)

imprimirResultado(ej1)

# =========================================================
# IC para media con sigma conocida
# =========================================================

ej2 = ICmu(
	x.barra = 442.7,
	sigma = 21,
	n = 10,
	coefConf = 0.98
)

imprimirResultado(ej2)

# =========================================================
# IC para media con sigma desconocida
# =========================================================

ej3 = ICmu(
	x.barra = 74.1,
	s = sqrt(13),
	n = 50,
	coefConf = 0.95
)

imprimirResultado(ej3)

# =========================================================
# IC para varianza
# =========================================================

ej4 = ICvar(
	s2 = 680,
	n = 15,
	coefConf = 0.99
)

imprimirResultado(ej4)

# =========================================================
# IC para diferencia de medias
# =========================================================

ej5 = ICmu1mu2(
	x.barra1 = 4.1,
	sigma1 = 1.8,
	n1 = 100,
	x.barra2 = 4.5,
	sigma2 = 2,
	n2 = 100,
	coefConf = 0.90
)

imprimirResultado(ej5)