# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
#
# Archivo principal para los ejercicios de inferencia estadistica
# =========================================================

# setwd("D:/VSC Proyectos/Inferencia estadistica/")
source("C:/Users/Dookie/Documents/Estad-stica-Inferencial/Intervalos de confianza/IC_Util.R")

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
# IC para media con sigma conocida
# =========================================================

ejercicio3 = ICmu(
	x.barra = 74.1,
	sigma = sqrt(13.0),
	n = 50,
	coefConf = 0.95
)

imprimirResultado(ejercicio3 )
