# Archivo: IC_Util.R
# Funciones reutilizables para calcular intervalos de confianza
# de dos poblaciones. Aqui no se resuelve un ejercicio especifico.

# Valida que una probabilidad, como coefConf, este entre 0 y 1.
# valor: numero a validar.
# nombreParametro: nombre usado en el mensaje de error.
validarProbabilidad = function(valor, nombreParametro) {
	
	if(valor <= 0 || valor >= 1) {
		
		stop(
			paste(
				nombreParametro,
				"debe estar entre 0 y 1."
			)
		)
	}
}


# Valida que un dato numerico sea mayor que cero.
# valor: numero a validar.
# nombreParametro: nombre usado en el mensaje de error.
validarPositivo = function(valor, nombreParametro) {
	
	if(valor <= 0) {
		
		stop(
			paste(
				nombreParametro,
				"debe ser mayor que cero."
			)
		)
	}
}


# Calcula el IC para la diferencia de proporciones p1 - p2.
# x1, x2: exitos observados en cada muestra.
# n1, n2: tamanos de muestra.
# coefConf: coeficiente de confianza.

ICprop1prop2 = function(
	x1,
	n1,
	x2,
	n2,
	coefConf = 0.95
) {
	
	validarPositivo(n1, "n1")
	validarPositivo(n2, "n2")
	validarProbabilidad(coefConf, "coefConf")
	
	if(x1 < 0 || x1 > n1) {
		stop("x1 debe estar entre 0 y n1.")
	}
	
	if(x2 < 0 || x2 > n2) {
		stop("x2 debe estar entre 0 y n2.")
	}
	
	alfa = 1 - coefConf
	alfaMedios = alfa / 2
	
	p1 = x1 / n1
	p2 = x2 / n2
	
	diferencia = p1 - p2
	
	z = qnorm(
		alfaMedios,
		lower.tail = FALSE
	)
	
	errorEstandar = sqrt(
		(p1 * (1 - p1) / n1) +
		(p2 * (1 - p2) / n2)
	)
	
	error = z * errorEstandar
	
	lic = diferencia - error
	lsc = diferencia + error
	
	contieneCero = lic <= 0 && lsc >= 0
	
	conclusion = if(contieneCero) {
		"Como el intervalo contiene al 0, no se puede afirmar que las proporciones sean diferentes."
	} else {
		"Como el intervalo no contiene al 0, se puede afirmar que las proporciones son diferentes."
	}
	
	condiciones = paste(
		"Condicion aproximada:",
		"n1*p1 =", round(n1 * p1, 4),
		", n1*(1-p1) =", round(n1 * (1 - p1), 4),
		", n2*p2 =", round(n2 * p2, 4),
		", n2*(1-p2) =", round(n2 * (1 - p2), 4)
	)
	
	pasos = c(
		
		paste(
			"p1 = x1 / n1 =",
			x1,
			"/",
			n1,
			"=",
			round(p1, 6)
		),
		
		paste(
			"p2 = x2 / n2 =",
			x2,
			"/",
			n2,
			"=",
			round(p2, 6)
		),
		
		paste(
			"Diferencia = p1 - p2 =",
			round(p1, 6),
			"-",
			round(p2, 6),
			"=",
			round(diferencia, 6)
		),
		
		paste(
			"alfa = 1 -",
			coefConf,
			"=",
			round(alfa, 6)
		),
		
		paste(
			"z critico =",
			round(z, 6)
		),
		
		"Error estandar = sqrt((p1*(1-p1)/n1) + (p2*(1-p2)/n2))",
		
		paste(
			"Error estandar =",
			round(errorEstandar, 6)
		),
		
		paste(
			"Error maximo = z * error estandar =",
			round(z, 6),
			"*",
			round(errorEstandar, 6),
			"=",
			round(error, 6)
		),
		
		paste(
			"LIC =",
			round(diferencia, 6),
			"-",
			round(error, 6),
			"=",
			round(lic, 6)
		),
		
		paste(
			"LSC =",
			round(diferencia, 6),
			"+",
			round(error, 6),
			"=",
			round(lsc, 6)
		),
		
		condiciones
	)
	
	resultado = list(
		
		metodo = "IC para diferencia de proporciones p1 - p2",
		
		parametros = list(
			x1 = x1,
			n1 = n1,
			x2 = x2,
			n2 = n2,
			coefConf = coefConf
		),
		
		calculos = list(
			alfa = alfa,
			z = z,
			p1 = p1,
			p2 = p2,
			diferencia = diferencia,
			errorEstandar = errorEstandar,
			error = error
		),
		
		intervalo = list(
			LIC = lic,
			LSC = lsc
		),
		
		procedimiento = pasos,
		
		conclusion = conclusion
	)
	
	return(resultado)
}


# Calcula el IC para mu1 - mu2 con sigmas poblacionales conocidas.
# n1, n2: tamanos de muestra.
# sigma1, sigma2: desviaciones estandar poblacionales.
# x.barra1, x.barra2 o dif.barras: medias muestrales o su diferencia.
# z.critico: valor Z manual opcional.

ICmu1mu2Sigmas = function(
	n1,
	n2,
	sigma1,
	sigma2,
	coefConf = 0.95,
	x.barra1 = NULL,
	x.barra2 = NULL,
	dif.barras = NULL,
	z.critico = NULL
) {
	
	validarPositivo(n1, "n1")
	validarPositivo(n2, "n2")
	validarPositivo(sigma1, "sigma1")
	validarPositivo(sigma2, "sigma2")
	validarProbabilidad(coefConf, "coefConf")
	
	if(is.null(dif.barras)) {
		
		if(is.null(x.barra1) || is.null(x.barra2)) {
			
			stop("Debe proporcionar x.barra1 y x.barra2, o directamente dif.barras.")
		}
		
		diferencia = x.barra1 - x.barra2
		
		pasoDiferencia = paste(
			"Diferencia de medias =",
			x.barra1,
			"-",
			x.barra2,
			"=",
			round(diferencia, 6)
		)
		
	} else {
		
		diferencia = dif.barras
		
		pasoDiferencia = paste(
			"Diferencia de medias proporcionada =",
			round(diferencia, 6)
		)
	}
	
	alfa = 1 - coefConf
	alfaMedios = alfa / 2
	
	if(is.null(z.critico)) {
		
		z = qnorm(
			alfaMedios,
			lower.tail = FALSE
		)
		
	} else {
		
		validarPositivo(z.critico, "z.critico")
		z = z.critico
	}
	
	errorEstandar = sqrt(
		(sigma1^2 / n1) +
		(sigma2^2 / n2)
	)
	
	error = z * errorEstandar
	
	lic = diferencia - error
	lsc = diferencia + error
	
	contieneCero = lic <= 0 && lsc >= 0
	
	conclusion = if(contieneCero) {
		"Como el intervalo contiene al 0, no se puede afirmar que exista diferencia entre las medias."
	} else if(lic > 0) {
		"Como todo el intervalo es positivo, la media de la poblacion 1 es mayor que la media de la poblacion 2."
	} else {
		"Como todo el intervalo es negativo, la media de la poblacion 1 es menor que la media de la poblacion 2."
	}
	
	pasos = c(
		
		pasoDiferencia,
		
		paste(
			"alfa = 1 -",
			coefConf,
			"=",
			round(alfa, 6)
		),
		
		"Distribucion utilizada: Normal Z",
		
		paste(
			"z critico =",
			round(z, 6)
		),
		
		"Error estandar = sqrt((sigma1^2/n1) + (sigma2^2/n2))",
		
		paste(
			"Error estandar =",
			round(errorEstandar, 6)
		),
		
		paste(
			"Error maximo = z * error estandar =",
			round(z, 6),
			"*",
			round(errorEstandar, 6),
			"=",
			round(error, 6)
		),
		
		paste(
			"LIC =",
			round(diferencia, 6),
			"-",
			round(error, 6),
			"=",
			round(lic, 6)
		),
		
		paste(
			"LSC =",
			round(diferencia, 6),
			"+",
			round(error, 6),
			"=",
			round(lsc, 6)
		)
	)
	
	resultado = list(
		
		metodo = "IC para diferencia de medias mu1 - mu2 con sigmas conocidas",
		
		parametros = list(
			x.barra1 = x.barra1,
			x.barra2 = x.barra2,
			dif.barras = dif.barras,
			n1 = n1,
			n2 = n2,
			sigma1 = sigma1,
			sigma2 = sigma2,
			coefConf = coefConf,
			z.critico = z.critico
		),
		
		calculos = list(
			alfa = alfa,
			z = z,
			errorEstandar = errorEstandar,
			error = error,
			diferencia = diferencia
		),
		
		intervalo = list(
			LIC = lic,
			LSC = lsc
		),
		
		procedimiento = pasos,
		
		conclusion = conclusion
	)
	
	return(resultado)
}


# Calcula el IC para la razon de varianzas sigma1^2 / sigma2^2.
# s1.2, s2.2: varianzas muestrales.
# n1, n2: tamanos de muestra.
# coefConf: coeficiente de confianza.

ICvar1var2 = function(
	s1.2,
	n1,
	s2.2,
	n2,
	coefConf = 0.95
) {
	
	validarPositivo(s1.2, "s1.2")
	validarPositivo(s2.2, "s2.2")
	validarPositivo(n1, "n1")
	validarPositivo(n2, "n2")
	validarProbabilidad(coefConf, "coefConf")
	
	if(n1 <= 1 || n2 <= 1) {
		stop("n1 y n2 deben ser mayores que 1.")
	}
	
	alfa = 1 - coefConf
	
	gl1 = n1 - 1
	gl2 = n2 - 1
	
	razon = s1.2 / s2.2
	
	fInferior = qf(
		alfa / 2,
		df1 = gl1,
		df2 = gl2
	)
	
	fSuperior = qf(
		1 - alfa / 2,
		df1 = gl1,
		df2 = gl2
	)
	
	lic = razon / fSuperior
	lsc = razon / fInferior
	
	contieneUno = lic <= 1 && lsc >= 1
	
	conclusion = if(contieneUno) {
		"Como el intervalo contiene al 1, no se puede afirmar que las varianzas sean distintas."
	} else {
		"Como el intervalo no contiene al 1, se puede afirmar que las varianzas son distintas."
	}
	
	pasos = c(
		
		paste(
			"Razon muestral = s1^2 / s2^2 =",
			s1.2,
			"/",
			s2.2,
			"=",
			round(razon, 6)
		),
		
		paste(
			"alfa = 1 -",
			coefConf,
			"=",
			round(alfa, 6)
		),
		
		paste(
			"Grados de libertad 1 = n1 - 1 =",
			n1,
			"- 1 =",
			gl1
		),
		
		paste(
			"Grados de libertad 2 = n2 - 1 =",
			n2,
			"- 1 =",
			gl2
		),
		
		paste(
			"F inferior =",
			round(fInferior, 6)
		),
		
		paste(
			"F superior =",
			round(fSuperior, 6)
		),
		
		"LIC = razon / F superior",
		
		paste(
			"LIC =",
			round(razon, 6),
			"/",
			round(fSuperior, 6),
			"=",
			round(lic, 6)
		),
		
		"LSC = razon / F inferior",
		
		paste(
			"LSC =",
			round(razon, 6),
			"/",
			round(fInferior, 6),
			"=",
			round(lsc, 6)
		)
	)
	
	resultado = list(
		
		metodo = "IC para razon de varianzas sigma1^2 / sigma2^2",
		
		parametros = list(
			s1.2 = s1.2,
			n1 = n1,
			s2.2 = s2.2,
			n2 = n2,
			coefConf = coefConf
		),
		
		calculos = list(
			alfa = alfa,
			gl1 = gl1,
			gl2 = gl2,
			fInferior = fInferior,
			fSuperior = fSuperior,
			razon = razon
		),
		
		intervalo = list(
			LIC = lic,
			LSC = lsc
		),
		
		procedimiento = pasos,
		
		conclusion = conclusion
	)
	
	return(resultado)
}


# Imprime el metodo, el procedimiento, el intervalo y la conclusion.
# resultado: lista devuelta por alguna funcion de intervalo.

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
			round(resultado$intervalo$LIC, 4),
			"\n"
		)
	)
	
	cat(
		paste(
			"LSC =",
			round(resultado$intervalo$LSC, 4),
			"\n"
		)
	)
	
	cat("\nConclusion:\n")
	cat(resultado$conclusion, "\n")
}
