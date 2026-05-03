# =========================================================
# Archivo: IC_Util.R
# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
#
# Utilerias para intervalos de confianza
# =========================================================

# =========================================================
# validarProbabilidad
# =========================================================
validarProbabilidad = function(valor, nombreParametro) {

	if(valor <= 0 || valor >= 1) {

		stop(
			paste(
				nombreParametro,
				"Debe estar entre 0 y 1."
			)
		)
	}
}

# =========================================================
# validarPositivo
# =========================================================
validarPositivo = function(valor, nombreParametro) {

	if(valor <= 0) {

		stop(
			paste(
				nombreParametro,
				"Debe ser mayor que cero."
			)
		)
	}
}

# =========================================================
# ICprop
# Intervalo para proporcion poblacional
# =========================================================
ICprop = function(x, n, coefConf=0.95) {

	validarPositivo(n, "n")
	validarProbabilidad(coefConf, "coefConf")

	if(x < 0 || x > n) {
		stop("x debe estar entre 0 y n.")
	}

	alfa = 1 - coefConf
	alfaMedios = alfa / 2

	p = x / n

	z = qnorm(
		alfaMedios,
		lower.tail = FALSE
	)

	varianza = (p * (1 - p)) / n

	raiz = sqrt(varianza)

	error = z * raiz

	lic = p - error
	lsc = p + error

	lic = max(0, lic)
	lsc = min(1, lsc)

	# -----------------------------------------------------
	# Procedimiento 
	# -----------------------------------------------------

	pasos = c(

		paste(
			"p̂ = x / n =",
			x,
			"/",
			n,
			"=",
			round(p, 6)
		),

		paste(
			"alfa = 1 -",
			coefConf,
			"=",
			round(alfa, 6)
		),

		paste(
			"z crítico =",
			round(z, 6)
		),

		paste(
			"Error estándar = sqrt((p*(1-p))/n)"
		),

		paste(
			"   = sqrt((",
			round(p, 6),
			"*(1-",
			round(p, 6),
			"))/",
			n,
			")"
		),

		paste(
			"   =",
			round(raiz, 6)
		),

		paste(
			"Error máximo = z * error estándar"
		),

		paste(
			"   =",
			round(z, 6),
			"*",
			round(raiz, 6),
			"=",
			round(error, 6)
		),

		paste(
			"LIC =",
			round(p, 6),
			"-",
			round(error, 6),
			"=",
			round(lic, 6)
		),

		paste(
			"LSC =",
			round(p, 6),
			"+",
			round(error, 6),
			"=",
			round(lsc, 6)
		)
	)

	resultado = list(

		metodo = "IC para proporcion",

		parametros = list(
			x = x,
			n = n,
			coefConf = coefConf
		),

		calculos = list(
			alfa = alfa,
			z = z,
			proporcion = p,
			error = error
		),

		intervalo = list(
			LIC = lic,
			LSC = lsc
		),

		procedimiento = pasos
	)

	return(resultado)
}

# =========================================================
# ICmu
# Intervalo para media poblacional
# =========================================================
ICmu = function(
	x.barra,
	n,
	coefConf=0.95,
	sigma=NULL,
	s=NULL
) {

	validarPositivo(n, "n")
	validarProbabilidad(coefConf, "coefConf")

	alfa = 1 - coefConf
	alfaMedios = alfa / 2

	if(is.null(sigma)) {

		validarPositivo(s, "s")

		estadistico = qt(
			alfaMedios,
			df = n - 1,
			lower.tail = FALSE
		)

		errorEstandar = s / sqrt(n)

		error = estadistico * errorEstandar

		distribucion = "t Student"

	} else {

		validarPositivo(sigma, "sigma")

		estadistico = qnorm(
			alfaMedios,
			lower.tail = FALSE
		)

		errorEstandar = sigma / sqrt(n)

		error = estadistico * errorEstandar

		distribucion = "Normal Z"
	}

	lic = x.barra - error
	lsc = x.barra + error

	# -----------------------------------------------------
	# Procedimiento
	# -----------------------------------------------------

	pasos = c(

		paste(
			"alfa = 1 -",
			coefConf,
			"=",
			round(alfa, 6)
		),

		paste(
			"Distribucion utilizada:",
			distribucion
		),

		paste(
			"Estadistico crítico =",
			round(estadistico, 6)
		),

		paste(
			"Error estándar =",
			round(errorEstandar, 6)
		),

		paste(
			"Error máximo =",
			round(estadistico, 6),
			"*",
			round(errorEstandar, 6),
			"=",
			round(error, 6)
		),

		paste(
			"LIC =",
			round(x.barra, 6),
			"-",
			round(error, 6),
			"=",
			round(lic, 6)
		),

		paste(
			"LSC =",
			round(x.barra, 6),
			"+",
			round(error, 6),
			"=",
			round(lsc, 6)
		)
	)

	resultado = list(

		metodo = "IC para media",

		parametros = list(
			x.barra = x.barra,
			n = n,
			coefConf = coefConf
		),

		calculos = list(
			alfa = alfa,
			distribucion = distribucion,
			estadistico = estadistico,
			error = error
		),

		intervalo = list(
			LIC = lic,
			LSC = lsc
		),

		procedimiento = pasos
	)

	return(resultado)
}

# =========================================================
# ICvar
# Intervalo para varianza poblacional
# =========================================================
ICvar = function(
	s2,
	n,
	coefConf=0.95
) {

	validarPositivo(s2, "s2")
	validarPositivo(n, "n")
	validarProbabilidad(coefConf, "coefConf")

	alfa = 1 - coefConf

	gl = n - 1

	chiInferior = qchisq(
		alfa / 2,
		gl
	)

	chiSuperior = qchisq(
		1 - alfa / 2,
		gl
	)

	lic = (gl * s2) / chiSuperior

	lsc = (gl * s2) / chiInferior

	# -----------------------------------------------------
	# Procedimiento
	# -----------------------------------------------------

	pasos = c(

		paste(
			"alfa =",
			round(alfa, 6)
		),

		paste(
			"Grados de libertad =",
			gl
		),

		paste(
			"Chi cuadrada inferior =",
			round(chiInferior, 6)
		),

		paste(
			"Chi cuadrada superior =",
			round(chiSuperior, 6)
		),

		paste(
			"LIC = (gl*s2)/ChiSup"
		),

		paste(
			"   = (",
			gl,
			"*",
			s2,
			")/",
			round(chiSuperior, 6),
			"=",
			round(lic, 6)
		),

		paste(
			"LSC = (gl*s2)/ChiInf"
		),

		paste(
			"   = (",
			gl,
			"*",
			s2,
			")/",
			round(chiInferior, 6),
			"=",
			round(lsc, 6)
		)
	)

	resultado = list(

		metodo = "IC para varianza",

		intervalo = list(
			LIC = lic,
			LSC = lsc
		),

		procedimiento = pasos
	)

	return(resultado)
}

# =========================================================
# ICmu1mu2
# Intervalo para diferencia de medias
# =========================================================

ICmu1mu2 = function(
	x.barra1,
	n1,
	x.barra2,
	n2,
	coefConf=0.95,
	sigma1=NULL,
	sigma2=NULL,
	s1=NULL,
	s2=NULL
) {

	validarPositivo(n1, "n1")
	validarPositivo(n2, "n2")

	validarProbabilidad(
		coefConf,
		"coefConf"
	)

	alfa = 1 - coefConf
	alfaMedios = alfa / 2

	diferencia = x.barra1 - x.barra2

	if(
		is.null(sigma1) ||
		is.null(sigma2)
	) {

		if(
			is.null(s1) ||
			is.null(s2)
		) {

			stop(
				"Debe proporcionar sigma1 y sigma2 o s1 y s2."
			)
		}

		gl = n1 + n2 - 2

		estadistico = qt(
			alfaMedios,
			gl,
			lower.tail = FALSE
		)

		sp2 =
			(
				((n1 - 1) * s1^2) +
				((n2 - 1) * s2^2)
			) / gl

		errorEstandar =
			sqrt(
				sp2 *
				((1 / n1) + (1 / n2))
			)

		error =
			estadistico *
			errorEstandar

		distribucion = "t Student"

	} else {

		estadistico = qnorm(
			alfaMedios,
			lower.tail = FALSE
		)

		errorEstandar =
			sqrt(
				(sigma1^2 / n1) +
				(sigma2^2 / n2)
			)

		error =
			estadistico *
			errorEstandar

		distribucion = "Normal Z"
	}

	lic = diferencia - error
	lsc = diferencia + error

	# -----------------------------------------------------
	# Procedimiento 
	# -----------------------------------------------------

	pasos = c(

		paste(
			"Diferencia de medias =",
			x.barra1,
			"-",
			x.barra2,
			"=",
			round(diferencia,6)
		),

		paste(
			"alfa = 1 -",
			coefConf,
			"=",
			round(alfa,6)
		),

		paste(
			"Distribucion utilizada:",
			distribucion
		),

		paste(
			"Estadistico critico =",
			round(estadistico,6)
		),

		paste(
			"Error estandar =",
			round(errorEstandar,6)
		),

		paste(
			"Error maximo =",
			round(estadistico,6),
			"*",
			round(errorEstandar,6),
			"=",
			round(error,6)
		),

		paste(
			"LIC =",
			round(diferencia,6),
			"-",
			round(error,6),
			"=",
			round(lic,6)
		),

		paste(
			"LSC =",
			round(diferencia,6),
			"+",
			round(error,6),
			"=",
			round(lsc,6)
		)
	)

	resultado = list(

		metodo = "IC diferencia de medias",

		parametros = list(
			x.barra1 = x.barra1,
			n1 = n1,
			x.barra2 = x.barra2,
			n2 = n2,
			coefConf = coefConf
		),

		calculos = list(
			alfa = alfa,
			distribucion = distribucion,
			estadistico = estadistico,
			error = error
		),

		intervalo = list(
			LIC = lic,
			LSC = lsc
		),

		procedimiento = pasos
	)

	return(resultado)
}