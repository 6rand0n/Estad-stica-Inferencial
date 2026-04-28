# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
#
# Utilerias para los ejercicios de intervalos de confianza
# =========================================================

# =========================================================
# validarProbabilidad
# Verifica que el coeficiente de confianza sea valido
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
# Verifica que un numero sea positivo
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
# Intervalo de confianza para una proporcion poblacional
# =========================================================
#
# Parametros:
# x          Numero de exitos
# n          Tamano de muestra
# coefConf   Coeficiente de confianza
#
# Regresa:
# Lista con calculos intermedios y limites
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

	error = z * sqrt(
		(p * (1 - p)) / n
	)

	lic = p - error
	lsc = p + error

	# Ajuste de limites validos
	lic = max(0, lic)
	lsc = min(1, lsc)

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
		)
	)

	return(resultado)
}

# =========================================================
# ICmu
# Intervalo de confianza para media poblacional
# =========================================================
#
# Trabaja con:
# - Sigma conocida  -> distribucion normal
# - Sigma desconocida -> distribucion t
#
# Parametros:
# x.barra
# n
# coefConf
# sigma
# s
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

		if(is.null(s)) {

			stop(
				"Debe proporcionar sigma o s."
			)
		}

		validarPositivo(s, "s")

		estadistico = qt(
			alfaMedios,
			df = n - 1,
			lower.tail = FALSE
		)

		error = estadistico * s / sqrt(n)

		distribucion = "t Student"

	} else {

		validarPositivo(sigma, "sigma")

		estadistico = qnorm(
			alfaMedios,
			lower.tail = FALSE
		)

		error = estadistico * sigma / sqrt(n)

		distribucion = "Normal Z"
	}

	lic = x.barra - error
	lsc = x.barra + error

	resultado = list(

		metodo = "IC para media",

		parametros = list(
			x.barra = x.barra,
			n = n,
			coefConf = coefConf,
			sigma = sigma,
			s = s
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
		)
	)

	return(resultado)
}

# =========================================================
# ICvar
# Intervalo de confianza para varianza poblacional
# =========================================================
#
# Parametros:
# s2
# n
# coefConf
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

	resultado = list(

		metodo = "IC para varianza",

		parametros = list(
			s2 = s2,
			n = n,
			coefConf = coefConf
		),

		calculos = list(
			alfa = alfa,
			gl = gl,
			chiInferior = chiInferior,
			chiSuperior = chiSuperior
		),

		intervalo = list(
			LIC = lic,
			LSC = lsc
		)
	)

	return(resultado)
}

# =========================================================
# ICmu1mu2
# Intervalo para diferencia de medias
# =========================================================
#
# Casos:
# - Sigmas conocidas
# - Sigmas desconocidas pero iguales
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

		error =
			estadistico *
			sqrt(
				sp2 *
				((1 / n1) + (1 / n2))
			)

		distribucion = "t Student"

	} else {

		estadistico = qnorm(
			alfaMedios,
			lower.tail = FALSE
		)

		error =
			estadistico *
			sqrt(
				(sigma1^2 / n1) +
				(sigma2^2 / n2)
			)

		distribucion = "Normal Z"
	}

	lic = diferencia - error
	lsc = diferencia + error

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
		)
	)

	return(resultado)
}