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
# Intervalo de confianza para una proporcion poblacional
#
# Uso recomendado:
# - Puede utilizarse aunque la poblacion
#   original no siga una distribucion normal.
# - Funciona mediante aproximacion normal
#   apoyada en el Teorema Central del Limite.
#
# Requisitos recomendados:
# - La muestra debe ser aleatoria.
# - Las observaciones deben ser independientes.
# - Se recomienda que:
#     n*p >= 5
#     n*(1-p) >= 5
#
# TCL:
# - La distribucion muestral de p̂ se aproxima
#   a una normal cuando el tamano de muestra
#   es suficientemente grande.
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
# Intervalo de confianza para media poblacional
#
# Casos soportados:
#
# 1) Sigma conocida
#    - Utiliza distribucion Normal Z.
#
# 2) Sigma desconocida
#    - Utiliza distribucion t Student.
#
# Uso recomendado:
# - Puede utilizarse si la poblacion es normal.
# - Tambien puede utilizarse aunque la
#   poblacion NO sea normal si el tamano
#   de muestra es suficientemente grande
#   (aplicando TCL).
#
# Regla practica:
# - Comunmente se considera suficiente:
#       n >= 30
#
# Parametros:
# x.barra    Media muestral
# n          Tamano de muestra
# coefConf   Coeficiente de confianza
# sigma      Desviacion estandar poblacional
# s          Desviacion estandar muestral
#
# Regresa:
# Lista con calculos intermedios y limites
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
# Intervalo de confianza para varianza poblacional
#
# Utiliza la distribucion Chi-cuadrada
#
# Uso recomendado:
# - SOLO debe utilizarse si puede asumirse
#   que la poblacion sigue una distribucion
#   normal.
#
# IMPORTANTE:
# - Este metodo NO se corrige con el TCL.
# - Aunque la muestra sea grande, la
#   normalidad poblacional sigue siendo
#   necesaria.
#
# Requisitos:
# - Muestra aleatoria.
# - Observaciones independientes.
#
# Parametros:
# s2         Varianza muestral
# n          Tamano de muestra
# coefConf   Coeficiente de confianza
#
# Regresa:
# Lista con calculos intermedios y limites
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
		),
	
		procedimiento = pasos
	)

	return(resultado)
}