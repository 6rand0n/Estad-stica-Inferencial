# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Ing. En Sistemas Computacionales
# Inferencia Estadistica
#
# Alumno: Angel Azael Fajardo Espino
# Semestre: 6to  Grupo: ISC
# Fecha: 2026-05-03
# =====
# Archivo: CH_contingencia_util.R
# Utilidades para contrastes de hipotesis en tablas de contingencia:
#   - Bondad de ajuste (chi-cuadrada)
#   - Prueba de independencia (chi-cuadrada)
#   - Prueba de homogeneidad (chi-cuadrada)
# =====


# =====================================================================
# bondad_ajuste
# Contraste de bondad de ajuste chi-cuadrada.
# ---
# Parametros:
#   observadas   Vector de frecuencias observadas
#   esperadas    Vector de frecuencias esperadas (misma longitud)
#   alfa         Nivel de significancia (default 0.05)
#   etiquetas    Vector de nombres para las categorias (opcional)
# ---
bondad_ajuste = function(observadas, esperadas, alfa = 0.05,
                         etiquetas = NULL) {
  k = length(observadas)

  if (is.null(etiquetas)) {
    etiquetas = paste0("Cat_", seq_len(k))
  }

  # Estadistico chi-cuadrada
  chi2_calc = sum((observadas - esperadas)^2 / esperadas)

  # Grados de libertad (en bondad de ajuste pura: k - 1)
  gl = k - 1

  # Valor critico y p-valor (prueba de cola derecha)
  chi2_crit  = qchisq(alfa, df = gl, lower.tail = FALSE)
  p_valor    = pchisq(chi2_calc, df = gl, lower.tail = FALSE)

  rechazamos = (chi2_calc > chi2_crit)

  # Tabla resumen
  tabla = data.frame(
    Categoria  = etiquetas,
    Observada  = observadas,
    Esperada   = round(esperadas, 4),
    Contribucion = round((observadas - esperadas)^2 / esperadas, 4)
  )

  resultado = list(
    tabla       = tabla,
    chi2_calc   = chi2_calc,
    gl          = gl,
    chi2_crit   = chi2_crit,
    p_valor     = p_valor,
    alfa        = alfa,
    rechaza_H0  = rechazamos
  )

  return(resultado)
}


# =====================================================================
# bondad_ajuste_poisson
# Bondad de ajuste para una distribucion Poisson con lambda conocido.
# Calcula las probabilidades teoricas y las frecuencias esperadas
# a partir de los intervalos dados, luego llama a bondad_ajjuste().
# ---
# parametros:
#   limites_inf  Vector de limites inferiores de cada intervalo (enteros)
#   limites_sup  Vector de limites superiores (usar Inf para la ultima cola)
#   observadas   Vector de frecuencias observadas
#   lambda       Tasa Poisson bajo H0
#   n            Tamano de muestra total
#   alfa         Nivel de significancia (default 0.05)
#   etiquetas    Nombres para las categorias (opcional)
# ---
bondad_ajuste_poisson = function(limites_inf, limites_sup,
                                  observadas, lambda, n,
                                  alfa = 0.05, etiquetas = NULL) {
  k = length(observadas)

  # Probabilidades teoricas por intervalo
  probs = numeric(k)
  for (i in seq_len(k)) {
    if (is.infinite(limites_sup[i])) {
      probs[i] = ppois(limites_inf[i] - 1, lambda = lambda,
                       lower.tail = FALSE)
    } else {
      probs[i] = ppois(limites_sup[i], lambda = lambda) -
                 ppois(limites_inf[i] - 1, lambda = lambda)
    }
  }

  esperadas = n * probs

  if (is.null(etiquetas)) {
    etiquetas = paste0(limites_inf, " a ",
                       ifelse(is.infinite(limites_sup),
                              "mas", limites_sup))
  }

  res = bondad_ajuste(observadas, esperadas, alfa, etiquetas)
  res$lambda   = lambda
  res$probs    = probs
  res$n        = n
  return(res)
}


# =====================================================================
# prueba_independencia
# Contraste de independencia chi-cuadrada para una tabla de contingencia.
# ---
# Parametros:
#   tabla_obs  Matriz o data.frame con frecuencias observadas
#              (filas = niveles var1, columnas = niveles var2)
#   alfa       Nivel de significancia (default 0.05)
# ---
prueba_independencia = function(tabla_obs, alfa = 0.05) {
  tabla_obs = as.matrix(tabla_obs)

  totales_fila = rowSums(tabla_obs)
  totales_col  = colSums(tabla_obs)
  n_total      = sum(tabla_obs)

  # Frecuenciias esperadas: E_ij = (total_fila_i * total_col_j) / n
  tabla_esp = outer(totales_fila, totales_col) / n_total

  chi2_calc = sum((tabla_obs - tabla_esp)^2 / tabla_esp)

  r  = nrow(tabla_obs)
  c  = ncol(tabla_obs)
  gl = (r - 1) * (c - 1)

  chi2_crit  = qchisq(alfa, df = gl, lower.tail = FALSE)
  p_valor    = pchisq(chi2_calc, df = gl, lower.tail = FALSE)

  rechazamos = (chi2_calc > chi2_crit)

  resultado = list(
    tabla_observada  = tabla_obs,
    tabla_esperada   = round(tabla_esp, 4),
    contribuciones   = round((tabla_obs - tabla_esp)^2 / tabla_esp, 4),
    chi2_calc        = chi2_calc,
    gl               = gl,
    chi2_crit        = chi2_crit,
    p_valor          = p_valor,
    alfa             = alfa,
    rechaza_H0       = rechazamos
  )

  return(resultado)
}


# =====================================================================
# prueba_homogeneidad
# Contraste de homogeneidad chi-cuadrada.
# Misma mecanica que independencia; se separa por claridad conceptual.
# ---
# Parametros:
#   tabla_obs  Matriz con frecuencias observadas
#              (filas = poblaciones, columnas = categorias de respuesta)
#   alfa       Nivel de significancia (default 0.05)
# ---
prueba_homogeneidad = function(tabla_obs, alfa = 0.05) {
  res = prueba_independencia(tabla_obs, alfa)
  res$tipo = "homogeneidad"
  return(res)
}


# =====================================================================
# imprimir_resultado_chi2
# Imprime de forma legible el resultado de cualquiera de las pruebas
# ---
imprimir_resultado_chi2 = function(res, titulo = "Contraste chi-cuadrada") {
  cat("\n", paste(rep("=", 55), collapse = ""), "\n")
  cat(" ", titulo, "\n")
  cat(paste(rep("=", 55), collapse = ""), "\n")

  if (!is.null(res$tabla)) {
    cat("\nTabla de frecuencias:\n")
    print(res$tabla)
  }
  if (!is.null(res$tabla_observada)) {
    cat("\nFrecuencias observadas:\n")
    print(res$tabla_observada)
    cat("\nFrecuencias esperadas:\n")
    print(res$tabla_esperada)
    cat("\nContribuciones a chi-cuadrada:\n")
    print(res$contribuciones)
  }

  cat(sprintf("\nchi2 calculado : %.4f\n", res$chi2_calc))
  cat(sprintf("Grados libertad: %d\n",     res$gl))
  cat(sprintf("chi2 critico   : %.4f  (alfa = %.2f)\n",
              res$chi2_crit, res$alfa))
  cat(sprintf("p-valor        : %.6f\n",   res$p_valor))

  if (res$rechaza_H0) {
    cat(sprintf("\nDecision: Se rechaza H0 al nivel alfa = %.2f\n", res$alfa))
  } else {
    cat(sprintf("\nDecision: No se rechaza H0 al nivel alfa = %.2f\n", res$alfa))
  }
  cat(paste(rep("-", 55), collapse = ""), "\n")
}
