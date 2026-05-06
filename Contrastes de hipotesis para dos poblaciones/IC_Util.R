# =========================================================
# Archivo: IC_Util.R
# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
# Oscar Ivan Gomez Ruiz
#
# Utilerias para contraste de hipótesis de dos poblaciones
# =========================================================

# --- FUNCIÓN 1: CONTRASTE DE MEDIAS (Z) ---
contraste_hipotesis_z_2pob <- function(n1, xbar1, var1, n2, xbar2, var2, alpha, tipo_contraste) {
    txt_datos <- sprintf("== Identificación de datos ==\n   Población 1: n1 = %g, Media = %g, Varianza = %g\n   Población 2: n2 = %g, Media = %g, Varianza = %g\n   Nivel de significancia (alpha): %g", n1, xbar1, var1, n2, xbar2, var2, alpha)

  # Cálculos intermedios
  diff_medias <- xbar1 - xbar2
  error_estandar <- sqrt((var1 / n1) + (var2 / n2))
  z_calc <- diff_medias / error_estandar
  
  txt_est <- sprintf("== Cálculos intermedios ==\n Diferencia de medias = %g - %g = %g\n Error estandar = sqrt((%g / %g) + (%g + %g)) = %g\n Z calculado = %g / %g = %g", xbar1, xbar2, diff_medias, var1, n1, var2, n2, error_estandar, diff_medias, error_estandar, z_calc)
  
  # Evaluación según el tipo de contraste
  if (tipo_contraste == "bilateral") {
    # H0: mu1 - mu2 = 0 vs Ha: mu1 - mu2 != 0
    z_crit_inf = qnorm(alpha / 2)
    z_crit_sup = qnorm(1 - alpha / 2)
    z_crit <- c(z_crit_inf, z_crit_sup)
    p_valor <- 2 * (1 - pnorm(abs(z_calc)))
    rechazo <- z_calc < z_crit_inf | z_calc > z_crit_sup
    
    txt_hip <- "== Planteamiento de hipótesis ==\n   H0: mu1 - mu2 = 0\n   Ha: mu1 - mu2 != 0"
    txt_crit <- sprintf("== Valor crítico ==\n   Z inferior = %g, Z superior = %g", z_crit_inf, z_crit_sup)
    txt_regla <- sprintf("== Regla de decisión ==\n   Rechazar H0 si Z calculado < %g o Z calculado > %g", z_crit_inf, z_crit_sup)
    
  } else if (tipo_contraste == "mayor_igual") {
    # H0: mu1 - mu2 >= 0 vs Ha: mu1 - mu2 < 0
    z_crit <- qnorm(alpha)
    p_valor <- pnorm(z_calc)
    rechazo <- z_calc < z_crit
    
    txt_hip <- "== Planteamiento de hipótesis ==\n   H0: mu1 - mu2 >= 0\n   Ha: mu1 - mu2 < 0"
    txt_crit <- sprintf("== Valor crítico ==\n   Z = %g", z_crit)
    txt_regla <- sprintf("== Regla de decisión==\n   Rechazar H0 si Z calculado < %g", z_crit)
    
  } else if (tipo_contraste == "menor_igual") {
    # H0: mu1 - mu2 <= 0 vs Ha: mu1 - mu2 > 0
    z_crit <- qnorm(1 - alpha)
    p_valor <- 1 - pnorm(z_calc)
    rechazo <- z_calc > z_crit
    
    txt_hip <- "== Planteamiento de hipótesis ==\n   H0: mu1 - mu2 <= 0\n   Ha: mu1 - mu2 > 0"
    txt_crit <- sprintf("== Valor crítico ==\n   Z = %g", z_crit)
    txt_regla <- sprintf("== Regla de decisión ==\n   Rechazar H0 si Z calculado > %g", z_crit)
    
  } else {
    stop("Tipo de contraste no válido. Opciones: 'bilateral', 'mayor_igual', 'menor_igual'.")
  }
  
  txt_interp <- sprintf("== Interpretación final ==\n   %s", ifelse(rechazo, "Se rechaza la hipótesis nula (H0).", "No se rechaza la hipótesis nula (H0)."))
  
  procedimiento_txt <- paste(txt_datos, txt_hip, txt_est, txt_crit, txt_regla, txt_interp, sep = "\n\n")
  
  # Construcción de la lista de salida
  resultados = list(
    diferencia_medias = diff_medias,
    error_estandar = error_estandar,
    z_calculado = z_calc,
    z_critico = z_crit,
    p_valor = p_valor,
    rechazar_H0 = rechazo,
    procedimiento = procedimiento_txt
  )
  
  return(resultados)
}

# --- FUNCIÓN 2: CONTRASTE DE PROPORCIONES (Z) ---
contraste_proporciones_z_2pob <- function(n1, x1, n2, x2, alpha, tipo_contraste) {
  p1 <- x1/n1
  p2 <- x2/n2
  p_pool <- (x1 + x2) / (n1 + n2)
  error_estandar <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
  z_calc <- (p1 - p2) / error_estandar
  
  txt_datos <- sprintf("== Identificación de Datos==\n    P1: x = %g, n = %g, proporcion = %.4f\n    P2: x = %g, n = %g, proporcion = %.4f\n    Alpha=%g\n    Proporción Agrupada = (%g + %g) / (%g + %g) = %g\n    Error Estandar = sqrt(%g * (1 - %g) * (1/%g + 1/%g)) = %g\n    Z Calculado = (%g - %g) / %g = %g", x1, n1, p1, x2, n2, p2, alpha, x1, x2, n1, n2, p_pool, p_pool, p_pool, n1, n2, error_estandar, p1, p2, error_estandar, z_calc)
  
  if (tipo_contraste == "mayor_igual") { # Ha: p1 < p2
    z_crit <- qnorm(alpha)
    rechazo <- z_calc < z_crit
    
    txt_hip <- "== Planteamiento de hipótesis ==\n    H0: p1 >= p2\n    Ha: p1 < p2"
    txt_crit <- sprintf("== Valor crítico ==\n   Z = %g", z_crit)
    txt_regla <- sprintf("== Regla de decisión==\n   Rechazar H0 si Z calculado < %g", z_crit)
    
  } else if (tipo_contraste == "menor_igual") { # Ha: p1 > p2
    z_crit <- qnorm(1 - alpha)
    rechazo <- z_calc > z_crit
    
    txt_hip <- "== Planteamiento de hipótesis ==\n    H0: p1 <= p2\n    Ha: p1 > p2"
    txt_crit <- sprintf("== Valor crítico ==\n   Z = %g", z_crit)
    txt_regla <- sprintf("== Regla de decisión ==\n   Rechazar H0 si Z calculado > %g", z_crit)
  } else {
    z_crit <- c(qnorm(alpha/2), qnorm(1-alpha/2))
    rechazo <- z_calc < z_crit[1] | z_calc > z_crit[2]
    
    txt_hip <- "== Planteamiento de hipótesis ==\n    H0: p1 = p2\n    Ha: p1 != p2"
    txt_crit <- sprintf("== Valor crítico ==\n   Z inferior = %g, Z superior = %g", z_crit[1], z_crit[2])
    txt_regla <- sprintf("== Regla de decisión ==\n   Rechazar H0 si Z calculado < %g o Z calculado > %g", z_crit[1], z_crit[2])
  }
  
  proc <- paste(txt_datos, txt_hip, txt_crit, txt_regla, sprintf("== Interpretación final ==\n    %s", ifelse(rechazo, "Se rechaza la hipótesis nula (H0).", "No se rechaza la hipótesis nula (H0).")), sep="\n\n")
  
  resultados = list(
    p1 = p1,
    p2 = p2,
    p_pool = p_pool,
    error_estandar = error_estandar,
    z_calc = z_calc,
    z_crit = z_crit,
    rechazar = rechazo,
    procedimiento = proc
  )
  return(resultados)
}

# --- FUNCIÓN 3: CONTRASTE DE VARIANZAS (F) ---
contraste_varianzas_f_2pob <- function(n1, var1, n2, var2, alpha, tipo_contraste) {
  f_calc <- var1 / var2
  df1 <- n1 - 1
  df2 <- n2 - 1
  
  txt_datos <- sprintf("== Identificación de datos ==\n    n1 = %g, S1^2 = %g, gradosLibertad1 = %g\n    n2 = %g, S2^2 = %g, gradosLibertad2 = %g\n    Alpha = %g\n    F calculado = %g / %g = %g", n1, var1, df1, n2, var2, df2, alpha, var1, var2, f_calc)
  
  if (tipo_contraste == "menor_igual") { # Ha: var1 > var2
    f_crit <- qf(1 - alpha, df1, df2)
    rechazo <- f_calc > f_crit
    
    txt_hip <- "== Planteamiento de hipótesis ==\n    H0: v1 <= v2\n    Ha: v1 > v2"
    txt_crit <- sprintf("== Valor crítico ==\n   F = %g", f_crit)
    txt_regla <- sprintf("== Regla de decisión ==\n   Rechazar H0 si F calculado > %g", f_crit)
    
  } else if (tipo_contraste == "mayor_igual") { # Ha: var1 < var2
    f_crit <- qf(alpha, df1, df2)
    rechazo <- f_calc < f_crit
    
    txt_hip <- "== Planteamiento de hipótesis ==\n    H0: v1 >= v2\n    Ha: v1 < v2"
    txt_crit <- sprintf("== Valor crítico ==\n   F = %g", f_crit)
    txt_regla <- sprintf("== Regla de decisión==\n   Rechazar H0 si F calculado < %g", f_crit)
    
  } else {
    f_crit <- c(qf(alpha/2, df1, df2), qf(1-alpha/2, df1, df2))
    rechazo <- f_calc < f_crit[1] | f_calc > f_crit[2]
    
    txt_hip <- "== Planteamiento de hipótesis ==\n    H0: v1 = v2\n    Ha: v1 != v2"
    txt_crit <- sprintf("== Valor crítico ==\n   F inferior = %g, F superior = %g", f_crit[1], f_crit[2])
    txt_regla <- sprintf("== Regla de decisión ==\n   Rechazar H0 si F calculado < %g o F calculado > %g", f_crit[1], f_crit[2])
    
  }
  
  proc <- paste(txt_datos, txt_hip, txt_crit, txt_regla, sprintf("== Interpretación final ==\n    %s", ifelse(rechazo, "Se rechaza la hipótesis nula (H0).", "No se rechaza la hipótesis nula (H0).")), sep="\n\n")
  
  resultados = list(
    df1 = df1,
    df2 = df2,
    f_calc = f_calc,
    f_crit = f_crit,
    rechazar = rechazo,
    procedimiento = proc
  )
  return(resultados)
}