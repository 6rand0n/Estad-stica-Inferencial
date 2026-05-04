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

contraste_hipotesis_z_2pob <- function(n1, xbar1, var1, n2, xbar2, var2, alpha, tipo_contraste) {
  
  # Cálculos intermedios
  diff_medias <- xbar1 - xbar2
  error_estandar <- sqrt((var1 / n1) + (var2 / n2))
  z_calc <- diff_medias / error_estandar
  
  # Evaluación según el tipo de contraste
  if (tipo_contraste == "bilateral") {
    # H0: mu1 - mu2 = 0 vs Ha: mu1 - mu2 != 0
    z_crit_inf = qnorm(alpha / 2)
    z_crit_sup = qnorm(1 - alpha / 2)
    z_crit <- c(z_crit_inf, z_crit_sup)
    p_valor <- 2 * (1 - pnorm(abs(z_calc)))
    rechazo <- z_calc < z_crit_inf | z_calc > z_crit_sup
    
  } else if (tipo_contraste == "mayor_igual") {
    # H0: mu1 - mu2 >= 0 vs Ha: mu1 - mu2 < 0
    z_crit <- qnorm(alpha)
    p_valor <- pnorm(z_calc)
    rechazo <- z_calc < z_crit
    
  } else if (tipo_contraste == "menor_igual") {
    # H0: mu1 - mu2 <= 0 vs Ha: mu1 - mu2 > 0
    z_crit <- qnorm(1 - alpha)
    p_valor <- 1 - pnorm(z_calc)
    rechazo <- z_calc > z_crit
    
  } else {
    stop("Tipo de contraste no válido. Opciones: 'bilateral', 'mayor_igual', 'menor_igual'.")
  }
  
  # Construcción de la lista de salida
  resultados = list(
    diferencia_medias = diff_medias,
    error_estandar = error_estandar,
    z_calculado = z_calc,
    z_critico = z_crit,
    p_valor = p_valor,
    rechazar_H0 = rechazo
  )
  
  return(resultados)
}