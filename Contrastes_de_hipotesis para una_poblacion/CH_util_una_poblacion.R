# Universidad Autónoma de Aguascalientes
# Centro de Ciencias Básicas
# Departamento de Estadística
# Ing. En Sistemas Computacionales
# Inferencia Estadística
# Francisco Javier Sanchez Vallin
# ==============================================================================
# CONTRASTE DE HIPÓTESIS PARA UNA PROPORCIÓN
contraste_prop <- function(Ha, p0, x, n, alfa = 0.05) {
  
  # Se calcula la proporción muestral
  p_gorro <- x / n
  
  # Estadístico Z para proporción
  Z_calc <- (p_gorro - p0) / sqrt(p0 * (1 - p0) / n)
  
  # Dependiendo del tipo de hipótesis alternativa
  if (Ha == "<") {
    # Cola izquierda
    Z_crit <- qnorm(alfa)
    rechaza <- Z_calc < Z_crit
    
  } else if (Ha == ">") {
    # Cola derecha
    Z_crit <- qnorm(1 - alfa)
    rechaza <- Z_calc > Z_crit
    
  } else {
    # Dos colas
    Z_crit <- qnorm(1 - alfa / 2)
    rechaza <- abs(Z_calc) > Z_crit
  }
  
  # Se devuelven todos los resultados en una lista
  return(list(
    parametro = "p",
    estadistico = "Z",
    p_muestral = p_gorro,
    Z_calculado = Z_calc,
    Z_critico = Z_crit,
    rechaza_H0 = rechaza
  ))
}
# ==============================================================================
# CONTRASTE PARA LA MEDIA (SIGMA CONOCIDA)
contraste_media_z <- function(Ha, mu0, x_barra, sigma, n, alfa = 0.05) {
  
  # Estadístico Z para la media con sigma conocida
  Z_calc <- (x_barra - mu0) / (sigma / sqrt(n))
  
  if (Ha == "<") {
    Z_crit <- qnorm(alfa)
    rechaza <- Z_calc < Z_crit
    
  } else if (Ha == ">") {
    Z_crit <- qnorm(1 - alfa)
    rechaza <- Z_calc > Z_crit
    
  } else {
    Z_crit <- qnorm(1 - alfa / 2)
    rechaza <- abs(Z_calc) > Z_crit
  }
  
  return(list(
    parametro = "mu",
    estadistico = "Z",
    Z_calculado = Z_calc,
    Z_critico = Z_crit,
    rechaza_H0 = rechaza
  ))
}
# ==============================================================================

# CONTRASTE PARA LA MEDIA (SIGMA DESCONOCIDA)
contraste_media_t <- function(Ha, mu0, x_barra, s, n, alfa = 0.05) {
  
  # Se calcula el estadístico t
  t_calc <- (x_barra - mu0) / (s / sqrt(n))
  gl <- n - 1   # grados de libertad
  
  if (Ha == "<") {
    t_crit <- qt(alfa, gl)
    rechaza <- t_calc < t_crit
    
  } else if (Ha == ">") {
    t_crit <- qt(1 - alfa, gl)
    rechaza <- t_calc > t_crit
    
  } else {
    t_crit <- qt(1 - alfa / 2, gl)
    rechaza <- abs(t_calc) > t_crit
  }
  
  return(list(
    parametro = "mu",
    estadistico = "t",
    t_calculado = t_calc,
    t_critico = t_crit,
    grados_libertad = gl,
    rechaza_H0 = rechaza
  ))
}
# ==============================================================================