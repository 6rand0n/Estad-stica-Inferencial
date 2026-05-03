# Universidad Autónoma de Aguascalientes
# Centro de Ciencias Básicas
# Departamento de Estadística
# Ing. En Sistemas Computacionales
# Inferencia Estadística
# Francisco Javier Sanchez Vallin
# ==============================================================================
# CONTRASTE DE HIPÓTESIS PARA UNA PROPORCIÓN
contraste_prop = function(Ha, p0, x, n, alfa = 0.05) {
  
  # Proporción muestral
  p_gorro = x / n
  
  # Estadístico de prueba Z
  Z_calc = (p_gorro - p0) / sqrt(p0 * (1 - p0) / n)
  
  if (Ha == "<") {
    Z_crit = qnorm(alfa)
    rechaza = Z_calc < Z_crit
    
  } else if (Ha == ">") {
    Z_crit = qnorm(1 - alfa)
    rechaza = Z_calc > Z_crit
    
  } else {
    Z_crit = qnorm(1 - alfa / 2)
    rechaza = abs(Z_calc) > Z_crit
  }
  
  # Resultado
  resultado = list(
    parametro = "p",
    estadistico = "Z",
    p_muestral = p_gorro,
    Z_calculado = Z_calc,
    Z_critico = Z_crit,
    rechaza_H0 = rechaza
  )
  
  return(resultado)
}
# ==============================================================================
# CONTRASTE PARA LA MEDIA (SIGMA CONOCIDA)
contraste_media_z = function(Ha, mu0, x_barra, sigma, n, alfa = 0.05) {
  
  # Estadístico Z
  Z_calc = (x_barra - mu0) / (sigma / sqrt(n))
  
  if (Ha == "<") {
    Z_crit = qnorm(alfa)
    rechaza = Z_calc < Z_crit
    
  } else if (Ha == ">") {
    Z_crit = qnorm(1 - alfa)
    rechaza = Z_calc > Z_crit
    
  } else {
    Z_crit = qnorm(1 - alfa / 2)
    rechaza = abs(Z_calc) > Z_crit
  }
  
  resultado = list(
    parametro = "mu",
    estadistico = "Z",
    Z_calculado = Z_calc,
    Z_critico = Z_crit,
    rechaza_H0 = rechaza
  )
  
  return(resultado)
}
# ==============================================================================
# CONTRASTE PARA LA MEDIA (SIGMA DESCONOCIDA)
contraste_media_t = function(Ha, mu0, x_barra, s, n, alfa = 0.05) {
  
  # Estadístico t
  t_calc = (x_barra - mu0) / (s / sqrt(n))
  gl = n - 1
  
  if (Ha == "<") {
    t_crit = qt(alfa, gl)
    rechaza = t_calc < t_crit
    
  } else if (Ha == ">") {
    t_crit = qt(1 - alfa, gl)
    rechaza = t_calc > t_crit
    
  } else {
    t_crit = qt(1 - alfa / 2, gl)
    rechaza = abs(t_calc) > t_crit
  }
  
  resultado = list(
    parametro = "mu",
    estadistico = "t",
    t_calculado = t_calc,
    t_critico = t_crit,
    grados_libertad = gl,
    rechaza_H0 = rechaza
  )
  
  return(resultado)
}
# ==============================================================================