# Universidad Autónoma de Aguascalientes
# Centro de Ciencias Básicas
# Departamento de Estadística
# Ing. En Sistemas Computacionales
# Inferencia Estadística
# Francisco Javier Sanchez Vallin
# ==============================================================================
# CONTRASTE DE HIPÓTESIS PARA UNA PROPORCIÓN
contraste_prop = function(Ha, p0, x, n, alfa = 0.05) {
  
  cat("\n")
  cat("========== CONTRASTE DE HIPÓTESIS PARA UNA PROPORCIÓN ==========\n")
  cat("\n--- PARÁMETROS DE ENTRADA ---\n")
  cat("Hipótesis alternativa (Ha):", Ha, "\n")
  cat("Proporción hipotética (p₀):", p0, "\n")
  cat("Número de éxitos (x):", x, "\n")
  cat("Tamaño de muestra (n):", n, "\n")
  cat("Nivel de significancia (α):", alfa, "\n")
  
  cat("\n--- PROCEDIMIENTO DE CÁLCULO ---\n")
  
  # Proporción muestral
  p_gorro = x / n
  cat("\n1. Proporción muestral (p̂ = x/n):\n")
  cat("   p̂ =", x, "/", n, "=", p_gorro, "\n")
  
  # Estadístico de prueba Z
  cat("\n2. Estadístico de prueba Z:\n")
  cat("   Fórmula: Z = (p̂ - p₀) / √[p₀(1-p₀)/n]\n")
  
  numerador = p_gorro - p0
  denominador = sqrt(p0 * (1 - p0) / n)
  Z_calc = numerador / denominador
  
  cat("   Numerador: p̂ - p₀ =", p_gorro, "-", p0, "=", numerador, "\n")
  cat("   Denominador: √[", p0, "(1-", p0, ")/", n, "] = √[", p0*(1-p0)/n, "] =", denominador, "\n")
  cat("   Z calculado =", numerador, "/", denominador, "=", Z_calc, "\n")
  
  cat("\n3. Valor crítico (Z crítico):\n")
  
  if (Ha == "<") {
    Z_crit = qnorm(alfa)
    rechaza = Z_calc < Z_crit
    cat("   Prueba unilateral izquierda (Ha: p < p₀)\n")
    cat("   Z_crítico = Z_α = qnorm(", alfa, ") =", Z_crit, "\n")
    cat("   Región crítica: Z <", Z_crit, "\n")
    
  } else if (Ha == ">") {
    Z_crit = qnorm(1 - alfa)
    rechaza = Z_calc > Z_crit
    cat("   Prueba unilateral derecha (Ha: p > p₀)\n")
    cat("   Z_crítico = Z_(1-α) = qnorm(", 1-alfa, ") =", Z_crit, "\n")
    cat("   Región crítica: Z >", Z_crit, "\n")
    
  } else {
    Z_crit = qnorm(1 - alfa / 2)
    rechaza = abs(Z_calc) > Z_crit
    cat("   Prueba bilateral (Ha: p ≠ p₀)\n")
    cat("   Z_crítico = Z_(1-α/2) = qnorm(", 1-alfa/2, ") =", Z_crit, "\n")
    cat("   Región crítica: |Z| >", Z_crit, "\n")
  }
  
  cat("\n--- COMPARACIÓN Y DECISIÓN ---\n")
  cat("Z calculado =", Z_calc, "\n")
  cat("Z crítico =", Z_crit, "\n")
  
  if (Ha == "<") {
    cat("¿", Z_calc, "<", Z_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  } else if (Ha == ">") {
    cat("¿", Z_calc, ">", Z_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  } else {
    cat("¿|", Z_calc, "| >", Z_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
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
  
  cat("\n==============================================================\n\n")
  
  return(resultado)
}
# ==============================================================================
# CONTRASTE PARA LA MEDIA (SIGMA CONOCIDA)
contraste_media_z = function(Ha, mu0, x_barra, sigma, n, alfa = 0.05) {
  
  cat("\n")
  cat("========== CONTRASTE DE HIPÓTESIS PARA LA MEDIA (σ CONOCIDA) ==========\n")
  cat("\n--- PARÁMETROS DE ENTRADA ---\n")
  cat("Hipótesis alternativa (Ha):", Ha, "\n")
  cat("Media hipotética (μ₀):", mu0, "\n")
  cat("Media muestral (x̄):", x_barra, "\n")
  cat("Desviación estándar poblacional (σ):", sigma, "\n")
  cat("Tamaño de muestra (n):", n, "\n")
  cat("Nivel de significancia (α):", alfa, "\n")
  
  cat("\n--- PROCEDIMIENTO DE CÁLCULO ---\n")
  
  cat("\n1. Estadístico de prueba Z:\n")
  cat("   Fórmula: Z = (x̄ - μ₀) / (σ / √n)\n")
  
  numerador = x_barra - mu0
  denominador = sigma / sqrt(n)
  Z_calc = numerador / denominador
  
  cat("   Numerador: x̄ - μ₀ =", x_barra, "-", mu0, "=", numerador, "\n")
  cat("   Denominador: σ / √n =", sigma, "/ √", n, "=", sigma, "/", sqrt(n), "=", denominador, "\n")
  cat("   Z calculado =", numerador, "/", denominador, "=", Z_calc, "\n")
  
  cat("\n2. Valor crítico (Z crítico):\n")
  
  if (Ha == "<") {
    Z_crit = qnorm(alfa)
    rechaza = Z_calc < Z_crit
    cat("   Prueba unilateral izquierda (Ha: μ < μ₀)\n")
    cat("   Z_crítico = Z_α = qnorm(", alfa, ") =", Z_crit, "\n")
    cat("   Región crítica: Z <", Z_crit, "\n")
    
  } else if (Ha == ">") {
    Z_crit = qnorm(1 - alfa)
    rechaza = Z_calc > Z_crit
    cat("   Prueba unilateral derecha (Ha: μ > μ₀)\n")
    cat("   Z_crítico = Z_(1-α) = qnorm(", 1-alfa, ") =", Z_crit, "\n")
    cat("   Región crítica: Z >", Z_crit, "\n")
    
  } else {
    Z_crit = qnorm(1 - alfa / 2)
    rechaza = abs(Z_calc) > Z_crit
    cat("   Prueba bilateral (Ha: μ ≠ μ₀)\n")
    cat("   Z_crítico = Z_(1-α/2) = qnorm(", 1-alfa/2, ") =", Z_crit, "\n")
    cat("   Región crítica: |Z| >", Z_crit, "\n")
  }
  
  cat("\n--- COMPARACIÓN Y DECISIÓN ---\n")
  cat("Z calculado =", Z_calc, "\n")
  cat("Z crítico =", Z_crit, "\n")
  
  if (Ha == "<") {
    cat("¿", Z_calc, "<", Z_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  } else if (Ha == ">") {
    cat("¿", Z_calc, ">", Z_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  } else {
    cat("¿|", Z_calc, "| >", Z_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  }
  
  resultado = list(
    parametro = "mu",
    estadistico = "Z",
    Z_calculado = Z_calc,
    Z_critico = Z_crit,
    rechaza_H0 = rechaza
  )
  
  cat("\n=======================================================================\n\n")
  
  return(resultado)
}
# ==============================================================================
# CONTRASTE PARA LA MEDIA (SIGMA DESCONOCIDA)
contraste_media_t = function(Ha, mu0, x_barra, s, n, alfa = 0.05) {
  
  cat("\n")
  cat("========== CONTRASTE DE HIPÓTESIS PARA LA MEDIA (σ DESCONOCIDA) ==========\n")
  cat("\n--- PARÁMETROS DE ENTRADA ---\n")
  cat("Hipótesis alternativa (Ha):", Ha, "\n")
  cat("Media hipotética (μ₀):", mu0, "\n")
  cat("Media muestral (x̄):", x_barra, "\n")
  cat("Desviación estándar muestral (s):", s, "\n")
  cat("Tamaño de muestra (n):", n, "\n")
  cat("Nivel de significancia (α):", alfa, "\n")
  
  cat("\n--- PROCEDIMIENTO DE CÁLCULO ---\n")
  
  cat("\n1. Grados de libertad:\n")
  gl = n - 1
  cat("   gl = n - 1 =", n, "- 1 =", gl, "\n")
  
  cat("\n2. Estadístico de prueba t:\n")
  cat("   Fórmula: t = (x̄ - μ₀) / (s / √n)\n")
  
  numerador = x_barra - mu0
  denominador = s / sqrt(n)
  t_calc = numerador / denominador
  
  cat("   Numerador: x̄ - μ₀ =", x_barra, "-", mu0, "=", numerador, "\n")
  cat("   Denominador: s / √n =", s, "/ √", n, "=", s, "/", sqrt(n), "=", denominador, "\n")
  cat("   t calculado =", numerador, "/", denominador, "=", t_calc, "\n")
  
  cat("\n3. Valor crítico (t crítico) con gl =", gl, ":\n")
  
  if (Ha == "<") {
    t_crit = qt(alfa, gl)
    rechaza = t_calc < t_crit
    cat("   Prueba unilateral izquierda (Ha: μ < μ₀)\n")
    cat("   t_crítico = t_α,", gl, "= qt(", alfa, ",", gl, ") =", t_crit, "\n")
    cat("   Región crítica: t <", t_crit, "\n")
    
  } else if (Ha == ">") {
    t_crit = qt(1 - alfa, gl)
    rechaza = t_calc > t_crit
    cat("   Prueba unilateral derecha (Ha: μ > μ₀)\n")
    cat("   t_crítico = t_(1-α),", gl, "= qt(", 1-alfa, ",", gl, ") =", t_crit, "\n")
    cat("   Región crítica: t >", t_crit, "\n")
    
  } else {
    t_crit = qt(1 - alfa / 2, gl)
    rechaza = abs(t_calc) > t_crit
    cat("   Prueba bilateral (Ha: μ ≠ μ₀)\n")
    cat("   t_crítico = t_(1-α/2),", gl, "= qt(", 1-alfa/2, ",", gl, ") =", t_crit, "\n")
    cat("   Región crítica: |t| >", t_crit, "\n")
  }
  
  cat("\n--- COMPARACIÓN Y DECISIÓN ---\n")
  cat("t calculado =", t_calc, "\n")
  cat("t crítico =", t_crit, "\n")
  
  if (Ha == "<") {
    cat("¿", t_calc, "<", t_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  } else if (Ha == ">") {
    cat("¿", t_calc, ">", t_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  } else {
    cat("¿|", t_calc, "| >", t_crit, "?", rechaza, "\n")
    if (rechaza) {
      cat("DECISIÓN: SÍ cae en la región crítica → RECHAZAR H₀\n")
    } else {
      cat("DECISIÓN: NO cae en la región crítica → NO RECHAZAR H₀\n")
    }
  }
  
  resultado = list(
    parametro = "mu",
    estadistico = "t",
    t_calculado = t_calc,
    t_critico = t_crit,
    grados_libertad = gl,
    rechaza_H0 = rechaza
  )
  
  cat("\n========================================================================\n\n")
  
  return(resultado)
}
# ==============================================================================