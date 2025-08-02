library('tidyverse')

# Probabilidad de robar 2 marincess y ni un ladrillo
d <- 43 #tamano del mazo
m <- 22 #marincess en mazo
n <- d-m #cartas no marincess del mazo (no starters o extenders)
k <- 5 #tamano mano inicial
l <- 2 #ladrillos en el mazo


f <- function(d, n) {
  if (d - n >= 0 && n >= l && d >= 5) {
    p <- dhyper(x = 2, m = d - n, n = n, k = 2) * dhyper(x = 0, m = l, n = n - l, k = 3)
  } else {
    p <- NA  # Usar NA para combinaciones no válidas
  }
  return(p)
}

# Crear un data.frame con todas las combinaciones de d y n
combinations <- expand.grid(d = 40:60, n = 18:38)

# Evaluar la función f para cada combinación y agregar el resultado al data.frame
combinations$p <- mapply(f, combinations$d, combinations$n)

# Filtrar por aquellos valores no validos
combinations <- combinations %>% filter(d-n==m) 


# Mostrar el data.frame resultante
head(combinations)


which.max(combinations$p)

#### Optimizando la mano madolche ####
# Definición de los tamaños de cada grupo# Definición de los tamaños de cada grupo
l  <- 4    # ladrillos 
ms <- 10   # madolches starters
e  <- 7    # Engine 
bb <- 12   # Boardbreakers
h  <- 14   # Non-engine
d  <- ms + l + bb + e + h  # Tamaño total del mazo
otros <- d - ms - bb       # Cartas que no son ni ms ni bb

# Función para calcular la probabilidad acumulada P(X_ms <= x, X_bb <= y)
p_acumulada <- function(x, y, n) {
  # Verificar que los parámetros sean coherentes
  if (x > ms || y > bb || n > d) {
    stop("Los valores de x, y o n exceden los límites permitidos.")
  }
  
  # Inicializar la probabilidad acumulada
  prob_total <- 0
  
  # Sumar todas las probabilidades P(X_ms = i, X_bb = j) para i ≤ x, j ≤ y
  for (i in 0:x) {
    for (j in 0:y) {
      if (i + j <= n) {  # Asegurar que no se extraigan más cartas de las disponibles
        prob_total <- prob_total + (choose(ms, i) * choose(bb, j) * choose(otros, n - i - j)) / choose(d, n)
      }
    }
  }
  
  return(prob_total)
}

# Ejemplo de uso:
# Calcular la probabilidad acumulada P(X_ms <= 3, X_bb <= 2) en una mano de 7 cartas
resultado <- p_acumulada(x = 3, y = 2, n = 6)
print(resultado)


max_p <- p_acumulada(d, h)
for (d_test in 60:40) {
  for (h_test in (40-l-e-o):(d_test-l-e-o)) {
      if (max_p < p_mano(d_test, h_test)) {
        max_p <- p_mano(d_test, h_test)
        ratios_optimos <- paste("deck = ", d_test, ", ",
                                "Non-Engine = ", h_test)
      } 
    }
}

paste(ratios_optimos, max_p, sep = " ; ")

### Marincess - Ice doll ####

e <- 25 #engine
b <- 4 # bricks
h <- 14 #handtraps
d <- e+b+h

#P(2 handtraps, 3 engine 0 bricks)

f <- function(e, h) {
  
  p <- dhyper(x = 3, m = e, n = b+h, k = 3) * #P(robar 3 engine)
    dhyper(x = 2, m = h, n = e+b-3, k = 2) #P(robar 2 handtraps)
  
  return(p)
}

val_ref <- f(e, h)
for (h_t in 12:(60-e)) {
  if (val_ref < f(e, h_t)) {
    val_ref <- f(e, h_t)
    print(paste("Cantidad optima de handtraps = ", h_t))
  } else {next}
}

print(val_ref)
