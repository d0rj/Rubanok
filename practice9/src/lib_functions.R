proportion <- function(x, dt, func) {
  stopifnot(is.vector(x) && class(x) == "numeric")
  stopifnot(is.numeric(dt) & dt %% 1 == 0 & dt > 0)
  stopifnot(2*dt < length(x))
  stopifnot(is.function(func))
  
  min_elem <- min(x)
  x <- sapply(x, function(x) x + min_elem + 1)
  
  y <- rep(c(0), length(x) - 2*dt)
  for (i in (1 + dt):(length(x) - dt)) {
    y[i - dt] <- func(x[i - dt], x[i], x[i + dt])
  }
  
  return (y)
}


arifmProportion <- function(x, dt=1) proportion(x, dt, function(prev, current, nex) log((prev + nex) / (2*current)))

geomProportion <- function(x, dt=1) proportion(x, dt, function(prev, current, nex) log(prev * nex /  current^2))

garmProportion <- function(x, dt=1) proportion(x, dt, function(prev, current, nex) log(2 * prev * nex / (current * (prev + nex))))


out_of_trend <- function(x, dt=1, method="Arifm") {
  #' Извлечение тренда из данных
  #' 
  #' @param method Метод извлечения тренда. Одно из трёх значений: 'Arifm' (по умолчанию), 'Geom' или 'Garm'
  stopifnot(method == "Arifm" || method == "Geom" || method == "Garm")
  
  return (switch(method,
                 "Arifm"=arifmProportion(x, dt),
                 "Geom"=geomProportion(x, dt),
                 "Garm"=garmProportion(x, dt),
                 arifmProportion(x, dt)))
}


AlterJohns_Tau <- function(y, tau) {
  #' Функция Альтера-Джонса для фиксированного аргумента tau
  stopifnot(is.vector(y) & is.numeric(y))
  n <- length(y)
  stopifnot(is.numeric(tau) & tau %% 1 == 0 & n > tau)
  
  summa <- 0
  for (i in 1:(n - tau)) {
    summa <- summa + abs(y[i + tau] - y[i])
  }
  
  return (1 / (n - tau) * summa)
}

AlterJohns <- function(y) {
  #' Функция Альтера-Джонса для ряда
  return (sapply(1:(length(y) - 1), function(i) (AlterJohns_Tau(y, i))))
}


solve <- function(A, f, u0, count, eps) {
  #' Итерационный метод решения СЛАУ
  stopifnot(is.matrix(A) & length(A[1,]) == length(A[,1]))
  stopifnot(is.vector(f) & length(f) == length(A[1,]))
  stopifnot(is.vector(u0) & length(u0) == length(f))
  stopifnot(is.integer(count) & count %% 1 == 0 & count > 0)
  stopifnot(is.numeric(eps) & eps > 0)
  
  n <- length(f)
  
  div <- max(A, f)
  A <- A / div
  f <- f / div
  
  B <- diag(1, n, n) - A
  
  U <- matrix(0, count + 1, n)
  U[1,] <- u0
  
  for (i in 2:(count + 1)) {
    U[i,] <- B %*% U[i - 1,] + f
    
    if (max(abs(U[i,]  - U[i - 1,])) < eps) {
      return (U[i,])
    }
  }
  
  return (U[count + 1,])
}
