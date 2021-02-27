# 1) шоб документацию посмотреть
?stopifnot


# 2) Объявление переменных (всё вводится с клавиатуры)
n <- as.integer(readline(prompt="N = "))
stopifnot(n > 0)
A <- edit(matrix(0, n, n), title="Матрица коэфициентов")
f <- edit(rep(c(0), n), title="Вектор решений")
count <- as.integer(readline(prompt="Количество итераций: "))
eps <- as.numeric(readline(prompt="eps = "))
u0 <- rnorm(n)


norm_vec <- function(x) sqrt(sum(x^2))


# 2) функция для итерационного метода
solve <- function(A, f, u0, count, eps) {
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


# 3) вывод ответа
print(solve(A, f, u0, count, eps))


# 4) Чтобы метод простых итераций не сходился (собственные значения A все по модулю больше 1)
# https://rep.bntu.by/bitstream/handle/data/29083/Neobhodimye_i_dostatochnye_usloviya_skhodimosti_iteracionnogo_processa_dlya_sistem_linejnyh_algebraicheskih_uravnenij.pdf?sequence=1&isAllowed=y
# https://portal.tpu.ru/SHARED/b/BGA/bio/bachelors/Tab/Numerica_meth_eq_solution.pdf
n <- 3
A <- rbind(c(1, 2, 3), c(4, 1, 6), c(7, 8, 1))
f <- c(1, 1, 1)
count <- 100
eps <- 0.00001
u0 <- rnorm(n)

print(solve(A, f, u0, count, eps))
