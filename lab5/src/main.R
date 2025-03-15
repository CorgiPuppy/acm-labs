
a <- c(-1.09, 0.44, 2.30, 0.87)
x <- c(-13.0, -7.0, 1.0, 9.2)
e <- c(0.1, 0.01, 0.001, 0.0001)

cat(sprintf("\n\t\t\tИсходные данные:
              Одномерный критерий вида: 
              \tR(x) = a[0] + a[1] * x + a[2] * sin(a[3] * x)
              Коэффициенты:\n"))
for (i in 0:(length(a))) {
  cat(sprintf("\t\t\t\ta[%d] = %.2f\n", i - 1, a[i]))
}
cat(sprintf("\t\t\tПределы локализации:\n\t\t\t\t[%.1f, %.1f]\n\n", x[1], x[4]))

R <- function(x) {
  return(a[1] + a[2] * x + a[3] * sin(a[4] * x))
}

title <- function() {
  cat(sprintf("%12s%13s%13s%13s%15s%15s%10s\n",
              "a", "b", "x1", "x2", "R(x1)", "R(x2)", "(b-a)/2"))
}

global_maximum <- c()

fibonacci_search <- function(a, b, e, times = 1) {
  fibonacci <- c(1, 1)
  while (fibonacci[length(fibonacci)] < (b - a) / e)
    fibonacci <- append(fibonacci,
                        fibonacci[length(fibonacci)]
                        + fibonacci[length(fibonacci) - 1])
  i <- length(fibonacci)

  dx <- (b - a) / fibonacci[i]

  x1 <- a + dx * fibonacci[i - 2]
  x2 <- a + dx * fibonacci[i - 1]

  if (i != 4) {
    cat(sprintf("%12.3f %12.3f %12.3f %12.3f %14.9f %14.9f %9.3f\n",
                a, b, x1, x2, R(x1), R(x2), ((b - a) / 2)))
  } else if (R(x1) > R(x2)) {
    cat(sprintf("%12.3f %12.3f %3s %.3f %s %12.3f %3s %.9f %s %14.9f %12.3f\n",
                a, b, "|", x1, "|", x2, "|", R(x1), "|", R(x2), ((b - a) / 2)))
  } else if (R(x1) < R(x2)) {
    cat(sprintf("%12.3f %12.3f %12.3f %3s %.3f %s %14.9f %3s %.9f %s %9.3f\n",
                a, b, x1, "|", x2, "|", R(x1), "|", R(x2), "|", ((b - a) / 2)))
  }

  if (R(x1) > R(x2)) {
    b <- x2
    x2 <- x1
    dx <- (b - a) / fibonacci[i]
    x1 <- a + dx * fibonacci[i - 2]
  } else if (R(x1) < R(x2)) {
    a <- x1
    x1 <- x2
    dx <- (b - a) / fibonacci[i]
    x2 <- a + dx * fibonacci[i - 1]
  } else if (R(x1) == R(x2)) {
    a <- x1
    b <- x2
    x1 <- a + dx * fibonacci[i - 2]
    x2 <- a + dx * fibonacci[i - 1]
  }

  if (i > 4) {
    return(fibonacci_search(a, b, e, times + 1))
  } else {
    return(times)
  }
}

cat(sprintf("\nЛокальный максимум в пределе локализации [%.1f, %.1f]\n",
            x[1], x[2]))
title()
fibonacci_search(x[1], x[2], e[3])
cat(sprintf("\nЛокальный максимум в пределе локализации [%.1f, %.1f]\n",
            x[2], x[3]))
title()
fibonacci_search(x[2], x[3], e[3])
cat(sprintf("\nЛокальный максимум в пределе локализации [%.1f, %.1f]\n",
            x[3], x[4]))
title()
fibonacci_search(x[3], x[4], e[3])

counter <- c()
for (epsilon in e) {
  cat(sprintf(
              "\nГлобальный максимум в пределе локализации [%.1f, %.1f]
              с точностью %.4f\n",
              x[1], x[4], epsilon))
  title()
  counter <- append(counter, fibonacci_search(x[1], x[4], epsilon))
}

func <- function(x) log(x)
log_e <- lapply(e, func)

png(file = "assets/Graph.png")
plot(log_e, counter, type = "l",
     main = substitute(paste(italic("График зависимости количества итераций от заданной точности"))),
     xlab = substitute(paste(bold("Log(e)"))),
     ylab = substitute(paste(bold("Количество шагов"))))

axis_x <- seq(-13.0, 9.2, 0.1)
png(file = "assets/Plot.png")
plot(axis_x, R(axis_x), type = "l",
     main = substitute(paste(italic("График оптимизируемой функции"))),
     xlab = substitute(paste(bold("x"))),
     ylab = substitute(paste(bold("R(x)"))))
