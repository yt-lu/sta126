
# Donut plot
donut <- function (x, labels = names(x), main = NULL, radius = 0.3, col = 2:10, ...)
{par(pty = "s", oma = c(0, 0, 0, 0))
  p <- c(0, cumsum(x / sum(x))) * 2 * pi # Partitions in radians
  u <- seq(0, 2 * pi, length.out = 200)  # Angle parameter u
  R <- 1                                    # Outer radius
  r <- 1 - radius                           # Inner radius
  size <- 1.4
  plot(R * cos(u), R * sin(u), type = 'l', axes = FALSE, 
       xlab = NA, ylab = NA, xlim = c(-size, size), ylim = c(-size, size))
  lines(r * cos(u), r * sin(u))
  for (i in 1:length(x)) {
    t <- seq(p[i], p[i + 1], length.out = 200)
    polygon(c(r * cos(t), rev(R * cos(t))), c(r * sin(t), rev(R * sin(t))), col = col[i])
    text(1.1 * cos(t[100]), 1.1 * sin(t[100]), labels[i], 
         srt = min(t[101] * 180 / pi - 90, abs(270 - t[101] * 180 / pi)))
  }
}