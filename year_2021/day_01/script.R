# Part 1
measurements <- scan("2021/day_01/input.txt")
sum(measurements[-1] > measurements[-length(measurements)])

# Part 2
rollSums <- function(x) {
  N <- length(x)
  ids <- outer(seq_len(N - 2), 0:2, `+`)
  apply(ids, 1, \(y) sum(x[y]))
}

sum(diff(rollSums(measurements)) > 0)

# Part 2 - Vectorised
rollSums2 <- function(x) {
  N <- length(x)
  x[-1:-2] + x[c(-1, -N)] + x[c(-(N - 1), -N)]
}
sum(diff(rollSums2(measurements)) > 0)

rollIncrease <- function(x) {
  N <- length(x)
  x[-1:-3] + x[c(-1, -2, -N)] + x[c(-1, -(N - 1), -N)] >
    x[-N + 0:2] + x[c(-1, -(N - 1), -N)] + x[c(-1, -2, -N)]
}

sum(rollIncrease(measurements))
