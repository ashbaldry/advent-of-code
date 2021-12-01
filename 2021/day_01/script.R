measurements <- scan("2021/day_01/input.txt")
sum(diff(measurements) > 0)

rollSums <- function(x) {
  N <- length(x)
  ids <- outer(seq_len(N - 2), 0:2, `+`)
  apply(ids, 1, \(y) sum(x[y]))
}

sum(diff(rollSums(measurements)) > 0)
