movements <- scan("2021/day_07/input.txt", what = numeric(), sep = ",", quiet = TRUE)

# Part 1
min(vapply(seq(min(movements), max(movements)), \(x) sum(abs(movements - x)), numeric(1)))

# Part 2
