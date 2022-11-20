movements <- scan("2021/day_07/input.txt", what = numeric(), sep = ",", quiet = TRUE)

# Part 1
min(vapply(seq(min(movements), max(movements)), \(x) sum(abs(movements - x)), numeric(1)))

# Part 2
# Part 2
triangle_numbers <- sapply(seq(min(movements), max(movements)), \(x) sum(seq(x)))
triangle_numbers[1] <- 0
min(sapply(seq(min(movements), max(movements)), \(x) sum(triangle_numbers[abs(movements - x) + 1])))
