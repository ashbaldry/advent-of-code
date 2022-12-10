#### Set-Up ####
signals <- read.table("input.txt", col.names = c("action", "strength"), sep = " ", fill = TRUE)
signals$strength <- ifelse(is.na(signals$strength), 0, signals$strength)

#### Part 1 ####
signals$cycle <- cumsum(ifelse(signals$action == "noop", 1, 2))
signals$total_strength <- 1 + cumsum(signals$strength)

indexes <- c(20, 60, 100, 140, 180, 220)
sapply(indexes, \(x) with(signals, x * tail(total_strength[cycle < (x + (action == "noop"))], 1))) |> sum()

#### Part 2 ####
pixels <- sapply(
  seq(240),
  \(x) with(signals, ifelse(abs(x %% 40 - tail(total_strength[cycle <= x], 1)) <= 1, "#", "."))
)
matrix(pixels, ncol = 40, byrow = TRUE) |> apply(1, paste, collapse = "")
"BACEKLHF"

pixels <- sapply(
  seq(240) - 1,
  \(x) with(signals, ifelse(abs(x %% 40 - tail(total_strength[cycle <= max(1, x)], 1)) <= 1, "#", "."))
)
matrix(pixels, ncol = 40, byrow = TRUE) |> apply(1, paste, collapse = "")
