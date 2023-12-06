races <- read.table("input.txt")

#### Part 1 ####
find_winning_races <- function(x) {
  time <- x[1]; distance <- x[2]
  times <- seq(time)
  distances <- (time - times) * times
  sum(distances > distance)
}

winning_times <- sapply(races[, -1], find_winning_races)
prod(winning_times)

#### Part 2 ####
find_winning_races(as.numeric(apply(races[, -1], 1, paste0, collapse = "")))
