sensors <- read.table("input.txt")

#### Part 1 ####
find_next_sensor <- function(x) {
  if (all(x == 0)) return(0)
  y <- diff(x)
  z <- find_next_sensor(y)
  tail(x, 1) + z
}

sum(apply(sensors, 1, find_next_sensor))

#### Part 2 ####
sum(apply(rev(sensors), 1, find_next_sensor))
