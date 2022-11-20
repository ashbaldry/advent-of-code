risk <- as.matrix(read.fwf("2021/day_15/input.txt", rep(1, 100), header = FALSE))

getRisk <- function(risk) {
  size <- nrow(risk)
  distance <- matrix(Inf, size, size)
  distance[1, 1] <- 0
  distance[-1, 1] <- cumsum(risk[-1, 1])
  distance[1, -1] <- cumsum(risk[1, -1])

  # First iteration only looks at moving right/down (what I thought was all that happened...)
  for (x in seq(2, size)) {
    for (y in seq(2, size)) {
      distance[x, y] <- risk[x, y] + min(distance[cbind(c(x - 1, x), c(y, y - 1))])
    }
  }

  # Checks if can move/left with lower risk
  reducible <- distance > risk + rbind(distance[2:size, ], Inf) |
    distance > risk + cbind(distance[, 2:size], Inf)

  while (any(reducible)) {
    for (x in seq(1, size)) {
      for (y in seq(1, size)) {
        # Checks all 4 directions for lowest risk
        points <- cbind(c(x - 1, x, x, x + 1), c(y, y - 1, y + 1, y))
        points <- points[points[, 1] >= 1 & points[, 1] <= size & points[, 2] >= 1 & points[, 2] <= size, ]
        distance[x, y] <- min(distance[x, y], risk[x, y] + min(distance[points]))
      }
    }
    reducible <- distance > risk + rbind(distance[2:size, ], Inf) |
      distance > risk + cbind(distance[, 2:size], Inf)
  }

  # Return bottom right element
  distance[size, size]
}

# Part 1
getRisk(risk)

# Part 2
risk2 <- cbind(risk, risk + 1, risk + 2, risk + 3, risk + 4)
risk2 <- rbind(risk2, risk2 + 1, risk2 + 2, risk2 + 3, risk2 + 4)
risk2[risk2 > 9] <- risk2[risk2 > 9] %% 10 + 1
getRisk(risk2)
