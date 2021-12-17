input <- readLines("2021/day_17/input.txt")
edges <- setNames(
  as.numeric(regmatches(input, gregexpr("(-|)\\d+", input))[[1]]),
  c("x1", "x2", "y1", "y2")
)

# Part 1
sum(seq(abs(edges["y1"]) - 1))

# Part 2
triangle_numbers <- cumsum(seq(20))
min_x <- min(which(triangle_numbers >= edges["x1"] & triangle_numbers <= edges["x2"]))
max_x <- unname(edges["x2"])
min_y <- unname(edges["y1"])
max_y <- unname(abs(edges["y1"]) - 1)

potential_veloctiy <- cbind(
  x = rep(min_x:max_x, each = max_y - min_y + 1),
  y = rep(min_y:max_y, times = max_x - min_x + 1)
)

apply(potential_veloctiy[1:3, ], 1, \(velocity) {
  x_pos <- cumsum(pmax(velocity["x"] - 0:280, 0))
  y_pos <- cumsum(velocity["y"] - 0:280)
  any(x_pos >= edges["x1"] & x_pos <= edges["x2"] & y_pos >= edges["y1"] & y_pos <= edges["y2"])
})
