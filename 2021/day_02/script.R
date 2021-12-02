directions <- read.delim(
  "2021/day_02/input.txt",
  sep = " ",
  header = FALSE,
  col.names = c("direction", "length")
)

# Part 1
sum(directions$length[directions$direction == "forward"]) *
  (sum(directions$length[directions$direction == "down"]) -
        sum(directions$length[directions$direction == "up"]))

# Part 2
findDepth <- function(directions, i = 1, n = nrow(directions), depth = 0, horizontal = 0, aim = 0) {
  if (directions$direction[i] == "forward") {
    horizontal <- horizontal + directions$length[i]
    depth <- depth + aim * directions$length[i]
  } else {
    position <- if (directions$direction[i] == "up") -1 else 1
    aim <- aim + directions$length[i] * position
  }

  if (i == n) {
    horizontal * depth
  } else {
    findDepth(directions, i = i + 1, n = n, depth = depth, horizontal = horizontal, aim = aim)
  }
}

findDepth(directions)
