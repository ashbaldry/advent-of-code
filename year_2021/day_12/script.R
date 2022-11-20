lines <- read.delim("2021/day_12/input.txt", sep = "-", header = FALSE, col.names = c("a", "b"))
points <- unique(unlist(lines))
valid_points <- setNames(rep(TRUE, length(points)), points)
visits <- setNames(rep(0, length(points)), points)

getNPaths <- function(lines, valid_points, visits, start = "start", finish = "end", max_visits = 1) {
  # If at the end then can finish the path
  if (start == finish) return(1)

  # Cannot revisit lowercase points more than max_visits times
  if (tolower(start) == start) {
    if (start == "start") valid_points[start] <- FALSE
    visits[start] <- visits[start] + 1
    if (visits[start] == max_visits) {
      valid_points[names(visits[visits >= 1])] <- FALSE
    } else if (any(visits == max_visits)) {
      valid_points[start] <- FALSE
    }
  }

  next_vertices <- c(lines$a[lines$b == start], lines$b[lines$a == start])
  next_vertices <- next_vertices[next_vertices %in% points[valid_points]]

  if (length(next_vertices) == 0) {
    0
  } else {
    sum(sapply(
      next_vertices, getNPaths, lines = lines, visits = visits,
      valid_points = valid_points, finish = finish, max_visits = max_visits
    ))
  }
}

# Part 1
getNPaths(lines, valid_points, visits)

# Part 2
getNPaths(lines, valid_points, visits, max_visits = 2)
