#### Set-Up ####
directions <- readLines("example.txt") |>
  strsplit(" -> ") |>
  lapply(strsplit, ",") |>
  lapply(\(x) lapply(x, as.numeric)) |>
  lapply(do.call, what = rbind) |>
  lapply(`colnames<-`, c("x", "y"))

all_directions <- do.call(rbind, directions)
drawing <- matrix(".", ncol = max(all_directions[, 1]), nrow = max(all_directions[, 2]) + 1)

for (rocks in directions) {
  for (i in seq(2, nrow(rocks))) {
    drawing[cbind(seq(rocks[i - 1, 2], rocks[i, 2]), seq(rocks[i - 1, 1], rocks[i, 1]))] <- "#"
  }
}

#### Part 1 ####
find_sand <- function(sand_drawing) {
  sand <- 0
  abyss <- FALSE

  start <- matrix(c(0, 500), ncol = 2)
  up <- matrix(c(1, 0), ncol = 2)
  right <- matrix(c(1, 1), ncol = 2)
  left <- matrix(c(1, -1), ncol = 2)

  while (!abyss) {
    sand_loc <- start
    while (any(sand_drawing[rep(sand_loc, each = 3) + rbind(up, right, left)] == ".")) {
      if (sand_drawing[sand_loc + up] == ".") {
        sand_loc <- sand_loc + up
      } else if (sand_drawing[sand_loc + left] == ".") {
        sand_loc <- sand_loc + left
      } else if (sand_drawing[sand_loc + right] == ".") {
        sand_loc <- sand_loc + right
      }

      if (sand_loc[, 1] == nrow(sand_drawing)) break
    }

    if (sand_loc[, 1] == nrow(sand_drawing) || identical(sand_loc, start)) {
      sand <- sand + identical(sand_loc, start)
      abyss <- TRUE
    } else {
      sand <- sand + 1
      sand_drawing[sand_loc] <- "o"
    }
  }

  sand_drawing <<- sand_drawing
  sand
}

find_sand(drawing)

#### Part 2 ####
full_drawing <- rbind(
  cbind(
    drawing,
    matrix(".", nrow = nrow(drawing), ncol = 500)
  ),
  "#"
)

find_sand(full_drawing)
