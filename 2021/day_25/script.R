fishes <- do.call(rbind, strsplit(readLines("2021/day_25/input.txt"), ""))

right <- which(fishes == ">", arr.ind = TRUE)
down <-  which(fishes == "v", arr.ind = TRUE)
n_points <- nrow(right) + nrow(down)

n_row <- nrow(fishes)
n_col <- ncol(fishes)
i <- 0
moveable <- TRUE

while (moveable) {
  new_right <- cbind(row = right[, "row"], col = right[, "col"] + 1)
  new_right[right[, "col"] == n_col, "col"] <- 1
  r_intersections <- duplicated(rbind(right, down, new_right))[-1:-n_points]
  new_right[r_intersections, ] <- right[r_intersections, ]
  right <- new_right

  new_down <- cbind(row = down[, "row"] + 1, col = down[, "col"])
  new_down[down[, "row"] == n_row, "row"] <- 1
  d_intersections <- duplicated(rbind(right, down, new_down))[-1:-n_points]
  new_down[d_intersections, ] <- down[d_intersections, ]
  down <- new_down

  i <- i + 1
  moveable <- !all(c(r_intersections, d_intersections))
}

print(i)
