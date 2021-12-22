instructions <- readLines("2021/day_22/input.txt")
inst <- as.data.frame(do.call(rbind, regmatches(instructions, gregexpr("on|off|(-|)\\d+", instructions))))
names(inst) <- c("switch", "x1", "x2", "y1", "y2", "z1", "z2")
inst[, -1] <- sapply(inst[, -1], as.numeric)
inst[, -1] <- cbind(
  x1 = pmin(inst$x1, inst$x2), x2 = pmax(inst$x1, inst$x2),
  y1 = pmin(inst$y1, inst$y2), y2 = pmax(inst$y1, inst$y2),
  z1 = pmin(inst$z1, inst$z2), z2 = pmax(inst$z1, inst$z2)
)
# Part 1
mini_inst <- inst
mini_inst[, -1] <- mini_inst[, -1] + 51
mini_board <- which(rowMeans(mini_inst[, -1] >= 1 & mini_inst[, -1] <= 101) == 1)
switches <- array(0, c(101, 101, 101))

for (i in mini_board) {
  value <- as.numeric(mini_inst$switch[i] == "on")
  switches[
    seq(mini_inst$x1[i], mini_inst$x2[i]),
    seq(mini_inst$y1[i], mini_inst$y2[i]),
    seq(mini_inst$z1[i], mini_inst$z2[i])
  ] <- value
}
sum(switches)

# Part 2
instructions <- readLines("2021/day_22/input.txt")
inst <- as.data.frame(do.call(rbind, regmatches(instructions, gregexpr("on|off|(-|)\\d+", instructions))))
names(inst) <- c("switch", "x1", "x2", "y1", "y2", "z1", "z2")
inst[, -1] <- sapply(inst[, -1], as.numeric)
inst[, -1] <- cbind(
  x1 = pmin(inst$x1, inst$x2), x2 = pmax(inst$x1, inst$x2),
  y1 = pmin(inst$y1, inst$y2), y2 = pmax(inst$y1, inst$y2),
  z1 = pmin(inst$z1, inst$z2), z2 = pmax(inst$z1, inst$z2)
)

x <- sort(unique(c(inst$x1, inst$x2 + 1)))
y <- sort(unique(c(inst$y1, inst$y2 + 1)))
z <- sort(unique(c(inst$z1, inst$z2 + 1)))
locs <- array(0, lengths(list(x, y, z)))

for (i in seq(nrow(inst))) {
  x_range <- seq(which(inst$x1[i] == x), which(inst$x2[i] + 1 == x) - 1)
  y_range <- seq(which(inst$y1[i] == y), which(inst$y2[i] + 1 == y) - 1)
  z_range <- seq(which(inst$z1[i] == z), which(inst$z2[i] + 1 == z) - 1)
  locs[as.matrix(expand.grid(x_range, y_range, z_range))] <- as.numeric(inst$switch[i] == "on")
}

on_points <- which(locs == 1, arr.ind = TRUE)
on_points2 <- on_points + 1

sum(
  (x[on_points2[, 1]] - x[on_points[, 1]]) *
    (y[on_points2[, 2]] - y[on_points[, 2]]) *
    (z[on_points2[, 3]] - z[on_points[, 3]])
)


# Part 2
findNCubes <- function(x) sum(apply(abs(x[, c("x2", "y2", "z2")] - x[, c("x1", "y1", "z1")]) + 1, 1, prod))
# c1 Current cube, c2 previous cube
excludeOverlap <- function(c1, c2) {
  # No Overlap
  if (any(c1[, c("x1", "y1", "z1")] > c2[, c("x2", "y2", "z2")]) |
      any(c1[, c("x2", "y2", "z2")] < c2[, c("x1", "y1", "z1")])) {
    c1
  # Full Outer Overlap
  } else if (all(c1[, c("x1", "y1", "z1")] >= c2[, c("x1", "y1", "z1")] &
                 c1[, c("x2", "y2", "z2")] <= c2[, c("x2", "y2", "z2")])) {
    NULL
    # Partial Outer Overlap
  } else {
    x_extra <- y_extra <- z_extra <- NULL
    x1 <- c1$x1
    x2 <- c1$x2
    y1 <- c1$y1
    y2 <- c1$y2
    z1 <- c1$z1
    z2 <- c1$z2

    if (c1$x1 < c2$x1 && c1$x2 > c2$x2) {
      x_extra <- data.frame(x1 = c(x1, c2$x2 + 1), x2 = c(c2$x1, x2 + 1), y1 = y1, y2 = y2, z1 = z1, z2 = z2)
      x1 <- c2$x1
      x2 <- c2$x2
    }
    if (c1$y1 < c2$y1 && c1$y2 > c2$y2) {
      y_extra <- data.frame(x1 = x1, x2 = x2, y1 = c(y1, c2$y2 + 1), y2 = c(c2$y1, y2 + 1), z1 = z1, z2 = z2)
      y1 <- c2$y1
      y2 <- c2$y2
    }
    if (c1$z1 < c2$z1 && c1$z2 > c2$z2) {
      z_extra <- data.frame(x1 = x1, x2 = x2, y1 = y1, y2 = y2, z1 = c(c1$z1, c2$z2 + 1), z2 = c(c2$z1 - 1, c1$z2))
      z1 <- c2$z1
      z2 <- c2$z2
    }

    # Partial Inner Overlap
    x_min <- max(x1, c2$x1)
    x_max <- min(x2, c2$x2)
    y_min <- max(y1, c2$y1)
    y_max <- min(y2, c2$y2)

    rbind(
      if (x1 >= c2$x1 && x2 > c2$x2) data.frame(x1 = c2$x2 + 1, x2 = x2, y1 = y1, y2 = y2, z1 = z1, z2 = z2),
      if (y1 >= c2$y1 && y2 > c2$y2) data.frame(x1 = x_min, x2 = x_max, y1 = c2$y2 + 1, y2 = y2, z1 = z1, z2 = z2),
      if (z1 >= c2$z1 && z2 > c2$z2) data.frame(x1 = x_min, x2 = x_max, y1 = y_min, y2 = y_max, z1 = c2$z2 + 1, z2 = z2),
      if (x2 <= c2$x2 && x1 < c2$x1) data.frame(x1 = x1, x2 = c2$x1 - 1, y1 = y1, y2 = y2, z1 = z1, z2 = z2),
      if (y2 <= c2$y2 && y1 < c2$y1) data.frame(x1 = x_min, x2 = x_max, y1 = y1, y2 = c2$y1 - 1, z1 = z1, z2 = z2),
      if (z2 <= c2$z2 && z1 < c2$z1) data.frame(x1 = x_min, x2 = x_max, y1 = y_min, y2 = y_max, z1 = z1, z2 = c2$z1 - 1),
      x_extra,
      y_extra,
      z_extra
    )
  }
}

on_cubes <- inst[1, -1]
for (i in seq(2, nrow(inst))) {
  print(i)
  curr_cube <- inst[i, -1]

  if (inst$switch[i] == "on") {

    for (j in seq(nrow(on_cubes))) {
      new_spaces <- NULL
      for (k in seq(nrow(curr_cube))) {
        new_spaces <- rbind(new_spaces, excludeOverlap(curr_cube[k, ], on_cubes[j, ]))
      }
      curr_cube <- new_spaces
      if (is.null(curr_cube)) {
        break
      }
    }
    on_cubes <- rbind(on_cubes, curr_cube)

  } else {
    new_spaces <- NULL
    for (j in seq(nrow(on_cubes))) {
      new_spaces <- rbind(new_spaces, excludeOverlap(c1 = on_cubes[j, ], c2 = inst[i, ]))
    }
    on_cubes <- new_spaces
  }
}

  sum(switches)
findNCubes(on_cubes)
