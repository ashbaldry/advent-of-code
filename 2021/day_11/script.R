octopi <- read.fwf("2021/day_11/input.txt", widths = rep(1, 10))
octopi <- as.matrix(octopi)

# Part 1
countNFlashes <- function(octopi, n = 100, i = 1, flashes = 0) {
  octopi <- octopi + 1

  if (any(octopi > 9)) {
    new_flashing_octopi <- flashing_octopi <- octopi > 9
    while (any(new_flashing_octopi)) {
      updating_octopi <- getSurroundingOctopi(which(new_flashing_octopi, arr.ind = TRUE))
      for (j in seq_along(updating_octopi)) {
        octopi[updating_octopi[[j]]] <- octopi[updating_octopi[[j]]] + 1
      }
      new_flashing_octopi <- octopi > 9 & !flashing_octopi
      flashing_octopi <- octopi > 9
    }

    flashes <- flashes + sum(octopi > 9)
    octopi[flashing_octopi] <- 0
  }

  if (i == n) {
    flashes
  } else {
    countNFlashes(octopi, n, i + 1, flashes)
  }
}

getSurroundingOctopi <- function(locations) {
  # Getting box of locations
  boxLocations <- \(x) max(1, x - 1):min(10, x + 1)
  all_locations <- apply(
    locations, 1, \(x) as.matrix(merge(boxLocations(x[1]), boxLocations(x[2]))), simplify = FALSE
  )
}

countNFlashes(octopi)

# Part 2
countAllFlashes <- function(octopi, i = 1) {
  octopi <- octopi + 1

  if (any(octopi > 9)) {
    new_flashing_octopi <- flashing_octopi <- octopi > 9
    while (any(new_flashing_octopi)) {
      updating_octopi <- getSurroundingOctopi(which(new_flashing_octopi, arr.ind = TRUE))
      for (j in seq_along(updating_octopi)) {
        octopi[updating_octopi[[j]]] <- octopi[updating_octopi[[j]]] + 1
      }
      new_flashing_octopi <- octopi > 9 & !flashing_octopi
      flashing_octopi <- octopi > 9
    }

    octopi[flashing_octopi] <- 0
  }

  if (all(flashing_octopi)) {
    i
  } else {
    countAllFlashes(octopi, i + 1)
  }
}

countAllFlashes(octopi)
