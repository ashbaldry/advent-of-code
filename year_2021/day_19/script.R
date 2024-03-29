input <- readLines("2021/day_19/input.txt")

gaps <- c(0, which(input == ""))
orientations <- expand.grid(x = c(1, -1), y = c(1, -1), z = c(1, -1))
scanners <- lapply(seq_along(gaps), \(x) {
  if (x == length(gaps)) nrows <- -1 else nrows <- gaps[x + 1] - gaps[x] - 2
  beacons <- read.csv(
    "2021/day_19/input.txt", header = FALSE, skip = gaps[x] + 1, nrows = nrows,
    col.names = c("x", "y", "z")
  )
  beacons <- as.matrix(beacons)

  # Everything said 24 directions, have mirrors here (so does make solution slower)
  all_locations <- c(
    apply(orientations, 1, \(y) t(y * t(beacons)), simplify = FALSE),
    apply(orientations, 1, \(y) t(y * t(beacons[, c("x", "z", "y")])), simplify = FALSE),
    apply(orientations, 1, \(y) t(y * t(beacons[, c("y", "x", "z")])), simplify = FALSE),
    apply(orientations, 1, \(y) t(y * t(beacons[, c("y", "z", "x")])), simplify = FALSE),
    apply(orientations, 1, \(y) t(y * t(beacons[, c("z", "x", "y")])), simplify = FALSE),
    apply(orientations, 1, \(y) t(y * t(beacons[, c("z", "y", "x")])), simplify = FALSE)
  )

  # Column rename for consistency
  lapply(all_locations, \(y) {colnames(y) <- c("x", "y", "z"); y})
})

# Scanner 1 is source of truth
beacon_locations <- scanners[[1]][[1]]
to_scan <- seq(2, length(scanners))
scanner_locations <- c(0, 0, 0)

while (length(to_scan) > 0) {
  for (i in to_scan) {
    k <- length(scanners[[i]])
    # Run through all combinations to look for connecting points
    for (j in seq_along(scanners[[i]])) {
      # Gets the x,y,z distances of each beacon to the known locations. Pasting together for speed
      distances <- apply(scanners[[i]][[j]], 1, \(x) {
        m <- sweep(beacon_locations, 2, x)
        paste(m[, 1], m[, 2], m[, 3])
      })
      distances_comb <- table(as.vector(distances))
      if (any(distances_comb >= 12)) break
    }

    # If no overlap leave to be scanned again
    if (j == length(scanners[[i]]) && !any(distances_comb >= 12)) next
    to_scan <- setdiff(to_scan, i)

    # Find the locations of the scanner and new beacons
    translation <- as.numeric(strsplit(names(distances_comb[distances_comb >= 12]), " ")[[1]])
    scanner_locations <- rbind(scanner_locations, translation)
    beacon_locations2 <- rbind(beacon_locations, sweep(scanners[[i]][[j]], 2, translation, "+"))
    beacon_locations <- beacon_locations2[!duplicated(beacon_locations2), ]
  }
}

# Part 1
nrow(beacon_locations)

# part 2
max(apply(scanner_locations, 1, \(x) max(rowSums(abs(sweep(scanner_locations, 2, x))))))

