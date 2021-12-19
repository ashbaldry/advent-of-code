input <- readLines("2021/day_19/input.txt")
input <- readLines("2021/day_19/example.txt")

gaps <- c(0, which(input == ""))
orientations <- expand.grid(x = c(1, -1), y = c(1, -1), z = c(1, -1))
scanners <- lapply(seq_along(gaps), \(x) {
  if (x == length(gaps)) nrows <- -1 else nrows <- gaps[x + 1] - gaps[x] - 2
  beacons <- read.csv(
    "2021/day_19/example.txt", header = FALSE, skip = gaps[x] + 1, nrows = nrows,
    col.names = c("x", "y", "z")
  )
  beacons <- as.matrix(beacons)

all_locations <- c(
  apply(orientations, 1, \(y) beacons * rep(y, each = nrow(beacons)), simplify = FALSE),
  apply(orientations, 1, \(y) beacons[, c("x", "z", "y")] * rep(y, each = nrow(beacons)), simplify = FALSE),
  apply(orientations, 1, \(y) beacons[, c("y", "x", "z")] * rep(y, each = nrow(beacons)), simplify = FALSE),
  apply(orientations, 1, \(y) beacons[, c("y", "z", "x")] * rep(y, each = nrow(beacons)), simplify = FALSE),
  apply(orientations, 1, \(y) beacons[, c("z", "x", "y")] * rep(y, each = nrow(beacons)), simplify = FALSE),
  apply(orientations, 1, \(y) beacons[, c("z", "y", "x")] * rep(y, each = nrow(beacons)), simplify = FALSE)
)


  all_locations <- lapply(all_locations, \(y) {colnames(y) <- c("x", "y", "z"); y})
  # all_locations[!duplicated(all_locations)]
})

# Part 1
# Scanner 1 is source of truth
scanners_orientated <- scanners[[1]][[1]]
to_scan <- seq(2, length(scanners))

while (length(to_scan) > 0) {
  for (i in to_scan) {
    k <- length(scanners[[i]])
    for (j in seq_along(scanners[[i]])) {
      distances <- apply(scanners[[i]][[j]], 1, \(x) {
        m <- sweep(scanners_orientated, 2, x)
        paste(m[, 1], m[, 2], m[, 3])
      })
      distances_comb <- table(as.vector(distances))
      if (any(distances_comb >= 12)) break
    }
    if (j == length(scanners[[i]]) && !any(distances_comb >= 12)) next
    to_scan <- setdiff(to_scan, i)

    translation <- as.numeric(strsplit(names(distances_comb[distances_comb >= 12]), " ")[[1]])
    scanners_orientated2 <- rbind(scanners_orientated, sweep(scanners[[i]][[j]], 2, translation, "+"))
    scanners_orientated <- scanners_orientated2[!duplicated(scanners_orientated2), ]
  }
}

nrow(scanners_orientated)

# part 2
max(apply(scanners_orientated, 1, \(x) max(rowSums(abs(sweep(scanners_orientated, 2, x))))))

