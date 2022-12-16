#### Set-Up ####
sensor_info <- readLines("input.txt")
sensors <- regmatches(sensor_info, gregexpr("\\d+", sensor_info)) |>
  do.call(what = rbind) |>
  apply(2, as.numeric) |>
  as.data.frame() |>
  setNames(c("sx", "sy", "bx", "by"))
sensors$distance <- with(sensors, abs(bx - sx) + abs(by - sy))

#### Part 1 ####
find_no_beacons <- function(x, y = 10) {
  if (abs(y - x$sy) > x$distance) return(numeric(0))

  spread <- x$distance - abs(y - x$sy)
  no_beacons <- x$sx + do.call(seq, as.list(spread * c(-1, 1)))
  if (x$by == y) setdiff(no_beacons, x$bx) else no_beacons
}

lapply(split(sensors, seq(nrow(sensors))), find_no_beacons, y = 2000000) |>
  Reduce(f = union) |>
  sort() |>
  length()

#### Part 2 ####
beacon <- NULL
for (sensor in split(sensors, seq(nrow(sensors)))) {
  print(sensor)
  dat <- with(
    sensor,
    data.frame(
      bx = sx + c(seq(-(distance + 1), distance + 1), seq(distance, -distance)),
      by = sy + c(seq(0, distance + 1), seq(distance, -(distance + 1)), seq(-distance, -1))
    )
  )
  dat <- dat[with(dat, bx >= 0 & by >= 0 & by <= 4000000 & bx <= 4000000), ]

  for (i in seq(nrow(dat))) {
    if (!any(abs(dat[i, "bx"] - sensors$sx) + abs(dat[i, "by"] - sensors$sy) < sensors$distance)) {
      beacon <- dat[i, ]
      break
    }
  }

  if (!is.null(beacon)) break
}

options(scipen = 20)
beacon$bx * 4000000 + beacon$by
