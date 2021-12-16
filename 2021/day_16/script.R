transmission <- strsplit(readLines("2021/day_16/input.txt"), "")[[1]]

transmission_bin <- strtoi(transmission, base = 16L)
class(transmission_bin) <- "binmode"
transmission_bin <- paste(as.character(transmission_bin), collapse = "")

binaryNumber <- function(number) {
  x <- as.numeric(strsplit(number, "")[[1]])
  sum(x * 2 ^ (seq(length(x) - 1, 0)))
}

findPackets <- function(transmission_bin, type_1_run = 0, type_1_max = 1) {
  version <- strtoi(substr(transmission_bin, 1, 3), 2L)
  type_id <- strtoi(substr(transmission_bin, 4, 6), 2L)

  if (type_id == 4) {
    i <- 7
    number <- substr(transmission_bin, i + 1, i + 4)
    while (substr(transmission_bin, i, i) == "1") {
      i <- i + 5
      number <- paste0(number, substr(transmission_bin, i + 1, i + 4))
    }
    i <- i + 5
    number <- binaryNumber(number)
    subpackages <- NULL
  } else {
    type_id_length <- substr(transmission_bin, 7, 7)

    if (type_id_length == "0") {
      length_subpackages <- strtoi(substr(transmission_bin, 8, 22), 2L)
      subpackages <- findPackets(substr(transmission_bin, 23, 22 + length_subpackages))
      i <- 23 + length_subpackages
    } else {
      n_subpackages <- strtoi(substr(transmission_bin, 8, 18), 2L)
      subpackages <- findPackets(substr(transmission_bin, 19, nchar(transmission_bin)), 1, n_subpackages)
      i <- nchar(transmission_bin) - subpackages[[n_subpackages]]$nchar + 1
    }
    number <- NULL
  }

  packet <- list(version = version, type = type_id, value = number, subpackages = subpackages)

  remaining_transmission <- substr(transmission_bin, i, nchar(transmission_bin))
  if (type_1_run == type_1_max) packet$nchar <- nchar(remaining_transmission)

  if (length(remaining_transmission) > 0 && grepl("1", remaining_transmission) && type_1_run != type_1_max) {
    if (type_1_run > 0) type_1_run <- type_1_run + 1
    c(list(packet), findPackets(remaining_transmission, type_1_run, type_1_max))
  } else {
    list(packet)
  }
}

# Part 1
packets <- findPackets(transmission_bin)[[1]]
packets_unlisted <- unlist(packets)
sum(packets_unlisted[grepl("version", names(packets_unlisted))])

# Part 2
findPacketSum <- function(packet) {
  if (packet$type == 0) {
    sum(sapply(packet$subpackages, findPacketSum))
  } else if (packet$type == 1) {
    prod(sapply(packet$subpackages, findPacketSum))
  } else if (packet$type == 2) {
    min(sapply(packet$subpackages, findPacketSum))
  } else if (packet$type == 3) {
    max(sapply(packet$subpackages, findPacketSum))
  } else if (packet$type == 4) {
    packet$value
  } else if (packet$type == 5) {
    as.numeric(findPacketSum(packet$subpackages[[1]]) > findPacketSum(packet$subpackages[[2]]))
  } else if (packet$type == 6) {
    as.numeric(findPacketSum(packet$subpackages[[1]]) < findPacketSum(packet$subpackages[[2]]))
  } else if (packet$type == 7) {
    as.numeric(findPacketSum(packet$subpackages[[1]]) == findPacketSum(packet$subpackages[[2]]))
  }
}
findPacketSum(packets)
