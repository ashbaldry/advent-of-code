#### Set-Up ####
instructions <- read.table("input.txt", col.names = c("dir", "n"), sep = " ")

#### Part 1 ####
head_loc <- c(0, 0)
tail_loc <- c(0, 0)
tail_visits <- matrix(c(0, 0), nrow = 1)

for (inst in split(instructions, seq(nrow(instructions)))) {
  head_move <- switch(inst$dir, L = c(-1, 0), R = c(1, 0), U = c(0, 1), D = c(0, -1))

  for (move in seq(inst$n)) {
    head_loc <- head_loc + head_move
    if (max(abs(head_loc - tail_loc)) <= 1) {
      tail_visits <- rbind(tail_visits, tail_loc)
      next
    } else if (all(sort(abs(head_loc - tail_loc)) == c(0, 2))) {
      tail_change <- which(abs(head_loc - tail_loc) == 2)
      tail_loc[tail_change] <- (tail_loc + ifelse(head_loc > tail_loc, 1, -1))[tail_change]
    } else {
      tail_loc <- tail_loc + ifelse(head_loc > tail_loc, 1, -1)
    }
    tail_visits <- rbind(tail_visits, tail_loc)
  }
}

sum(!duplicated(tail_visits))

#### Part 2 ####
head_loc <- c(0, 0)
tail_locs <- lapply(seq(9), \(x) c(0, 0))
tail9_visits <- matrix(c(0, 0), nrow = 1)

for (inst in split(instructions, seq(nrow(instructions)))) {
  head_move <- switch(inst$dir, L = c(-1, 0), R = c(1, 0), U = c(0, 1), D = c(0, -1))

  for (move in seq(inst$n)) {
    head_loc <- head_loc + head_move
    if (max(abs(head_loc - tail_locs[[1]])) <= 1) {
      NULL
    } else if (all(sort(abs(head_loc - tail_locs[[1]])) == c(0, 2))) {
      tail_change <- which(abs(head_loc - tail_locs[[1]]) == 2)
      tail_locs[[1]][tail_change] <- (tail_locs[[1]] + ifelse(head_loc > tail_locs[[1]], 1, -1))[tail_change]
    } else {
      tail_locs[[1]] <- tail_locs[[1]] + ifelse(head_loc > tail_locs[[1]], 1, -1)
    }

    for (i in 2:9) {
      if (max(abs(tail_locs[[i - 1]] - tail_locs[[i]])) <= 1) {
        NULL
      } else if (all(sort(abs(tail_locs[[i - 1]] - tail_locs[[i]])) == c(0, 2))) {
        tail_change <- which(abs(tail_locs[[i - 1]] - tail_locs[[i]]) == 2)
        tail_locs[[i]][tail_change] <- (tail_locs[[i]] + ifelse(tail_locs[[i - 1]] > tail_locs[[i]], 1, -1))[tail_change]
      } else {
        tail_locs[[i]] <- tail_locs[[i]] + ifelse(tail_locs[[i - 1]] > tail_locs[[i]], 1, -1)
      }

      if (i == 9) {
        tail9_visits <- rbind(tail9_visits, tail_locs[[i]])
      }
    }
  }
}

sum(!duplicated(tail9_visits))
