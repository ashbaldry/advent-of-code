fish <- as.integer(strsplit(readLines("2021/day_06/input.txt"), ",")[[1]])

findNFish <- function(fish, i = 1, n = 80) {
  new_fish <- sum(fish == 0)
  fish <- fish - 1
  fish[fish == -1] <- 6
  fish <- append(fish, rep(8, new_fish))
  if (i < n) {
    findNFish(fish, i + 1, n)
  } else {
    length(fish)
  }
}

# Part 1
findNFish(fish)
# Part 2
findNewFish <- function(fish_table, i = 1, n = 80) {
  fish_table$days <- fish_table$days - 1
  if (-1 %in% fish_table$days) {
    fish_table <- rbind(
      fish_table[!fish_table$days %in% c(-1, 6), ],
      data.frame(
        days = 6,
        freq = sum(subset(fish_table, days %in% c(6, -1))$freq)
      ),
      data.frame(
        days = 8,
        freq = fish_table$freq[fish_table$days == -1]
      )
    )
  }

  if (i < n) {
    findNewFish(fish_table, i + 1, n)
  } else {
    fish_table
  }
}

findNFish <- function(fish, n = 80) {
  fish_table <- table(fish)
  fish_df <- data.frame(
    days = as.integer(names(fish_table)),
    freq = as.numeric(fish_table)
  )
  fish_df <- findNewFish(fish_df, n = n)
  sum(fish_df$freq)
}

findNFish(fish, n = 256)
