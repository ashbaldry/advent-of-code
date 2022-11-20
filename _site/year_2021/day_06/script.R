fish <- as.integer(strsplit(readLines("2021/day_06/input.txt"), ",")[[1]])

# Part 1
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

findNFish(fish)

# Part 2
findNewFish <- function(fish_table, i = 1, n = 80) {
  fish_table$days <- fish_table$days - 1
  if (-1 %in% fish_table$days) {
    fish_table$freq[fish_table$days == 6] <- sum(
      subset(fish_table, days %in% c(6, -1))$freq
    )
    fish_table$days[fish_table$days == -1] <- 8
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

# Part 2 Attempt 2
periods <- 0:8
fish <- factor(scan("2021/day_06/input.txt", what = numeric(), sep = ",", quiet = TRUE), periods)
fish_cnt <- unclass(table(fish))

n <- 256
for (i in seq(0, n %/% 7)) {
  days <- min(7, n - i * 7)
  tmp_fish_cnt <- setNames(fish_cnt, (periods - days) %% 9)
  fish_cnt[periods >= days] <- 0
  fish_cnt <- fish_cnt + tmp_fish_cnt[as.character(periods)]
}
sum(fish_cnt)
