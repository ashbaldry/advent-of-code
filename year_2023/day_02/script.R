games <- read.table("input.txt", sep = ":", col.names = c("game", "results"))
games$game <- as.numeric(sub(".* ", "", games$game))
games$result_split <- strsplit(games$results, ";")

#### Part 1 ####
colours <- c("red", "blue", "green")

for (colour in colours) {
  games[[colour]] <- lapply(games$result_split, function(x) {
    matches <- grep(colour, x, value = TRUE)
    as.numeric(sub(paste0("^.* (\\d+) ", colour, ".*"), "\\1", matches))
  })
  games[[paste0(colour, "_max")]] <- sapply(games[[colour]], max)
}

sum(subset(games, red_max <= 12 & green_max <= 13 & blue_max <= 14)$game)

#### Part 2 ####
sum(games$red_max * games$blue_max * games$green_max)
