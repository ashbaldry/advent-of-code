library(data.table)
cards <- read.table("input.txt", col.names = c("card", "bid"))

#### Part 1 ####
card_order <- c("A", "K", "Q", "J", "T", 9:1)

get_card_score <- function(x, wild_jacks = FALSE, card_order = card_order) {
  x <- factor(x, card_order)
  tbl <- sort(table(x), decreasing = TRUE)
  if (wild_jacks) {
    tbl[[1 + (names(tbl)[1] == "J")]] <- tbl[[1 + (names(tbl)[1] == "J")]] + tbl[["J"]]
    tbl[["J"]] <- 0
    tbl <- sort(tbl, decreasing = TRUE)
  }

  index <- 7
  if (tbl[1] == 5) index <- 1
  if (tbl[1] == 4) index <- 2
  if (tbl[1] == 3) index <- if (tbl[2] == 2) 3 else 4
  if (tbl[1] == 2) index <- if (tbl[2] == 2) 5 else 6
  data.frame(index = index, cd1 = x[1], cd2 = x[2], cd3 = x[3], cd4 = x[4], cd5 = x[5])
}

card_result <- data.table::rbindlist(lapply(strsplit(cards$card, ""), get_card_score), use.names = TRUE, fill = TRUE)
card_result[, (names(card_result)[-1]) := lapply(.SD, factor, levels = card_order), .SDcols = names(card_result)[-1]]
card_result <- data.table::data.table(cbind(cards, card_result))
data.table::setorderv(card_result, c("index", paste0("cd", 1:5)), order = rep(-1, 6))
card_result[, rank := .I]
card_result[, sum(bid * rank)]

#### Part 2 ####
card_order <- c("A", "K", "Q", "T", 9:1, "J")
card_result <- data.table::rbindlist(lapply(strsplit(cards$card, ""), get_card_score, wild_jacks = TRUE, card_order = card_order), use.names = TRUE, fill = TRUE)
card_result[, (names(card_result)[-1]) := lapply(.SD, factor, levels = card_order), .SDcols = names(card_result)[-1]]
card_result <- data.table::data.table(cbind(cards, card_result))
data.table::setorderv(card_result, c("index", paste0("cd", 1:5)), order = rep(-1, 6))
card_result[, rank := .I]
card_result[, sum(bid * rank)]
