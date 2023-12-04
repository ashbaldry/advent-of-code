cards <- readLines("input.txt")
card_df <- data.frame(id = seq(cards))
card_df$winning <- strsplit(trimws(sub(".*: ", "", sub(" \\|.*", "", cards))), " +")
card_df$numbers <- strsplit(trimws(sub(".*\\|", "", cards)), " +")

#### Part 1 ####
card_df$inc <- apply(card_df, 1, \(x) sum(x$numbers %in% x$winning))
sum(2 ^ (card_df$inc[card_df$inc > 0] - 1))

#### Part 2 ####
card_df$win <- 1
for (i in seq(cards)) {
  inc <- card_df$inc[i]
  if (inc > 0) {
    card_df$win[i + seq(inc)] <- card_df$win[i + seq(inc)] + card_df$win[i]
  }
}

sum(card_df$win)
