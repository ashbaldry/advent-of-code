report <- readLines("2021/day_03/input.txt")
reports <- Reduce(rbind, lapply(strsplit(report, ""), as.numeric))

# Part 1
most_common_value <- round(apply(reports, 2, mean))
least_common_value <- round(apply(1 - reports, 2, mean))

strtoi(paste(most_common_value, collapse = ""), base = 2) *
strtoi(paste(least_common_value, collapse = ""), base = 2)

# Part 2
match_reports <- not_match_reports <- reports
for (i in 1:12) {
  if (nrow(match_reports) > 1 && is.matrix(match_reports)) {
    match_reports <- match_reports[
      match_reports[, i] == round(mean(match_reports[, i]) + .Machine$double.eps),
    ]
  }
  if (nrow(not_match_reports) > 1 && is.matrix(not_match_reports)) {
    not_match_reports <- not_match_reports[
      not_match_reports[, i] == 1 - round(mean(not_match_reports[, i]) + .Machine$double.eps),
    ]
  }
}

strtoi(paste(match_reports, collapse = ""), base = 2) *
  strtoi(paste(not_match_reports, collapse = ""), base = 2)
