input <- readLines("2021/day_03/input.txt")
reports <- t(sapply(strsplit(input, ""), as.numeric))
bin2dec <- function(x) strtoi(paste(x, collapse = ""), base = 2)

# Part 1
most_common_value <- round(apply(reports, 2, mean) + .Machine$double.eps)
least_common_value <- 1 - most_common_value
bin2dec(most_common_value) * bin2dec(least_common_value)

# Part 2
oxygen_report <- co2_report <- reports
for (i in 1:12) {
  if (nrow(oxygen_report) > 1 && is.matrix(oxygen_report)) {
    oxygen_report <- oxygen_report[
      oxygen_report[, i] == round(mean(oxygen_report[, i]) + .Machine$double.eps),
    ]
  }
  if (nrow(co2_report) > 1 && is.matrix(co2_report)) {
    co2_report <- co2_report[
      co2_report[, i] == 1 - round(mean(co2_report[, i]) + .Machine$double.eps),
    ]
  }
}
bin2dec(oxygen_report) * bin2dec(co2_report)

# Part 2 Improved
findRating <- function(reports, report_type = c("oxygen", "co2"), i = 1) {
  report_type <- match.arg(report_type)

  needed_value <- round(mean(reports[, i] + .Machine$double.eps))
  if (report_type == "co2") needed_value <- 1 - needed_value
  reports <- reports[reports[, i] == needed_value, , drop = FALSE]

  if (nrow(reports) == 1) {
    bin2dec(reports)
  } else {
    findRating(reports, report_type = report_type, i = i + 1)
  }
}

findRating(reports, "oxygen") * findRating(reports, "co2")
