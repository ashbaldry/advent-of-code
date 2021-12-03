input <- readLines("2021/day_03/input.txt")
reports <- Reduce(rbind, lapply(strsplit(input, ""), as.numeric))

# Part 1
most_common_value <- round(apply(reports, 2, mean))
least_common_value <- round(apply(1 - reports, 2, mean))

strtoi(paste(most_common_value, collapse = ""), base = 2) *
  strtoi(paste(least_common_value, collapse = ""), base = 2)

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

strtoi(paste(oxygen_report, collapse = ""), base = 2) *
  strtoi(paste(co2_report, collapse = ""), base = 2)

# Part 2 Improved
findReport <- function(reports, report_type = c("oxygen", "co2"), i = 1) {
  report_type <- match.arg(report_type)

  needed_value <- round(mean(reports[, i] + .Machine$double.eps))
  if (report_type == "co2") needed_value <- 1 - needed_value
  reports <- reports[reports[, i] == needed_value, , drop = FALSE]

  if (nrow(reports) == 1) {
    strtoi(paste(reports, collapse = ""), base = 2)
  } else {
    findReport(reports, report_type = report_type, i = i + 1)
  }
}

oxygen_report <- findReport(reports)
co2_report <- findReport(reports, "co2")
oxygen_report * co2_report
