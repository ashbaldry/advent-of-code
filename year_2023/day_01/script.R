#### Part 1 ####
txt <- readLines("input.txt")

gsub("[^0-9]", "", txt) |>
  strsplit("") |>
  sapply(\(x) as.numeric(paste0(head(x, 1L), tail(x, 1L)))) |>
  sum()

#### Part 2 ####
y <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
matches <- regexpr(paste(y, collapse = "|"), txt)
txt2 <- sapply(txt, USE.NAMES = FALSE, \(x) {
  p <- regexpr(paste(y, collapse = "|"), x)
  while (p > 0) {
    number <- substr(x, p, p + attr(p, "match.length") - 1)
    x <- sub(paste0("(?<=.{", p-1, "})", substr(number, 1, 1)), match(number, y), x, perl = T)
    p <- regexpr(paste(y, collapse = "|"), x)
  }
  x
})

gsub("[^0-9]", "", txt2) |>
  strsplit("") |>
  sapply(\(x) as.numeric(paste0(head(x, 1L), tail(x, 1L)))) |>
  sum()
