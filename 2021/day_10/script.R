commands <- readLines("2021/day_10/input.txt")

# Part 1
error_sizes <- c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137)
matching_regex <- "\\(\\)|\\[\\]|\\{\\}|<>"

sub_commands <- commands
while (!identical(gsub(matching_regex, "", sub_commands), sub_commands)) {
  sub_commands <- gsub(matching_regex, "", sub_commands)
}

invalid_commands <- substr(gsub("\\(|\\[|\\{|<", "", sub_commands), 1, 1)
invalid_commands <- invalid_commands[invalid_commands != ""]
sum(table(invalid_commands) * error_sizes)

# Part 2
incomplete_values <- c("(", "[", "{", "<")
incomplete_commands <- grep("\\)|\\}|\\]|>", sub_commands, value = TRUE, invert = TRUE)

incomplete_scores <- sapply(strsplit(incomplete_commands, ""), \(x) {
  scores <- rev(match(x, incomplete_values))
  y <- 0
  for (i in seq_along(scores)) y <- 5 * y + scores[i]
  y
})
median(incomplete_scores)
