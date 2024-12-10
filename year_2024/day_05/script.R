#### Set-Up ####
input <- readLines("input.txt")
empty_line <- which(input == "")

prompts <- input[seq(empty_line - 1L)]
pages <- strsplit(input[-seq(empty_line)], ",")

#### Step 1 ####
middle_pages <- as.numeric(sapply(pages, \(x) x[ceiling(length(x) / 2L)]))
valid <- sapply(pages, \(x) all(paste0(head(x, -1L), "|", tail(x, -1L)) %in% prompts))

sum(middle_pages[valid])

#### Step 2 ####
invalid_pages <- lapply(pages[!valid], \(x) {
  y <- x[1L]; x <- x[-1L]
  while (length(x)) {
    for (i in rev(seq_along(x))) {
      if (any(paste0(y, "|", x[i]) %in% prompts) && any(paste0(x[i], "|", y) %in% prompts)) {
        y <- append(y, x[i], tail(which(paste0(y, "|", x[i]) %in% prompts), 1L)); x <- x[-i]
      } else if (any(prompts == paste0(tail(y, 1L), "|", x[i]))) {
        y <- c(y, x[i]); x <- x[-i]
      } else if (any(prompts == paste0(x[i], "|", head(y, 1L)))) {
        y <- c(x[i], y); x <- x[-i]
      }
    }
  }
  y
})

sum(as.numeric(sapply(invalid_pages, \(x) x[ceiling(length(x) / 2L)])))
