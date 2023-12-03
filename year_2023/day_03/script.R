schema_lines <- readLines("input.txt")
schema <- do.call(rbind, strsplit(schema_lines, ""))

#### Part 1 ####
special_chars <- unique(grep("[^0-9\\.]", schema, value = TRUE))
special_locs <- schema == special_chars[1]
for (i in special_chars) special_locs <- special_locs | schema == i

dims <- dim(schema)
special_area <- which(special_locs, arr.ind = TRUE) |>
  apply(1, simplify = FALSE, \(x) {
    dat <- cbind(
      row = rep((x[1] - 1):(x[1] + 1), each = 3),
      col = rep((x[2] - 1):(x[2] + 1), times = 3)
    )
    dat[dat[, 1] >= 1 & dat[, 2] >= 1 & dat[, 1] <= dims[1] & dat[, 2] <= dims[2], ]
  }) |>
  do.call(what = rbind)

inner_nums <- numeric()
for (i in seq(dims[1])) {
  special_cols <- special_area[special_area[, 1] == i, "col"]
  number_locs <- gregexpr("[0-9]+", schema_lines[i])[[1]]
  if (number_locs[1] == -1) next
  for (j in seq_along(number_locs)) {
    start <- number_locs[j]
    area <- start + seq(attr(number_locs, "match.length")[j]) - 1
    if (any(area %in% special_cols)) {
      inner_nums <- c(inner_nums, as.numeric(substr(schema_lines[i], area, max(area))))
    }
  }
}

sum(inner_nums)

#### Part 2 ####
special_locs <- schema == "*"

special_area <- which(special_locs, arr.ind = TRUE) |>
  apply(1, \(x) {
    tl <- grepl("\\d", schema[x[1] - 1, x[2] - 1])
    tr <- grepl("\\d", schema[x[1] - 1, x[2] + 1]) && !(tl && grepl("\\d", schema[x[1] - 1, x[2]]))
    l <- grepl("\\d", schema[x[1], x[2] - 1])
    r <- grepl("\\d", schema[x[1], x[2] + 1])
    bl <- grepl("\\d", schema[x[1] + 1, x[2] - 1])
    br <- grepl("\\d", schema[x[1] + 1, x[2] + 1]) && !(bl && grepl("\\d", schema[x[1] + 1, x[2]]))

    if (sum(tl, tr, l, r, bl, br) != 2) return(0)

    nums <- c()
    if (tl) {
      nums <- c(nums, sub(".*[^0-9]", "", sub(paste0("(^.{", x[2] - 2, "}\\d+).*"), "\\1", schema_lines[x[1] - 1])))
    }
    if (tr) {
      nums <- c(nums, sub(".*[^0-9]", "", sub(paste0("(^.{", x[2], "}\\d+).*"), "\\1", schema_lines[x[1] - 1])))
    }
    if (l) nums <- c(nums, sub(".*[^0-9]", "", substr(schema_lines[x[1]], 1, x[2] - 1)))
    if (r) nums <- c(nums, sub("[^0-9].*", "", substr(schema_lines[x[1]], x[2] + 1, dims[2])))
    if (bl) {
      nums <- c(nums, sub(".*[^0-9]", "", sub(paste0("(^.{", x[2] - 2, "}\\d+).*"), "\\1", schema_lines[x[1] + 1])))
    }
    if (br) {
      nums <- c(nums, sub(".*[^0-9]", "", sub(paste0("(^.{", x[2], "}\\d+).*"), "\\1", schema_lines[x[1] + 1])))
    }

    prod(as.numeric(nums))
  })

sum(special_area)
