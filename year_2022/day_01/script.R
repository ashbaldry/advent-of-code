# Set-Up
calories <- as.integer(readLines("input.txt"))

# Part 1
splits <- cumsum(is.na(calories))
elf_calories <- split(calories, splits) |> vapply(sum, integer(1L), na.rm = TRUE, USE.NAMES = FALSE)
max(elf_calories)

# Part 2
sum(sort(elf_calories, decreasing = TRUE)[1:3])
