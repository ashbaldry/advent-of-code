algorithm <- scan("2021/day_20/input.txt", n = 1, sep = "", what = character())
algorithm <- ifelse(strsplit(algorithm, "")[[1]] == "#", 1, 0)

image <- readLines("2021/day_20/input.txt")[-1:-2]
image <- do.call(rbind, lapply(strsplit(image, ""), \(x) ifelse(x == "#", 1, 0)))

enhanceImage <- function(image, void = 0) {
  # Padding
  image_pad <- cbind(void, void, rbind(void, void, image, void, void), void, void)
  new_image <- matrix(0, nrow(image_pad), ncol(image_pad))

  inner_box <- 2:(ncol(image_pad) - 1)

  for (i in inner_box) {
    for (j in inner_box) {
      bin_number <- paste0(t(image_pad[(i - 1):(i + 1), (j - 1):(j + 1)]), collapse = "")
      new_image[i, j] <- algorithm[strtoi(bin_number, 2L) + 1]
    }
  }
  new_image[inner_box, inner_box]
}

# Part 1
image1 <- enhanceImage(image, 0)
image2 <- enhanceImage(image1, image1[1, 1])
sum(image2)

for (i in seq(nrow(image2))) cat(ifelse(image2[i, ], "#", "."), "\n", sep = "")

# Part 2
imagex <- image
for (i in 1:50) imagex <- enhanceImage(imagex, imagex[1, 1])
sum(imagex)
