
library("abind")
library("purrr")
f <- c(
  list.files("~/Desktop/teaching/stat479/data/train/dogs/", recursive = TRUE, full.names = TRUE) %>% sample(500),
  list.files("~/Desktop/teaching/stat479/data/train/cats/", recursive = TRUE, full.names = TRUE) %>% sample(500)
)

images <- map(f, image_load, target_size = c(150, 150)) %>%
  map(~ image_to_array(.)) %>%
  abind(along = 0)

save(images, file = "images2.rda")