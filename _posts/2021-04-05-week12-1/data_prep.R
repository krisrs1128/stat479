housing <- read_csv("https://raw.githubusercontent.com/empathy87/The-Elements-of-Statistical-Learning-Python-Notebooks/master/data/California%20Housing.txt")
ix <- sample(seq_len(nrow(housing)), 1000)
y <- housing$medianHouseValue
housing <- housing %>%
  mutate(
    ave_bedrooms = totalBedrooms / households,
    ave_rooms = totalRooms / households,
    ave_occup = population / households,
    ave_occup = pmin(4, ave_occup)
  ) %>%
  rename(house_age = housingMedianAge, median_income = medianIncome) %>%
  select(population, ave_bedrooms, ave_rooms, ave_occup, house_age, latitude, longitude, median_income, medianHouseValue)
write_csv(housing, "~/Desktop/housing.csv")