# If openair is not installed, uncomment the following line to install it
# install.packages("openair")

library(openair)
library(dplyr)
library(lubridate)

# Step 1: Parse and extract time components
weather <- weather %>%
  mutate(
    datetime = ymd_hms(datetime),
    year = year(datetime),
    month = month(datetime)
  )

# Step 2: Filter data for target periods
weather_2024 <- filter(weather, year == 2024)
weather_2025_h1 <- filter(weather, year == 2025, month <= 6)

# Step 3: Plot wind roses
par(mfrow = c(1, 2))  # Display two plots side by side

# Wind rose for year 2024
windRose(
  mydata = weather_2024,
  ws = "windspeed",
  wd = "winddir",
  breaks = c(0, 1, 3, 5, 8, 12, 20),  # Wind speed bins (m/s)
  angle = 22.5,
  paddle = FALSE,
  key.position = "right",
  main = "Wind Rose - Year 2024"
)

# Wind rose for January–June 2025
windRose(
  mydata = weather_2025_h1,
  ws = "windspeed",
  wd = "winddir",
  breaks = c(0, 1, 3, 5, 8, 12, 20),
  angle = 22.5,
  paddle = FALSE,
  key.position = "right",
  main = "Wind Rose - Jan to Jun 2025"
)


# If openair is not installed, uncomment the following line to install it
# install.packages("openair")

library(openair)
library(dplyr)
library(lubridate)

# Step 1: Prepare date variables
weather <- weather %>%
  mutate(
    datetime = ymd_hms(datetime),
    year = year(datetime),
    month = month(datetime)
  )

# Step 2: Filter two half-year periods
weather_2024_h1 <- filter(weather, year == 2024, month <= 6)
weather_2025_h1 <- filter(weather, year == 2025, month <= 6)

# Step 3: Plot wind roses side by side
par(mfrow = c(1, 2))  # Layout: 1 row, 2 columns

# Wind Rose for Jan–Jun 2024
windRose(
  mydata = weather_2024_h1,
  ws = "windspeed",
  wd = "winddir",
  breaks = c(0, 1, 3, 5, 8, 12, 20),
  angle = 22.5,
  paddle = FALSE,
  key.position = "right",
  main = "Wind Rose - Jan to Jun 2024"
)

# Wind Rose for Jan–Jun 2025
windRose(
  mydata = weather_2025_h1,
  ws = "windspeed",
  wd = "winddir",
  breaks = c(0, 1, 3, 5, 8, 12, 20),
  angle = 22.5,
  paddle = FALSE,
  key.position = "right",
  main = "Wind Rose - Jan to Jun 2025"
)

