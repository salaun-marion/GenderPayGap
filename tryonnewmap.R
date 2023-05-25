#tryon to improve the map

library(dplyr)
library(tidyverse)
library(highcharter)

data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
colnames(data) <- c("region","year","GDP","Urban population","Industry","Business","Mining", 
                    "Manufacturing","Electricity supply","Water supply","Construction",
                    "Retail","Transportation","Accommodation","Information",
                    "Financial","Real estate","Science",
                    "Administrative","Public administration","Education",
                    "Health","Arts","Other")

# worldmap <- map_data("world")
#glimpse(data)
# mapdata <- left_join(data, worldmap, by="region", relationship = "many-to-many")

mapdata <- get_data_from_map(download_map_data("custom/europe"))
mapGPG <- left_join(data, mapdata, by="region", relationship = "many-to-many")

# glimpse(mapdata)

data_fake <- mapGPG |>
  select(code = `iso-a2`) |>
  mutate(value = 1e5 * abs(rt(nrow(mapdata), df = 10)))

glimpse(mapGPG)

# map itself

hcmap(
  "custom/europe",
  data = data_fake,
  value = "value",
  joinBy = c("iso-a2", "code"),
  name = "Fake data",
  dataLabels = list(enabled = TRUE, format = "{point.name}"),
  borderColor = "#FAFAFA",
  borderWidth = 0.1,
  tooltip = list(
    valueDecimals = 2,
    valuePrefix = "€",
    valueSuffix = "EUR"
  )
)