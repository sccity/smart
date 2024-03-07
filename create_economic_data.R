# **********************************************************
# * CATEGORY  REVENUE & TAXATION
# * GROUP     MODELING
# * AUTHOR    LANCE HAYNIE <LHAYNIE@SCCITY.ORG>
# **********************************************************
# SMART
# Sustainable Management and Assessment for Responsible Taxation
# Copyright Santa Clara City
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.#
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
library(fredr)
library(dplyr)
source("db.R")
db <- db_connect()

fredr_set_key("ff45b73c550c4782e3f5a791c4a18e98")

cpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1990-01-01"),
  observation_end = Sys.Date()
)

cpi <- cpi %>%
  mutate(fiscal_year = lubridate::year(date)) %>%
  group_by(fiscal_year) %>%
  summarise(cpi = mean(value))

hpi <- fredr(
  series_id = "UTSTHPI",
  observation_start = as.Date("1990-01-01"),
  observation_end = Sys.Date()
)

hpi <- hpi %>%
  mutate(fiscal_year = lubridate::year(date)) %>%
  group_by(fiscal_year) %>%
  summarise(hpi = mean(value))

gdp <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1990-01-01"),
  observation_end = Sys.Date()
)

gdp <- gdp %>%
  mutate(fiscal_year = lubridate::year(date)) %>%
  group_by(fiscal_year) %>%
  summarise(gdp = mean(value))

population <- fredr(
  series_id = "UTPOP",
  observation_start = as.Date("2000-01-01"),
  observation_end = Sys.Date()
)
population$fiscal_year <- format(as.Date(population$date), "%Y")
population$population <- population$value * 1000
population <- population[, c("fiscal_year", "population")]
population$growth <- c(NA, diff(population$population))
population$population_growth <- (population$growth / lag(population$population)) * 100

unemployment <- fredr(
  series_id = "UTURN",
  observation_start = as.Date("1990-01-01"),
  observation_end = Sys.Date()
)

unemployment <- unemployment %>%
  mutate(fiscal_year = lubridate::year(date)) %>%
  group_by(fiscal_year) %>%
  summarise(unemployment = mean(value))

economic_data <- merge(cpi, hpi, by = "fiscal_year", all = TRUE)
economic_data <- merge(economic_data, gdp, by = "fiscal_year", all = TRUE)
economic_data <- merge(economic_data, population, by = "fiscal_year", all = TRUE)
economic_data <- merge(economic_data, unemployment, by = "fiscal_year", all = TRUE)

economic_data <- economic_data[order(economic_data$fiscal_year),]

save(economic_data, file = 'data/economic_data.RData')

dbWriteTable(db, name = "economic_data", value = economic_data, overwrite = TRUE, append = FALSE)
dbDisconnect(db)