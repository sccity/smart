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
library(forecast)
library(sqldf)
library(ggplot2)
library(scales)

source("db.R")
db <- db_connect()

load("data/model_data.RData")

forecast_model_data <- function(forecast_variable) {
  model_data <- model_data[complete.cases(model_data$fiscal_year), ]
  model_data$fiscal_year <- as.integer(model_data$fiscal_year)
  tax_ts <- ts(model_data[[forecast_variable]], start = c(model_data$fiscal_year[1], 1), frequency = 1)
  arima_model <- auto.arima(tax_ts)
  future_years <- max(model_data$fiscal_year) + 1:10
  forecast_result <- forecast(arima_model, h = 10)
  forecast_data <- data.frame(
    fiscal_year = c(model_data$fiscal_year, future_years),
    value = c(model_data[[forecast_variable]], forecast_result$mean)
  )
  return(forecast_data)
}

run_forecast <- function(target_column) {
  dataset <- forecast_model_data(target_column)
  colnames(dataset)[colnames(dataset) == "value"] <- target_column
  return(dataset)
}

taxable_value_forecast <- run_forecast("taxable_value")
taxable_rate_forecast <- run_forecast("tax_rate")
tax_revenue_forecast <- run_forecast("tax_revenue")
total_gf_revenue_forecast <- run_forecast("total_gf_revenue")
cpi_forecast <- run_forecast("cpi")
hpi_forecast <- run_forecast("hpi")
gdp_forecast <- run_forecast("gdp")
population_forecast <- run_forecast("population")
growth_forecast <- run_forecast("growth")
population_growth_forecast <- run_forecast("population_growth")
unemployment_forecast <- run_forecast("unemployment")

model_forecast_data <- sqldf("SELECT a.fiscal_year,
                              a.taxable_value,
                              b.tax_rate,
                              c.tax_revenue,
                              d.total_gf_revenue,
                              e.cpi,
                              f.hpi,
                              g.gdp,
                              h.population,
                              i.growth,
                              j.population_growth,
                              k.unemployment
                              from taxable_value_forecast a
                              left join taxable_rate_forecast b on a.fiscal_year = b.fiscal_year
                              left join tax_revenue_forecast c on a.fiscal_year = c.fiscal_year
                              left join total_gf_revenue_forecast d on a.fiscal_year = d.fiscal_year
                              left join cpi_forecast e on a.fiscal_year = e.fiscal_year
                              left join hpi_forecast f on a.fiscal_year = f.fiscal_year
                              left join gdp_forecast g on a.fiscal_year = g.fiscal_year
                              left join population_forecast h on a.fiscal_year = h.fiscal_year
                              left join growth_forecast i on a.fiscal_year = i.fiscal_year
                              left join population_growth_forecast j on a.fiscal_year = j.fiscal_year
                              left join unemployment_forecast k on a.fiscal_year = k.fiscal_year
                             ")

model_forecast_data <- model_forecast_data %>%
  arrange(fiscal_year) %>%
  mutate(
    inflation_rate = suppressWarnings((c(NA, diff(cpi) / lag(cpi) * 100)[-n()])),
    tax_revenue_adjusted = tax_revenue / (1 + inflation_rate),
    total_gf_revenue_adjusted = total_gf_revenue / (1 + inflation_rate),
    tax_percent_of_gf_adjusted = (tax_revenue_adjusted / total_gf_revenue_adjusted),
    inf_adjusted_tax_rate = tax_rate + (tax_revenue - tax_revenue_adjusted) / taxable_value,
    target_tax_revenue = total_gf_revenue * 0.20,
    target_tax_rate = target_tax_revenue / taxable_value,
    target_tax_rate_change = ((target_tax_rate - tax_rate) / tax_rate)
  )

save(model_forecast_data, file = 'data/model_forecast_data.RData')

dbWriteTable(db, name = "model_forecast_data", value = model_forecast_data, overwrite = TRUE, append = FALSE)
dbDisconnect(db)