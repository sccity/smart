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
library(dplyr)
source("db.R")
db <- db_connect()

load("data/historic_tax_data.RData")
load("data/economic_data.RData")

model_data <- merge(historic_tax_data, economic_data, by = "fiscal_year", all = TRUE)
model_data <- subset(model_data, !is.na(taxable_value))
model_data <- subset(model_data, !is.na(population_growth))

model_data <- model_data %>%
  arrange(fiscal_year) %>%
  mutate(
    inflation_rate = suppressWarnings((c(NA, diff(cpi) / lag(cpi) * 100)[-n()])),
    tax_revenue_adjusted = tax_revenue / (1 + inflation_rate),
    total_gf_revenue_adjusted = total_gf_revenue / (1 + inflation_rate),
    tax_percent_of_gf_adjusted = (tax_revenue_adjusted / total_gf_revenue_adjusted),
    inf_adjusted_tax_rate = tax_rate + (tax_revenue - tax_revenue_adjusted) / taxable_value,
    target_tax_revenue = total_gf_revenue * 0.25,
    target_tax_rate = target_tax_revenue / taxable_value,
    target_tax_rate_change = ((target_tax_rate - tax_rate) / tax_rate)
  )

save(model_data, file = 'data/model_data.RData')

dbWriteTable(db, name = "model_data", value = model_data, overwrite = TRUE, append = FALSE)
dbDisconnect(db)