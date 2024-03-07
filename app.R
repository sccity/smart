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

#Tax Rate Variables
min_tax_rate <- 0 #No taxes
max_tax_rate <- 0.011 #US National Average
tax_rates <- c(min_tax_rate, max_tax_rate)
median_tax_rate <- median(tax_rates)

#General Fund Budget
gf_target_fund_bal_pct <- 0.30
gf_expense <- 10000000
gf_revenue <- 8400000
gf_projects <- 2500000

source("create_tax_data.R")
source("create_economic_data.R")
source("model_data.R")
source("forecast.R")
source("model.R")
source("plots.R")
