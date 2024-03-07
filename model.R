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

load("data/model_forecast_data.RData")

smartax <- model_forecast_data %>%
  arrange(fiscal_year) %>%
  mutate(
    estimated_population = population * 0.0023729637850509272,
    estimated_households = estimated_population/3,
    tax_per_household = tax_revenue / estimated_households,
    target_tax_per_household = target_tax_revenue / estimated_households
  )

current_tax_rate <- tail(smartax, 11)[1, ]$tax_rate

suggested_tax_rate <- mean(tail(smartax$target_tax_rate, 10))
estimated_taxable_value <- tail(smartax, 10)[1, ]$taxable_value
estimated_tax_revenue <- suggested_tax_rate*estimated_taxable_value
estimated_tax_per_person <- estimated_tax_revenue / tail(smartax, 10)[1, ]$estimated_population
taxable_value_10yr_change <- tail(smartax, 10)[1, ]$taxable_value - tail(smartax, 20)[1, ]$taxable_value

gf_net <- gf_revenue - gf_expense
gf_target_surplus <- gf_expense*gf_target_fund_bal_pct

#Budget With Fund Balance & Projects
gf_total_expense <- gf_expense + gf_projects + gf_target_surplus
gf_total_net <- gf_revenue - gf_total_expense
gf_total_tax_rate <- suggested_tax_rate - gf_total_net / estimated_taxable_value
gf_total_tax_revenue <- gf_total_tax_rate * estimated_taxable_value
gf_total_tax_per_person <- gf_total_tax_revenue / tail(smartax, 10)[1, ]$estimated_population

#Budget With Projects
gf_project_expense <- gf_expense + gf_projects
gf_project_net <- gf_revenue - gf_project_expense
gf_project_tax_rate <- suggested_tax_rate - gf_project_net / estimated_taxable_value
gf_project_tax_revenue <- gf_project_tax_rate * estimated_taxable_value
gf_project_tax_per_person <- gf_project_tax_revenue / tail(smartax, 10)[1, ]$estimated_population

#Budget With Fund Balance
gf_fund_bal_expense <- gf_expense + gf_target_fund_bal_pct
gf_fund_bal_net <- gf_revenue - gf_fund_bal_expense
gf_fund_bal_tax_rate <- suggested_tax_rate - gf_fund_bal_net / estimated_taxable_value
gf_fund_bal_tax_revenue <- gf_fund_bal_tax_rate * estimated_taxable_value
gf_fund_bal_tax_per_person <- gf_fund_bal_tax_revenue / tail(smartax, 10)[1, ]$estimated_population

invisible({
  cat("****************** RESULTS ******************\n")
  cat("Current Tax Rate.............:", sprintf("%.6f%%", current_tax_rate), "\n")
  cat("Estimated Taxable Value......: $", format(estimated_taxable_value, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Taxable Value 10-Year Change.: $", format(taxable_value_10yr_change, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("*********************************************\n")
  cat("BUDGET VARIABLES\n")
  cat("----------------\n")
  cat("Fund Balance Savings Percent.:", sprintf("%.2f%%", gf_target_fund_bal_pct*100), "\n")
  cat("Fund Balance Savings Amount..: $", format(gf_expense*gf_target_fund_bal_pct, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("General Fund Expense.........: $", format(gf_expense, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("General Fund Revenue.........: $", format(gf_revenue, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("General Fund Net.............: $", format(gf_revenue - gf_expense, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("General Fund Projects........: $", format(gf_projects, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("General Fund Net w/Projects..: $", format(gf_revenue - (gf_expense + gf_projects), big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("General Fund Total Net.......: $", format(gf_revenue - ((gf_expense*gf_target_fund_bal_pct) + (gf_expense + gf_projects)), big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("*********************************************\n")
  cat("BALANCED TAX RATE\n")
  cat("-----------------\n")
  cat("Suggested Tax Rate...........:", sprintf("%.6f%%", suggested_tax_rate), "\n")
  cat("Estimated Tax Revenue........: $", format(estimated_tax_revenue, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Estimated Tax Per Person.....: $", format(estimated_tax_per_person, big.mark = ",", scientific = FALSE, digits = 2), "\n", sep = "")
  cat("*********************************************\n")
  cat("PROJECTS & FUND BALANCE TAX RATE\n")
  cat("--------------------------------\n")
  cat("Suggested Tax Rate...........:", sprintf("%.6f%%", gf_total_tax_rate), "\n")
  cat("Estimated Tax Revenue........: $", format(gf_total_tax_revenue, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Estimated Tax Per Person.....: $", format(gf_total_tax_per_person, big.mark = ",", scientific = FALSE, digits = 2), "\n", sep = "")
  cat("*********************************************\n")
  cat("PROJECTS TAX RATE\n")
  cat("-----------------\n")
  cat("Suggested Tax Rate...........:", sprintf("%.6f%%", gf_project_tax_rate), "\n")
  cat("Estimated Tax Revenue........: $", format(gf_project_tax_revenue, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Estimated Tax Per Person.....: $", format(gf_project_tax_per_person, big.mark = ",", scientific = FALSE, digits = 2), "\n", sep = "")
  cat("*********************************************\n")
  cat("FUND BALANCE TAX RATE\n")
  cat("---------------------\n")
  cat("Suggested Tax Rate...........:", sprintf("%.6f%%", gf_fund_bal_tax_rate), "\n")
  cat("Estimated Tax Revenue........: $", format(gf_fund_bal_tax_revenue, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Estimated Tax Per Person.....: $", format(gf_fund_bal_tax_per_person, big.mark = ",", scientific = FALSE, digits = 2), "\n", sep = "")
  cat("*********************************************\n")
})

