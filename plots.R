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
library(RColorBrewer)
library(dplyr)
library(scales)
library(viridis)

standardize <- function(x) {
  (x - mean(x)) / sd(x)
}

color_palette <- brewer.pal(n = 9, name = "Spectral")
#color_palette <- viridis_pal()(8)
x_range <- seq(min_tax_rate, max_tax_rate, length.out = 100)

laffer_curve <- function(x, median_tax_rate, max_tax_rate) {
  return(-(x - median_tax_rate)^2 + max_tax_rate)
}

points_data <- data.frame(
  rates = c(min_tax_rate, max_tax_rate, median_tax_rate, suggested_tax_rate, gf_total_tax_rate, gf_project_tax_rate, gf_fund_bal_tax_rate, current_tax_rate),
  revenue = c(laffer_curve(min_tax_rate, median_tax_rate, max_tax_rate),
              laffer_curve(max_tax_rate, median_tax_rate, max_tax_rate),
              laffer_curve(median_tax_rate, median_tax_rate, max_tax_rate),
              laffer_curve(suggested_tax_rate, median_tax_rate, max_tax_rate),
              laffer_curve(gf_total_tax_rate, median_tax_rate, max_tax_rate),
              laffer_curve(gf_project_tax_rate, median_tax_rate, max_tax_rate),
              laffer_curve(gf_fund_bal_tax_rate, median_tax_rate, max_tax_rate),
              laffer_curve(current_tax_rate, median_tax_rate, max_tax_rate)),
  color = c("No Taxes", "US Average Tax Rate", "Median Tax Rate", "Balanced Tax Rate",
            "Project & Fund Balance Tax Rate", "Projects Tax Rate", "Fund Balance Tax Rate", "Current Tax Rate")
)

legend_order <- c("No Taxes", "Current Tax Rate", "US Average Tax Rate", "Balanced Tax Rate", "Median Tax Rate", "Fund Balance Tax Rate","Projects Tax Rate", "Project & Fund Balance Tax Rate")

ggplot() +
  geom_line(aes(x = x_range, y = laffer_curve(x_range, median_tax_rate, max_tax_rate)), 
            col = "blue", linetype = "solid", size = 0.75) +
  geom_point(data = points_data, aes(x = rates, y = revenue, color = color), size = 5, shape = "diamond") +
  labs(title = "Tax Rate Comparison",
       subtitle = "Santa Clara City",
       caption = "SMARTAX: Sustainable Management and Assessment for Responsible Taxation",
       x = "Tax Rate",
       y = "Revenue") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 0, size = 10, color = "gray"),
        panel.grid.major = element_line(linewidth = .5, linetype = "dashed"),
        panel.grid.minor = element_line(linewidth = .25, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "red1"),
        panel.grid.major.y = element_line(color = "blue1"),
        panel.grid.minor.x = element_line(color = "red4"),
        panel.grid.minor.y = element_line(color = "blue4"),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = 0),
        plot.subtitle = element_text(size = 14, hjust = 0),
        legend.position = "bottom") +
  scale_color_manual(values = c(color_palette[8], color_palette[7], color_palette[6], color_palette[5], color_palette[4], color_palette[3], color_palette[2], color_palette[1]),
                     breaks = legend_order) +
  labs(color = "Tax Rates")

