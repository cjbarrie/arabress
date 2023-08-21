library(dplyr)
library(tidyr)
library(tidylog)
library(ggplot2)
library(ggthemes)
library(readr)
library(scales)
library(cowplot)

vdem_data <- read_csv("data/output/vdem_all.csv") %>%
  filter(year < 2020) %>%
  rename(value = "score",
         cilow = "score_low", 
         cihigh = "score_high") %>%
  mutate(type = "V-Dem")

cos_sims_data <- read_csv("data/output/bootstrap_results.csv") %>%
  mutate(value = (ll + ul)/2,
         type = "Text") %>%
  rename(cilow = "ll",
        cihigh = "ul") %>%
  select(year, value, cilow, cihigh, country, type)

df_all <- rbind(vdem_data, cos_sims_data)

countries <- unique(df_all$country)

for (countryname in countries) {
  
  countrydf <- df_all %>%
    filter(country == countryname)
  
  countrydf_wide <- countrydf %>%
    select(year, value, type) %>%
    spread(type, value)
  
  print(round(cor(countrydf_wide$Text, countrydf_wide$`V-Dem`), digits = 3))
}

df_all %>%
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(color = type)) +
  geom_ribbon(aes(ymin = cilow, ymax = cihigh, color = type), alpha = .2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  coord_cartesian(ylim=c(0.0, 3.0), xlim = c(2008, 2020)) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1)) +
  theme_base(base_family = "Helvetica") +
  labs(x = "Year", 
       y = "VDEM score",title = "") +
  scale_color_grey(labels = c("Text", "V-Dem")) +
  # annotate("text", x = 2020, y = 1.7, 
  #          parse = T, label = as.character(anncoregy),
  #          size = 10)  +
  theme(
    legend.position = c(0.9, 0.2),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
    strip.text = element_text(size = 20)
  ) +
  facet_wrap(~country, ncol = 3)
