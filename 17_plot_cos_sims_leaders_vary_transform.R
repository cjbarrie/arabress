library(ggplot2)
library(ggthemes)
library(dplyr)

# Define the list of countries
countries <- c("djazairess", "maghress", "masress", "sauress", "turess")
# Versions to process based on weights
weights <- c(1, 10, 50, 100, 500, 1000, 2000, 10000, 50000, 100000)
weights <- sapply(weights, function(x) format(x, scientific = FALSE))
versions <- paste0("1500000_weight", weights)

# Iterate through both versions and read the corresponding data
all_data <- bind_rows(lapply(versions, function(version) {
  bind_rows(lapply(countries, function(country) {
    cos_simsdf <-
      readRDS(paste0("data/output/cos_sims/", country, "/", "cos_simsdf_all", version, "30k.rds"))
    cos_simsdf %>%
      mutate(group = as.Date(group)) %>%
      arrange(group) %>%
      rename(yearwk = group,
             cos_sim = val) %>%
      mutate(country = country, 
             version = version) # Add columns for country and version
  }))
}))

country_map <- c(
  djazairess = "Algeria", 
  maghress = "Morocco", 
  masress = "Egypt", 
  sauress = "Saudi Arabia", 
  turess = "Tunisia"
)

all_data <- all_data %>%
  mutate(country_name = country_map[country])

# # Order of versions
# ordered_versions <- c("150000_weight10", "1500000_weight50", "1500000_weight100", "1500000_weight500", "1500000_weight1000", "1500000_weight2000")
# # Convert 'version' to a factor with specified order
# all_data$version <- factor(all_data$version, levels = ordered_versions)

# Define versions and their desired order
versions <- c(
  "1500000_weight1",
  "1500000_weight10",
  "1500000_weight50",
  "1500000_weight100",
  "1500000_weight500",
  "1500000_weight1000",
  "1500000_weight2000",
  "1500000_weight10000",
  "1500000_weight50000",
  "1500000_weight100000"
)

# Ensure versions in all_data are ordered as per the desired sequence
all_data$version <- factor(all_data$version, levels = versions)

# Then plot the data:
colors_for_versions <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#AD8BC9", "#FFC085")

labeller_func <- labeller(country_name = function(x) c(rep("", length(x) - 1), x))

# Plot the combined data
ggplot(all_data, aes(x = yearwk, y = cos_sim, col = version)) +
  geom_point(alpha = 0.1, size = 2) + # Increased point size and reduced transparency
  geom_smooth(method = "loess", size = 1.5, # Increased line size
              span = .5, se = F) +
  theme_tufte(base_family = "Helvetica") +
  labs(x = "Year-week", 
       y = "Cosine similarity, leaders : opposition index",
       color = "Version") + # Label for the color legend
  ylim(-.2, 0.2) +
  scale_color_manual(values = colors_for_versions) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text=element_text(size=20),
        legend.title = element_text(size = 20),
        legend.key.width = unit(3, "in"),  
        legend.direction = "horizontal",
        legend.box = "horizontal",
        strip.text.y = element_text(size = 25, face = "bold", angle = 90),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(size = 0.1, linetype = "solid"),
        panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
        strip.placement = "outside",
        strip.text = element_text(size = 25)) + 
  facet_grid(rows = vars(country_name), cols = vars(version), switch = "y")

ggsave("plots/combinedplot_all_vary_transform.png", units = "in",
       width = 40, height =20, dpi = 300)
  