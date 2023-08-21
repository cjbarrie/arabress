library(ggplot2)
library(ggthemes)
library(dplyr)

# Define the list of countries
countries <- c("djazairess", "maghress", "masress", "sauress", "turess")
# Versions to process
sample_sizes <- c(1e4, 5e4, 1e5, 5e5, 1e6, 1.5e6)
formatted_sample_sizes <- sapply(sample_sizes, function(x) format(x, scientific = FALSE))
versions <- paste0(formatted_sample_sizes, "30k")

# Iterate through both versions and read the corresponding data
all_data <- bind_rows(lapply(versions, function(version) {
  bind_rows(lapply(countries, function(country) {
    cos_simsdf <-
      readRDS(paste0("data/output/cos_sims/", country, "/", "cos_simsdf_all_ip", version, ".rds"))
    cos_simsdf %>%
      mutate(group = as.Date(group)) %>%
      arrange(group) %>%
      rename(yearwk = group,
             cos_sim = val) %>%
      mutate(country = country, version = version) # Add columns for country and version
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

# Order of versions
ordered_versions <- c("1000030k", "5000030k", "10000030k", "50000030k", "100000030k", "150000030k")
# Convert 'version' to a factor with specified order
all_data$version <- factor(all_data$version, levels = ordered_versions)
# Then plot the data:
colors_for_versions <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")


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
  theme(legend.position = "right",
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(size = 0.1, linetype = "solid"),
        panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
        strip.text = element_text(size = 24)) +  # Increased facet title size
  facet_wrap(~ country_name, ncol = 5)

ggsave("plots/combinedplot_all_ip.png", units = "in",
       width = 20, height =5, dpi = 300)

colors_for_versions <- c("grey50","black")  # modify as needed

# Plot the combined data
all_data %>%
  filter(version %in% c("100000030k", "150000030k")) %>%
  ggplot(aes(x = yearwk, y = cos_sim, col = version)) +
  geom_point(alpha = .25) +
  geom_smooth(
    method = "loess",
    size = 1,
    span = .5,
    fill = "white"
  ) +
  theme_tufte(base_family = "Helvetica") +
  labs(x = "Year-week",
       y = "Cosine similarity, leaders : opposition index",
       color = "Version") + # Label for the color legend
  ylim(-.2, 0.2) +
  scale_color_manual(values = colors_for_versions) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 15),
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
  facet_wrap( ~ country_name, ncol = 5)

ggsave(
  "plots/combinedplot_top_ip.png",
  units = "in",
  width = 20,
  height = 5,
  dpi = 300
)
