library(ggplot2)
library(dplyr)

# Define the countries to emphasize
highlight_countries <- c("Russia", "Germany", "Israel", "Turkey", 
                         "Brazil", "Mexico", "Indonesia", "South Korea")

# Create a dataset with custom hjust and vjust values for label positioning
highlighted_data <- issue_1 %>%
  filter(country %in% highlight_countries) %>%
  mutate(
    hjust_value = case_when(
      country %in% c("Russia", "Israel", "Brazil", "Indonesia") ~ -0.3,  # Move left
      country %in% c("Germany", "Turkey", "Mexico", "South Korea") ~ 1.3,  # Move right
      TRUE ~ -0.2  # Default adjustment
    ),
    vjust_value = case_when(
      country %in% c("Israel", "Brazil") ~ -0.5,  # Slightly higher
      country %in% c("Turkey", "Mexico") ~ 1.5,  # Slightly lower
      TRUE ~ 0  # Default
    )
  )

# Plot the data
ggplot(issue_1, aes(x = net_approval, y = Region, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +  # Main data points
  geom_point(data = highlighted_data, aes(x = net_approval, y = Region), 
             size = 4, shape = 1, stroke = 1, color = "black") +  # Circle only listed countries
  geom_text(data = highlighted_data, aes(label = country, hjust = hjust_value, vjust = vjust_value), 
            size = 4, color = "black") +  # Label these countries with custom position
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none"
  ) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey") +
  geom_vline(xintercept = mean(issue_1$net_approval, na.rm = TRUE), 
             linetype = "dashed", color = "grey") +
  annotate("text", x = mean(issue_1$net_approval, na.rm = TRUE), y = 4.5, 
           label = "AVG", vjust = 0, size = 3, fontface = "bold", color = "grey") +
  annotate("text", x = -100, y = 4.5, 
           label = "\u2023 MORE DISAPPROVE", hjust = 0, vjust= 20, size = 4, color = "gray") + 
  annotate("text", x = 100, y = 4.5, 
           label = "MORE APPROVE \u2023", hjust = 1, vjust= 20, size = 4, color = "gray") +
  scale_x_continuous(breaks = c(-100, 0, 100), labels = c("-100", "0", "100")) +
  labs(
    title = "Withdraw from Global Climate Change Agreements",
    x = "Net Approval",
    y = "Region"
  )
