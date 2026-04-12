library(ggplot2)
library(dplyr)

# Data
df <- data.frame(
  source = c("Fed Minutes", "Fed Speeches", "SEC Speeches", "GDELT", "Congressional Hearings"),
  value  = c(1074, 1869, 4639, 15516, 16927)
)

# Order factor
df$source <- factor(df$source, levels = df$source)

# Plot
ggplot(df, aes(x = source, y = value)) +
  
  # Bars
  geom_col(fill = "#1f3b73", width = 0.65) +
  
  # Labels on top
  geom_text(aes(label = scales::comma(value)),
            vjust = -0.4,
            size = 4,
            fontface = "bold",
            color = "#1a1a1a") +
  
  # Title & labels
  labs(
    title = "Main Sentiment Data Sources",
    subtitle = "Number of Unique Observations by Source",
    x = NULL,
    y = "Number of Documents"
  ) +
  
  # Y scale formatting
  scale_y_continuous(labels = scales::comma) +
  
  # Theme (quant style)
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    
    axis.text.x = element_text(angle = 15, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    
    axis.title.y = element_text(size = 12),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray85"),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    
    plot.margin = margin(15, 15, 15, 15)
  )
