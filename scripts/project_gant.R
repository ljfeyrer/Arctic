# Load the necessary package
library(ggplot2)
library(grid) # for the arrow function

# Define the updated data frame with the project activities
activities <- data.frame(
  Activity = factor(c("Arctic Fieldwork", "Arctic Fieldwork", 
                      "(1) Population Analysis", "(2) Diet models", "(3) Risk assessment", "Community Workshops", 
                      "Papers & Abstract Submission", 
                      "Dataset & Paper Publication"),
                    levels = c("Dataset & Paper Publication", 
                               "Papers & Abstract Submission", "Community Workshops", "(3) Risk assessment","(2) Diet models", 
                               "(1) Population Analysis", "Arctic Fieldwork", "Pilot Studies")),
  
  Start = as.Date(c("2024-09-01", "2025-09-01", 
                    "2024-10-01",  "2024-12-01","2025-01-01", "2025-06-01", "2025-07-01", "2026-01-01")),
  
  End = as.Date(c("2024-11-30", "2025-11-01", "2025-11-30",
                  "2025-11-30", "2025-11-30", "2025-11-30", "2026-03-30", "2026-03-31")),
  Color = c("#98FB98","#98FB98","#89CFF0", "#89CFF0", "#4682B4",  "#6D79C8",   "#9370DB", "#FFD700", "#FFDAB9", "#FF7F50"),
  Alpha = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# Convert the Start and End dates to character
activities$Start <- as.character(activities$Start)
activities$End <- as.character(activities$End)

# Create a Gantt chart using geom_segment
gantt_plot <- ggplot(activities, aes(y = Activity, x = Start, xend = End, yend = Activity, colour = Color, alpha = Alpha)) +
  geom_segment( linewidth = 7, lineend = "round") +
  geom_segment(linewidth = 2, lineend = "square", linetype = "dotted", colour = "white", alpha = 1) +
  scale_x_discrete(name = "", labels = function(x) format(as.Date(x), "%b %Y")) +
  scale_y_discrete(expand = c(0.1, 0)) +
  labs( y = "Activity") +
  scale_color_identity() +
  scale_alpha_identity() +
  theme_minimal() +
  theme(
    # panel.background = element_rect(fill = "#FFF5E1", color = NA), # Set panel background to gray
        # plot.background = element_rect(fill = "gray", color = NA), # Set plot background to gray
    axis.text.x = element_text(size =12, angle = 45, hjust = 1,  face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold", color = "black", lineheight = .8),
        axis.title.y = element_text(size = 26, face = "bold", color = "black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(size = 14,  face = "bold"))

# Print the Gantt chart
print(gantt_plot)

#save map

gg_gant =  here::here("FIGS/gantt_plot2.png")
ggsave(gg_gant, gantt_plot, dpi = 300)



