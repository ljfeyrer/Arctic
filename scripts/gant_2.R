# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)  # For custom annotations
library(ggtext)  # For advanced text formatting

# Define the tasks data
tasks <- tribble(
  ~Objective, ~Task, ~HQP, ~Year1_Q1, ~Year1_Q2, ~Year1_Q3, ~Year1_Q4, ~Year2_Q1, ~Year2_Q2, ~Year2_Q3, ~Year2_Q4,
  "1. Determine whale distribution & population size", "1.1 Photo-identification analyses", "URA1, PD1", 1, 1, 1, 0, 0, 0, 0, 0,
  "1. Determine whale distribution & population size", "1.2 Statistical mark-recapture models", "PD1", 1, 1, 1, 0, 0, 0, 0, 0,
  "1. Determine whale distribution & population size", "1.3 Model trends with fishing activity", "PD1", 0, 1, 1, 1, 0, 0, 0, 0,
  "1. Determine whale distribution & population size", "1.4 Integrate PAM and telemetry data", "PD1", 0, 0, 1, 1, 0, 0, 0, 0,
  "2. Assess health of whales", "2.1 Dietary biomarker analysis", "PD1, URA1", 1, 1, 1, 0, 0, 0, 0, 0,
  "2. Assess health of whales", "2.2 Measure body condition from UAVs", "MSc1, PD1", 0, 1, 1, 1, 0, 0, 0, 0,
  "3. Evaluate risks of whale-fisheries interactions", "3.1 Assess risk tolerance from UAV footage", "MSc1, PD1", 0, 0, 1, 1, 1, 0, 0, 0,
  "3. Evaluate risks of whale-fisheries interactions", "3.2 Analyze anthropogenic scaring", "MSc1,URA1", 0, 0, 0, 1, 1, 0, 0, 0,
  "3. Evaluate risks of whale-fisheries interactions", "3.3 Assess trends in bycatch incidents", "PD1, URA2", 0, 0, 0, 1, 1, 1, 0, 0,
  "3. Evaluate risks of whale-fisheries interactions", "3.4 Risk assessment report", "PD1", 0, 0, 0, 0, 1, 1, 1, 0
)

write.csv(tasks, "output/tasks.csv")

# Define the objectives data
objectives <- tasks %>%
  select(Objective) %>%
  distinct() %>%
  mutate(Task = Objective, HQP = "", Year1_Q1 = 1, Year1_Q2 = 1, Year1_Q3 = 1, Year1_Q4 = 1, Year2_Q1 = 1, Year2_Q2 = 1, Year2_Q3 = 1, Year2_Q4 = 1)

# Combine objectives and tasks
combined_data <- bind_rows(objectives, tasks) %>%
  arrange(Objective, Task)

# Transform data for plotting
plot_data <- combined_data %>%
  pivot_longer(cols = starts_with("Year"), names_to = "Quarter", values_to = "Value") %>%
  mutate(
    Quarter = factor(Quarter, levels = c("Year1_Q1", "Year1_Q2", "Year1_Q3", "Year1_Q4", "Year2_Q1", "Year2_Q2", "Year2_Q3", "Year2_Q4")),
    Quarter_Label = recode(Quarter,
                           "Year1_Q1" = "Q1",
                           "Year1_Q2" = "Q2",
                           "Year1_Q3" = "Q3",
                           "Year1_Q4" = "Q4",
                           "Year2_Q1" = "Q1",
                           "Year2_Q2" = "Q2",
                           "Year2_Q3" = "Q3",
                           "Year2_Q4" = "Q4"),
    Task = factor(Task, levels = rev(unique(Task))),  # Reverse the order of tasks
    Objective = factor(Objective, levels = unique(Objective))  # Factorize Objective for coloring
  )

# Ensure no paired values of 0/1 in the plot_data
plot_data <- plot_data %>%
  group_by(Task, Quarter, Quarter_Label, Objective) %>%
  summarise(Value = max(Value), .groups = 'drop') %>%
  ungroup()

# Verify that plot_data is correctly processed
head(plot_data)

# Create a function to customize y-axis labels with bold text for objectives
custom_y_labels <- function(labels) {
  sapply(labels, function(label) {
    if (label %in% unique(combined_data$Objective)) {
      paste0("<b>", label, "</b>")  # Make objectives bold using HTML tags
    } else {
      label  # Keep subtasks normal
    }
  })
}

# Create a function to customize y-axis labels with bold text and background fill for objectives
custom_y_labels_bg <- function(labels, objective_colors) {
  sapply(labels, function(label) {
    if (label %in% unique(combined_data$Objective)) {
      paste0("<span style='background-color:", objective_colors[label], "; padding:2px;'><b>", label, "</b></span>")
    } else {
      label
    }
  })
}

# Add HQP column as a separate data frame
hqp_data <- combined_data %>%
  filter(HQP != "") %>%
  mutate(Task = factor(Task, levels = rev(unique(Task)))) %>%
  select(Task, HQP)

# Define custom colors for each objective
objective_colors <- c("1. Determine whale distribution & population size" = "#a6cee3",
                      "2. Assess health of whales" = "#1f78b4",
                      "3. Evaluate risks of whale-fisheries interactions" = "#b2df8a")

# Create the Gantt chart
gantt_plot <- ggplot(plot_data, aes(x = interaction(Quarter, Quarter_Label), y = Task, fill = Objective, alpha = as.factor(Value))) +
  geom_tile(color = NA) +  # Remove the border color
  scale_fill_manual(values = objective_colors) +
  scale_alpha_manual(values = c("0" = 0, "1" = 1), guide = "none") +
  scale_x_discrete(labels = rep(c("Q1", "Q2", "Q3", "Q4"), 2)) +  # Customize x-axis labels
  scale_y_discrete(labels = function(x) custom_y_labels_bg(x, objective_colors)) +  # Customize y-axis labels with background fill for objectives
  theme_void() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_markdown(size = 10, margin = margin(r = 20), hjust = 0),  # Use element_markdown to render HTML tags
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text.y = element_blank(),  # Remove facet labels
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    panel.background = element_rect(fill = NA, color = NA),  # Set panel background to cream
    plot.background = element_rect(fill = NA, color = NA),  # Set plot background to cream
    plot.margin = margin(t = 40, r = 20, b = 80, l = 80),  # Increase left margin for HQP labels
    plot.subtitle = element_text(vjust = -.25, hjust = -.25)
  )

# Add year labels as annotation_custom
gantt_plot <- gantt_plot +
  annotation_custom(
    grob = textGrob("Year 1", x = unit(0.325, "npc"), y = unit(-0.1, "npc"), gp = gpar(fontface = "bold", fontsize = 12)),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  annotation_custom(
    grob = textGrob("Year 2", x = unit(0.725, "npc"), y = unit(-0.1, "npc"), gp = gpar(fontface = "bold", fontsize = 12)),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  coord_cartesian(clip = "off")

# Add HQP annotations as separate text labels with a smaller font size
gantt_plot <- gantt_plot +
  geom_text(data = hqp_data, aes(x = -0.5, y = Task, label = HQP), 
            inherit.aes = FALSE, hjust = 1, vjust = 0.5, size = 3) 

# Print the Gantt chart
print(gantt_plot)
