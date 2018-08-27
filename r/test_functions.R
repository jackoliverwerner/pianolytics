library(tidyverse)
source("C:/Users/jack.werner1/Documents/Music/src/music_functions.R")

# Keys Plot
(piano <- keyboard_plot("C1", "C2"))

# Scale dots
full_dots <- keydots_df("C1", "C2")
piano + geom_point(data = full_dots, aes(x = px, y = py), color = "red", size = 8)

# Enumerate Scales
scales <- get_scale_df(num_notes = 7, max_interval = 2)
head(scales)

# Plot scales
plot_scales(scale_id = c(2773, 2906))
plot_scales(binary = c(101010101010, 110110110110), scale_names = c("Whole-Tone", "Diminished"))
