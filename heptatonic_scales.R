library(tidyverse)
source("C:/Users/jack.werner1/Documents/Music/src/music_functions.R")

### Enumerate scales?
### Symmetric scales

scales_7 <- get_scale_df(num_notes = 7, max_interval = 2)

ids_7 <- unique(scales_7$scale_id)
length(ids_7)
factorial(7)/(factorial(5)*factorial(2))

plot_scale(scale_id = ids_7[1])
plot_scales(scale_id = ids_7[1:4], psize = 1)

# Known scales
major_modes <- data.frame(name = c("ionian", "dorian", "phrygian", "lydian",
                                    "mixolydian", "aeolian", "locrian"),
                           binary = c(101011010101, 101101010110,
                                      110101011010, 101010110101,
                                      101011010110, 101101011010,
                                      110101101010)) %>%
  group_by(name) %>%
  mutate(scale_id = todec(binary)) %>%
  ungroup()

plot_scales(scale_id = major_modes$scale_id, psize = 2, scale_names = major_modes$name)


hm_modes <- data.frame(name = c("melodic minor", "hm b2", "lydian #5", 
                                "lydian dominant", "major minor", "half-diminished", 
                                "altered"),
                    binary = c(101101010101, 110101010110,
                               101010101101, 101010110110,
                               101011011010, 101101101010,
                               110110101010)) %>%
  group_by(name) %>%
  mutate(scale_id = todec(binary)) %>%
  ungroup()

plot_scales(scale_id = hm_modes$scale_id, psize = 2, scale_names = hm_modes$name)


w_modes <- data.frame(name = c("w1", "w2", "w3", "w4", "w5",
                               "w6", "w7"),
                       binary = c(101010101011, 101010101110,
                                  101010111010, 101011101010,
                                  101110101010, 111010101010,
                                  110101010101)) %>%
  group_by(name) %>%
  mutate(scale_id = todec(binary)) %>%
  ungroup()

plot_scales(scale_id = w_modes$scale_id, psize = 2, scale_names = hm_modes$name)


# Name degrees

major_degrees <- data.frame(scale_num = 1:7,
                            maj_note_num = c(1, 3, 5, 6, 8, 10, 12))


scales_7_deg <- scales_7 %>% 
  left_join(major_degrees, by = "scale_num") %>%
  group_by(scale_id, note) %>%
  mutate(flats = max(maj_note_num - note_num, 0),
         sharps = max(note_num - maj_note_num, 0),
         degree = paste0(paste0(rep("#", sharps), rep("b", flats), collapse = ""), scale_num)) %>%
  ungroup()


