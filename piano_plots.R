library(tidyverse)

source("C:/Users/jack.werner1/Documents/Music/src/music_functions.R")

setwd("C:/Users/jack.werner1/Documents/Music/src")



# Mary Had a Little Lamb
note_string <- "EDCDEEEDDDEGGEDCDEEEEDDEDC"
note_vec <- strsplit(note_string, "")[[1]]
note_df <- table(note_vec) %>% data.frame()

keyboard_color_plot(note_df$note_vec, note_df$Freq, highkey = "B1")


# Danny Boy

notes <- LETTERS[c(3:7, 1:2)]

num_string <- "712323653216134565313271232365321671234321215671776565315671776532555322161531712365321611"
num_vec <- strsplit(num_string, "")[[1]] %>% as.numeric()
note_vec <- sapply(num_vec, function(x){notes[x]})


octave_string <- c("122222222221222222222212222222222112222222222223222222222223222222222333323222122222222122")
octave_vec <- num_vec <- strsplit(octave_string, "")[[1]]

key_vec <- paste0(note_vec, octave_vec)
key_df <- table(key_vec) %>% data.frame()

keyboard_color_plot(key_df$key_vec, key_df$Freq, lowkey = "C1", highkey = "C4")



