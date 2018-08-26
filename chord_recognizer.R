library(tidyverse)
source("C:/Users/jack.werner1/Documents/Music/src/music_functions.R")


# Function
### Has a problem: if there's a higher interval, won't fix lower-interval crashes
  # Example: If there's a 13, it won't throw an error for an 11 and sharp 11
name_chord <- function(notes, root = notes[1], errors = F) {
  
  # Get numeric intervals above root
  intervals <- sapply(notes, interval, lownote = root)
  
  # If it has both sevenths, abort mission
  if (all(10:11 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}

  ### Chord quality
  if (4 %in% intervals) {
    if (10 %in% intervals) {
      
      extensions <- c("b9", "9", "#9", "3", "11", "#11", "5", "#5", "13", "b7", "7")
      
      # Dominant 7th chords
      if (sum(1:3 %in% intervals) > 1) {if (errors) {print("No chord found.")}; return(NA)}
      
      intervals <- c(sort(intervals[intervals %in% (7:8)]),
                     sort(intervals[!(intervals %in% (7:8))]))
      
      if (7 %in% intervals) {
        extensions[8] <- "b13"
        intervals <- c(sort(intervals[intervals == 7]),
                       sort(intervals[intervals != 7]))
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "13"
      } else if (5 %in% intervals) {
        if (all(6:7 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "11"
        extensions[6] <- "b5"
        intervals <- c(intervals[intervals == 6],
                       intervals[intervals != 6])
      } else if (2 %in% intervals) {
        if (any(c(1, 3) %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "9"
      } else {
        ext_1 <- "7"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 3, 6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
      
    } 
    else if (11 %in% intervals) {
      
      # Major 7th chords
      
      extensions <- c("b9", "9", "#9", "3", "11", "#11", "5", "#5", "13", "b7", "7")
      
      intervals <- c(sort(intervals[intervals %in% (7:8)]),
                     sort(intervals[!(intervals %in% (7:8))]))
      
      if (7 %in% intervals) {
        extensions[8] <- "b13"
        intervals <- c(sort(intervals[intervals == 7]),
                       sort(intervals[intervals != 7]))
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "maj13"
      } else if (5 %in% intervals) {
        if (all(6:7 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "maj11"
        extensions[6] <- "b5"
        intervals <- c(intervals[intervals == 6],
                       intervals[intervals != 6])
      } else if (2 %in% intervals) {
        if (any(c(1, 3) %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "maj9"
      } else {
        ext_1 <- "maj7"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 3, 6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
      
    } 
    else if (9 %in% intervals) {
      if (all(5:6 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
      if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
      if (sum(1:3 %in% intervals) > 1) {if (errors) {print("No chord found.")}; return(NA)}
      if (8 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}
      
      if (2 %in% intervals) {
        ext_1 <- "6/9"
      } else {
        ext_1 <- "6"
      }
      
      extensions <- c("b9", "9", "#9", "3", "11", "#11", "5", "b13", "13", "b7", "7")
      alterations <- c()
      
      alterations <- extensions[intersect(intervals, c(1, 3, 5, 6))]
      
      if (length(alterations) > 0) {alterations <- c("add", alterations)}
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
    else {
      if (sum(c(1:3, 5:6) %in% intervals) > 1) {if (errors) {print("No chord found.")}; return(NA)}
      if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
      
      ext_1 <- ""
      if (8 %in% intervals) ext_1 <- "aug"
      
      extensions <- c("addb9", "add9", "add#9", "", "add4", "add#11")
      
      alterations <- extensions[intersect(intervals, c(1:3, 5:6))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }} 
  else if (3 %in% intervals) {
    if (10 %in% intervals) {
      
      # Minor 7th chords
      
      extensions <- c("b9", "9", "b3", "3", "11", "b5", "5", "#5", "13", "b7", "7")
      
      intervals <- c(sort(intervals[intervals %in% (6:8)]),
                     sort(intervals[!(intervals %in% (6:8))]))
     
      if (7 %in% intervals) {
        extensions[8] <- "b13"
        extensions[6] <- "#11"
        intervals <- sort(intervals)
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "min13"
      } else if (5 %in% intervals) {
        if (all(6:7 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "min11"
      } else if (2 %in% intervals) {
        if (any(c(1, 3) %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "min9"
      } else {
        ext_1 <- "min7"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    } 
    else if (11 %in% intervals) {
      
      # Minor-major 7th chords
      
      extensions <- c("b9", "9", "b3", "3", "11", "b5", "5", "#5", "13", "b7", "7")
      
      intervals <- c(sort(intervals[intervals %in% (6:8)]),
                     sort(intervals[!(intervals %in% (6:8))]))
      
      if (7 %in% intervals) {
        extensions[8] <- "b13"
        extensions[6] <- "#11"
        intervals <- sort(intervals)
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "minmaj13"
      } else if (5 %in% intervals) {
        if (all(6:7 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "minmaj11"
      } else if (2 %in% intervals) {
        if (any(c(1, 3) %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "minmaj9"
      } else {
        ext_1 <- "minmaj7"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    } 
    else if (9 %in% intervals) {
      
      # Minor sixths and diminished sevenths
      if (all(5:6 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
      if (sum(1:2 %in% intervals) > 1) {if (errors) {print("No chord found.")}; return(NA)}
      if (8 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}
      
      intervals <- sort(intervals)
      
      if (6 %in% intervals & !(7 %in% intervals)) {
        ext_1 <- "dim7"
        extensions <- c("b9", "9", "b3", "3", "11", "", "5", "b13", "13", "b7", "7")
      } else if (2 %in% intervals) {
        ext_1 <- "min6/9"
        extensions <- c("b9", "", "b3", "3", "11", "#11", "5", "b13", "13", "b7", "7")
      } else {
        ext_1 <- "min6"
        extensions <- c("b9", "", "b3", "3", "11", "#11", "5", "b13", "13", "b7", "7")
      }
      
      alterations <- c()
      
      alterations <- extensions[intersect(intervals, c(1, 2, 5, 6))]
      
      if (sum(alterations != "") > 0) {alterations <- c("add", alterations)}
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    } 
    else {
      if (sum(c(1, 2, 5, 6) %in% intervals) > 1) {if (errors) {print("No chord found.")}; return(NA)}
      if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
      
      intervals <- sort(intervals)
      
      ext_1 <- "min"
      if (6 %in% intervals & !(7 %in% intervals)) {
        ext_1 <- "dim"
        extensions <- c("addb9", "add9", "", "", "add4", "")
      } else {
        extensions <- c("addb9", "add9", "", "", "add4", "add#11")
      }
      
      alterations <- extensions[intersect(intervals, c(1:3, 5:6))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    } } 
  else if (5 %in% intervals) {
    if (10 %in% intervals) {
      
      # Dominant 7th sus chords
      
      if (6 %in% intervals)  {if (errors) {print("No chord found.")}; return(NA)}
      
      extensions <- c("b9", "9", "#9", "3", "11", "#11", "5", "#5", "13", "b7", "7")
      
      intervals <- c(sort(intervals[intervals %in% (7:8)]),
                     sort(intervals[!(intervals %in% (7:8))]))
      
      if (7 %in% intervals) {
        extensions[8] <- "b13"
        intervals <- c(sort(intervals[intervals == 7]),
                       sort(intervals[intervals != 7]))
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "13sus4"
      } else if (2 %in% intervals) {
        if (any(c(1, 3) %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "9sus4"
      } else {
        ext_1 <- "7sus4"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
    else if (11 %in% intervals) {
      
      # Major 7th sus chords
      
      if (6 %in% intervals)  {if (errors) {print("No chord found.")}; return(NA)}
      
      extensions <- c("b9", "9", "#9", "3", "11", "#11", "5", "#5", "13", "b7", "7")
      
      intervals <- c(sort(intervals[intervals %in% (7:8)]),
                     sort(intervals[!(intervals %in% (7:8))]))
      
      if (7 %in% intervals) {
        extensions[8] <- "b13"
        intervals <- c(sort(intervals[intervals == 7]),
                       sort(intervals[intervals != 7]))
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "maj13sus4"
      } else if (2 %in% intervals) {
        if (any(c(1, 3) %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "maj9sus4"
      } else {
        ext_1 <- "maj7sus4"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
    else {
      if (6 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}
      
      intervals <- sort(intervals)
      
      ext_1 <- "sus4"
      
      if (7 %in% intervals) {
        if (all(1:2 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        if (all(8:9 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        extensions <- c("b9", "9", "", "", "", "", "", "b13", "6")
      } else {
        if (sum(c(1:2) %in% intervals) > 1) {if (errors) {print("No chord found.")}; return(NA)}
        if (8 %in% intervals) ext_1 <- "sus4#5"
        extensions <- c("b9", "9", "", "", "", "", "", "", "6")
      }
      
      alterations <- extensions[intersect(intervals, c(1:2, 8:9))]
      if (sum(alterations != "") > 0) {alterations <- c("add", alterations)}
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
  } 
  else if (2 %in% intervals) {
    if (10 %in% intervals) {
      
      # Dominant 7th sus2 chords
      
      if (1 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}

      # Fix interaction between 13 chord and b13 added note
      if (7 %in% intervals) {
        if (all(8:9 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        extensions <- c("", "", "", "", "", "#11", "", "b13", "", "", "")
        sort(intervals)
      } else {
        extensions <- c("", "", "", "", "", "#11", "", "#5", "", "", "")
        intervals <- c(sort(intervals[intervals %in% (8)]),
                       sort(intervals[!(intervals %in% (8))]))
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "13sus4"
      } else {
        ext_1 <- "7sus4"
      }
      
      alterations <- extensions[intersect(intervals, c(6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
    else if (11 %in% intervals) {
      
      # Major 7th sus2 chords
      
      if (1 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}
      
      # Fix interaction between 13 chord and b13 added note
      if (7 %in% intervals) {
        if (all(8:9 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        extensions <- c("", "", "", "", "", "#11", "", "b13", "", "", "")
        sort(intervals)
      } else {
        extensions <- c("", "", "", "", "", "#11", "", "#5", "", "", "")
        intervals <- c(sort(intervals[intervals %in% (8)]),
                       sort(intervals[!(intervals %in% (8))]))
      }
      
      if (9 %in% intervals) {
        if (all(7:8 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "maj13sus2"
      } else {
        ext_1 <- "maj7sus2"
      }
      
      alterations <- extensions[intersect(intervals, c(6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
    else {
      if (1 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}
      
      intervals <- c(intervals[intervals == 9],
                     sort(intervals[intervals != 9]))
      
      ext_1 <- "sus2"
      
      if (7 %in% intervals) {
        if (all(8:9 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
        extensions <- c("", "", "", "", "", "#11", "", "b13", "6")
      } else {
        if (8 %in% intervals) ext_1 <- "sus2#5"
        extensions <- c("", "", "", "", "", "#11", "", "", "6")
      }
      
      alterations <- extensions[intersect(intervals, c(6, 8:9))]
      if (sum(alterations != "") > 0) {alterations <- c("add", alterations)}
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
  } 
  else if (7 %in% intervals) {
    if (10 %in% intervals) {
      extensions <- c("b9", "", "", "", "", "#11", "", "b13", "")
      
      intervals <- sort(intervals)
      
      if (9 %in% intervals) {
        if (8 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "13no3"
      } else {
        ext_1 <- "7no3"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
    else if (11 %in% intervals) {
      extensions <- c("b9", "", "", "", "", "#11", "", "b13", "")
      
      intervals <- sort(intervals)
      
      if (9 %in% intervals) {
        if (8 %in% intervals) {if (errors) {print("No chord found.")}; return(NA)}
        ext_1 <- "maj13no3"
      } else {
        ext_1 <- "maj7no3"
      }
      
      alterations <- extensions[intersect(intervals, c(1, 6, 8))]
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
    else {
      if (all(8:9 %in% intervals)) {if (errors) {print("No chord found.")}; return(NA)}
      ext_1 <- "5"
      
      intervals <- c(intervals[intervals == 9],
                     sort(intervals[intervals != 9]))
      
      extensions <- c("b9", "", "", "", "", "#11", "", "b13", "6")
      
      alterations <- extensions[intersect(intervals, c(1, 6, 8, 9))]
      
      if (sum(alterations != "") > 0) alterations <- c("add", alterations)
      
      return(paste0(c(root, ext_1, alterations), collapse = ""))
    }
  }
  else {
    if (errors) {print("No chord found.")}; return(NA)
  }
}

name_chord(c("C", "E", "G", "B", "D"))


# How many of all C chords are named?
all_notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
               "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")

nlist <- rep(list(0:1), 12)
names(nlist) <- all_notes

chords_df <- expand.grid(nlist) %>%
  filter(C == 1)

bin <- chords_df %>%
  as.matrix() %>%
  apply(1, function(x){as.numeric(paste0(x, collapse = ""))})

chords_df$chord <- chords_df %>%
  as.matrix() %>%
  apply(1, function(x){name_chord(all_notes[which(x == 1)])})

chords_df$binary <- bin

chords_df <- chords_df %>% filter(!is.na(chord))


# Plot random chords for checking purposes

r <- sample(1:nrow(chords_df), 1)

plot_scale(binary = chords_df$binary[r]) + labs(title = chords_df$chord[r])

















