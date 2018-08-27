library(tidyverse)
source("C:/Users/jack.werner1/Documents/Music/src/music_functions.R")


# Combine root and chord type into chord name
make_chord_name <- function(root = "C", type = "major") {
  if (type == "major") suf <- "_maj"
  else suf <- "_min"
  
  return(paste0(root, suf))
}

# Get root from a chord name
extract_root <- function(chord_name) {
  return(gsub("_.*", "", chord_name))
}

# Get type from a chord name
extract_type <- function(chord_name) {
  suf <- gsub(".*_", "", chord_name)
  
  if (suf == "maj") out <- "major"
  else out <- "minor"
  
  return(out)
}

# Put a chord in triad order (root, third, fifth)
order_chord <- function(notes = c("G", "C", "E")) {
  for (i in notes) {
    for (j in notes) {
      if (i == j) next
      if (interval(i, j) == 7) {
        root <- i
        fifth <- j
        third <- notes[notes != root & notes != fifth]
        return(c(root, third, fifth))
      } else if (interval(i, j) == 5) {
        root <- j
        fifth <- i
        third <- notes[notes != root & notes != fifth]
        return(c(root, third, fifth))
      }
    }
  }
  print("Something went wrong...")
  return(NA)
}

# Get chord name from notes
get_chord <- function(notes = c("C", "E", "G")) {
  notes <- order_chord(notes = notes)
  
  root <- notes[1]
  type <- ifelse(interval(notes[1], notes[2]) == 4, "major", "minor")
  
  return(make_chord_name(root = root, type = type))
}

# Get notes from chord name
get_notes <- function(chord_name = "C_maj") {
  notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
             "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  notes <- factor(notes, levels = notes)
  
  root <- extract_root(chord_name)
  type <- extract_type(chord_name)
  
  third <- transpose_notes(root, 3 + (type == "major")) %>% as.character()
  fifth <- transpose_notes(root, 7) %>% as.character()
  
  chord <- c(root, third, fifth); names(chord) <- c("root", "third", "fifth")
  
  return(chord)
}

# Apply a Neo-Riemannian Theory transformation
nrt <- function(chord_name = "C_maj", transformation = "P") {
  root <- extract_root(chord_name)
  
  
  if (extract_type(chord_name = chord_name) == "major") {
    type <- "minor"
    
    if (transformation == "L") root <- transpose_notes(root, 4)
    if (transformation == "R") root <- transpose_notes(root, 3, up = F)
  } else {
    type <- "major"
    
    if (transformation == "L") root <- transpose_notes(root, 4, up = F)
    if (transformation == "R") root <- transpose_notes(root, 3)
  }
  
  return(make_chord_name(root = root, type = type))
}

nrt_mult <- function(chord_name, transformations = "LPR") {
  trans <- strsplit(transformations, "")[[1]]
  
  for (i in trans) {
    chord_name <- nrt(chord_name = chord_name, transformation = i)
  }
  
  return(chord_name)
}

all_nrt <- function(chord_name = "C_maj") {
  out <- sapply(c("L", "P", "R"), nrt, chord_name = chord_name)
  return(out)
}


x <- "C_maj"
(x <- sapply(x, all_nrt))


######################################
# Graph of transformations (tonnetz) #
######################################

### Create graph ###
notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
           "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")

chords <- c(paste0(notes, "_maj"), paste0(notes, "_min"))

trans_chords <- sapply(chords, all_nrt)

edges_df <- data.frame(e1 = rep(chords, each = 3),
                    e2 = as.vector(trans_chords),
                    stringsAsFactors = F)

edges <- matrix(0, nrow = length(chords), ncol = length(chords))
colnames(edges) <- chords; rownames(edges) <- chords

for (i in 1:nrow(edges_df)) {
  edges[edges_df$e1[i], edges_df$e2[i]] <- 1
}


### Find edge lengths (Floyd-Warshall) ###

# Initialize with high values
dist <- matrix(99999, nrow = length(chords), ncol = length(chords))
colnames(dist) <- chords; rownames(dist) <- chords

# Set edges to 1
for (i in 1:nrow(edges_df)) {
  dist[edges_df$e1[i], edges_df$e2[i]] <- 1
}

# Set diag to 0
diag(dist) <- 0

for (k in chords) {
  for (i in chords) {
    for (j in chords) {
      dist[i, j] <- min(dist[i, j], (dist[i, k] + dist[k, j]))
    }
  }
}


### Find path (Djikstra) ###

start <- "C_maj"
end <- "A#/Bb_min"
inf_val <- 9999

nodes <- data.frame(chord = chords,
                    stringsAsFactors = F) %>%
  mutate(current = chord == start,
         visited = FALSE,
         dist = ifelse(current, 0, inf_val))

for (i in 1:100) {
  print(i)
  
  current_node <- nodes$chord[nodes$current]
  current_dist <- nodes$dist[nodes$current]
  
  nodes <- nodes %>%
    mutate(edge = ifelse(edges[,current_node] == 0, inf_val, 1),
           dist_tent = current_dist + edge,
           dist = pmin(dist, dist_tent))
  
  nodes$visited[nodes$chord == current_node] <- TRUE
  
  next_node <- nodes %>% filter(!visited) %>% filter(dist == min(dist))
  
  nodes$current <- ifelse(nodes$chord == next_node$chord[1], T, F)
  
  print(nodes)
  
  if (current_dist == inf_val) break
  if (nodes$visited[nodes$chord == end]) break
}









