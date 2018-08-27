library(tidyverse)


###################
# Scale Functions #
###################

# Get half steps between notes
interval <- function(lownote, highnote) {
  notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
             "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  
  lownum <- which(notes == lownote)
  highnum <- which(notes == highnote)
  
  int <- (highnum - lownum) %% 12
  
  return(int)
}

# Transpose a note up or down a number of half steps
transpose_notes <- function(note, halfsteps, up = TRUE) {
  notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
             "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  
  note_num <- which(notes == note)
  
  newnote_num <- (note_num + up*halfsteps - (!up)*halfsteps) %% 12
  if (newnote_num == 0) newnote_num <- 12
  
  newnote <- factor(notes[newnote_num], levels = notes)
  
  return(newnote)
}

# Transpose a key up or down a number of half steps (includes octave #)
transpose_keys <- function(key, halfsteps, up = TRUE) {
  notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
             "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  
  note <- gsub("[0-9]+", "", key)
  octave_num <- gsub("[A-Z]", "", key) %>% as.numeric()
  
  octaves <- floor(halfsteps/12)
  
  leftover <- halfsteps %% 12
  
  if (up) carryover <- (which(notes == note) + leftover) > 12
  else carryover <- (which(notes == note) - leftover) < 1
  
  octave_change <- (octaves + carryover)
  if (!up) octave_change <- -1*octave_change
  
  newkey <- paste0(
    transpose_notes(note = note, halfsteps = halfsteps, up = up),
    octave_num + octave_change
  )
  
  return(newkey)
}

# Data frame of all (or only some) scales
get_scale_df <- function(num_notes = NA, max_interval = NA, interval_limits = NA, starting_note = "C") {
  notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F", 
             "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  
  
  nlist <- rep(list(0:1), 12)
  names(nlist) <- notes[12:1]
  
  out <- expand.grid(nlist) %>%
    mutate(scale_id = 0:(n()-1)) %>%
    gather(key = "note", value = "included", -scale_id) %>%
    mutate(note = factor(note, levels = notes)) %>%
    arrange(scale_id, note) %>%
    mutate(note_num = rep(1:12, 4096)) %>%
    arrange(scale_id, note) %>%
    group_by(scale_id, included) %>%
    mutate(scale_num = ifelse(included == 1, 1:n(), NA)) %>%
    ungroup() %>%
    filter(included == 1) %>% select(-included) %>%
    arrange(scale_id, note) %>%
    group_by(scale_id) %>%
    mutate(interval = ifelse(row_number() < n(), lead(note_num)-note_num, first(note_num)-note_num+12)) %>%
    ungroup() %>%
    group_by(scale_id) %>%
    filter(any(note == "C")) %>%
    ungroup()
  
  if (!is.na(num_notes)) {
    out <- out %>%
      group_by(scale_id) %>%
      filter(n() %in% num_notes) %>%
      ungroup()
  }
  
  if (!is.na(max_interval)) {
    out <- out %>%
      group_by(scale_id) %>%
      filter(max(interval) <= max_interval) %>%
      ungroup()
  }
  
  if (!is.na(interval_limits)) {
    lims <- rep(0, 12)
    lims[1:length(interval_limits)] <- interval_limits
    
    ### Add logic ###
    out <- out %>%
      group_by(scale_id) %>%
      filter(sum(interval == 1) <= lims[1],
             sum(interval == 2) <= lims[2],
             sum(interval == 3) <= lims[3],
             sum(interval == 4) <= lims[4],
             sum(interval == 5) <= lims[5],
             sum(interval == 6) <= lims[6],
             sum(interval == 7) <= lims[7],
             sum(interval == 8) <= lims[8],
             sum(interval == 9) <= lims[9],
             sum(interval == 10) <= lims[10],
             sum(interval == 11) <= lims[11],
             sum(interval == 12) <= lims[12]) %>%
      ungroup()
  }
  
  if (starting_note != "C") {
    out$note <- sapply(out$note, transpose_notes, interval("C", starting_note))
  }
  
  return(out)
}

# Get notes from scale ID or binary version
get_notes <- function(binary = NA, scale_id = NA, starting_note = "C") {
  
  if (is.na(binary)) {
    binary <- tobin(scale_id)
  }
  
  all_notes <- notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F", 
                          "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  
  vec <- binary %>% as.character() %>% strsplit("") %>% .[[1]]
  
  notes_out <- all_notes[vec == "1"] %>% sapply(transpose_notes, interval("C", starting_note))
  names(notes_out) <- NULL
  
  return(notes_out)
}

# Plot a scale in any key on any range of the piano
plot_scale <- function(notes = NA, binary = NA, scale_id = NA, starting_note = "C",
                       low_note = paste0(starting_note, 1), high_note = paste0(starting_note, 2)) {
  
  if(all(is.na(notes))) {
    if(is.na(binary)) {
      notes <- get_notes(scale_id = scale_id, starting_note = starting_note)
    } else {
      notes <- get_notes(binary = binary, starting_note = starting_note)
    }
  }
  
  dots <- keydots_df(low_note, high_note) %>% filter(note %in% notes)
  
  out <- keyboard_plot(low_note, high_note) + 
    geom_point(data = dots, aes(x = px, y = py), color = "red", size = 8)
  
  return(out)
}

# Plot multiple scales in any key on any range of the piano
plot_scales <- function(scale_id = NA, binary = NA, scale_names = NA, psize = 8,
                        starting_note = "C", low_note = "C1", high_note = "C2",
                        color_tonic = FALSE) {
  
  if(all(is.na(scale_id))) {
    scale_id <- sapply(binary, todec)
  }
  
  sid <- scale_id
  
  scale_info_df <- data.frame(scale_id = scale_id,
                              starting_note = starting_note)
  
  if (all(is.na(scale_names))) {
    scale_info_df$name <- factor(scale_id, levels = scale_id)
  } else {
    scale_info_df$name <- factor(scale_names, levels = scale_names)
  }
  
  dots <- get_scale_df() %>% filter(scale_id %in% sid) %>%
    left_join(scale_info_df, by = "scale_id") %>%
    group_by(scale_id, note_num) %>%
    mutate(note = transpose_notes(note, interval("C", starting_note)),
           tonic = ifelse(as.character(note) == as.character(starting_note), "Tonic", "Not Tonic")) %>%
    ungroup() %>%
    left_join(keydots_df(low_note, high_note), by = c("note"))
    
  if (color_tonic) {
    out <- keyboard_plot(low_note, high_note) + 
      geom_point(data = dots, aes(x = px, y = py, color = tonic), size = psize) +
      facet_wrap(~name) +
      scale_color_manual(values = c("Tonic" = "red", "Not Tonic" = "blue"))
  } else {
    out <- keyboard_plot(low_note, high_note) + 
      geom_point(data = dots, aes(x = px, y = py), color = "red", size = psize) +
      facet_wrap(~name)
  }
  
  return(out)
}

###########################
# Binary helper functions #
###########################

# List powers of two that sum to a given number (helper to "tobin")
listTwos <- function(dec) {
  
  if (dec == 0) {
    return(c())
  } else if (dec==1) {
    return(1)
  } else if (dec < 1) {
    return(NA)
  } else {
    l2 <- log(dec, base=2)
    
    subt <- 2^(floor(l2))
    
    left <- dec-subt
    
    return(c(subt, listTwos(left)))
  }
}

# Convert decimal to binary
tobin <- function(dec) {
  places <- log(listTwos(dec), base=2)
  
  vec <- rep("0", max(places)+1)
  vec[max(places)-places+1] <- 1
  
  num <- vec %>% paste0(collapse = "") %>% as.numeric()
  
  return(num)
}

# Convert binary to decimal
todec <- function(bin) {
  aschar <- bin %>% as.character() %>% strsplit("") %>% .[[1]]
  
  pows <- ((length(aschar)-1):0)[aschar == "1"]
  
  num <- sum(2^pows)
  
  return(num)
}


######################
# Plotting Functions #
######################

# Create a sequence of notes (note names and octave numbers)
note_sequence <- function(lowkey = "C1", highkey = "C2") {
  lowNote <- gsub("[0-9]*", "", lowkey)
  lowNum <- gsub("[^0-9]*", "", lowkey) %>% as.numeric()
  highNote <- gsub("[0-9]*", "", highkey)
  highNum <- gsub("[^0-9]*", "", highkey) %>% as.numeric()
  
  notes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
             "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  
  
  firstOct <- notes[which(notes == lowNote):length(notes)] %>% paste0(lowNum)
  lastOct <- notes[1:which(notes == highNote)] %>% paste0(highNum)
  
  if ((highNum - lowNum) > 1) {
    middleOcts <- paste0(
      rep(notes, highNum - lowNum - 1),
      rep((lowNum+1):(highNum-1), each = 12)
    )
  } else if ((highNum - lowNum) == 1) {
    middleOcts <- c()
  } else {
    middleOcts <- c()
    lastOct <- c()
  }
  
  out <- c(firstOct, middleOcts, lastOct)
  
  return(out)
}

# Create a dataframe of notes for plotting
### DF contains key, key color, position/size of key for plotting
keyboard_df <- function(lowkey = "C1", highkey = "C2",
                        white_key_height = 150, white_key_width = 23.5,
                        black_key_height = 90, black_key_width = 13.7) {
  
  notes <- note_sequence(lowkey = lowkey, highkey = highkey)
  
  keys <- data.frame(key = notes,
                     note = gsub("[0-9]*", "", notes),
                     octave = gsub("[^0-9]*", "", notes),
                     stringsAsFactors = F) %>%
    mutate(keytype = ifelse(grepl("#", notes), "black", "white"),
           cy = ifelse(keytype == "white", white_key_height/2, white_key_height - black_key_height/2),
           w = ifelse(keytype == "white", white_key_width, black_key_width),
           h = ifelse(keytype == "white", white_key_height, black_key_height))
  
  keys$cx[keys$keytype == "white"] <- 1:sum(keys$keytype == "white")*white_key_width + white_key_width/2
  
  out <- keys %>%
    mutate(cx = ifelse(is.na(cx),
                       lag(cx, default = white_key_width/2) + white_key_width/2,
                       cx)) %>%
    arrange(desc(keytype))
  
  return(out)
}

# Create a dataframe of key dots for plotting
keydots_df <- function(lowkey = "C1", highkey = "C2",
                       white_key_height = 150, white_key_width = 23.5,
                       black_key_height = 90, black_key_width = 13.7) {
  
  notes <- note_sequence(lowkey = lowkey, highkey = highkey)
  
  allnotes <- c("C", "C#/Db", "D", "D#/Eb", "E", "F",
             "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
  
  dots <- data.frame(key = notes,
                     note = factor(gsub("[0-9]*", "", notes), levels = allnotes),
                     octave = gsub("[^0-9]*", "", notes),
                     stringsAsFactors = F) %>%
    mutate(keytype = ifelse(grepl("#", notes), "black", "white"),
           py = ifelse(keytype == "white", 
                       (white_key_height - black_key_height)/2, 
                       white_key_height - .75*black_key_height))
  
  dots$px[dots$keytype == "white"] <- 1:sum(dots$keytype == "white")*white_key_width + white_key_width/2
  
  out <- dots %>%
    mutate(px = ifelse(is.na(px),
                       lag(px, default = white_key_width/2) + white_key_width/2,
                       px)) %>%
    arrange(desc(keytype))
  
  return(out)
}

# Plots a given range of notes on a keyboard
keyboard_plot <- function(lowkey = "C1", highkey = "C2",
                          white_key_height = 150, white_key_width = 23.5,
                          black_key_height = 90, black_key_width = 13.7) {
  keys <- keyboard_df(lowkey = lowkey, highkey = highkey,
                      white_key_height = white_key_height, white_key_width = white_key_width,
                      black_key_height = black_key_height, black_key_width = black_key_width)
  
  suppressWarnings(
    out <- ggplot(data = keys) + 
      geom_tile(aes(x = cx, y = cy, width = w, height = h, fill = keytype), color = "black") +
      coord_fixed() +
      scale_fill_manual(values = c("white" = "white", "black" = "black")) +
      theme_void() +
      theme(legend.position = "none")
  )
  
  return(out)
}

# Plots a keyboard with key colors based on given frequencies
keyboard_color_plot <- function(colored_keys = c("C1", "D1", "G1", "B1"),
                                color_weights = rep(1, length(colored_keys)),
                                lowkey = "C1", highkey = "C2",
                                low_color = "white", high_color = "red",
                                white_key_height = 150, white_key_width = 23.5,
                                black_key_height = 90, black_key_width = 13.7,
                                background_color = "white") {
  keys <- keyboard_df(lowkey = lowkey, highkey = highkey,
                      white_key_height = white_key_height, white_key_width = white_key_width,
                      black_key_height = black_key_height, black_key_width = black_key_width)
  
  
  if (all(grepl("[0-9]", colored_keys))) {
    
    colors <- data.frame(key = colored_keys, color = color_weights, stringsAsFactors = F)
    
    suppressWarnings(
      keys_colors <- keys %>% left_join(colors, by = "key") %>%
        mutate(color = ifelse(is.na(color), 0, color))
    )
    
  } else if (all(!grepl("[0-9]", colored_keys))) {
    colors <- data.frame(note = colored_keys, color = color_weights, stringsAsFactors = F)
    
    suppressWarnings(
      keys_colors <- keys %>% left_join(colors, by = "note") %>%
        mutate(color = ifelse(is.na(color), 0, color))
    )
    
  } else {
    stop("Mixed note types")
  }
  
  
  suppressWarnings(
    out <- ggplot(data = keys_colors) + 
      geom_tile(aes(x = cx, y = cy, width = w, height = h, fill = color), color = "black") +
      coord_fixed() +
      scale_fill_gradient(low = low_color, high = high_color) +
      theme_void() +
      theme(legend.position = "none",
            plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color)
            )
  )
  
  return(out)
}

















