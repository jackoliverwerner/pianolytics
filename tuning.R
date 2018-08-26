library(tidyverse)
library(scales)


#####################
# Equal Temperament #
#####################

fundamental <- 440

halfstep <- 2^(1/12)

scale.et <- fundamental*(halfstep^(0:12))


et.df <- data.frame(note = 1:13, 
                    freq = scale.et, 
                    rat = scale.et/fundamental, 
                    freq_log = log(scale.et, base = halfstep),
                    rat_log = log(scale.et/fundamental, base = halfstep),
                    key = c("w", "b", "w", "b", "w", "w", "b", "w", "b", "w", "b", "w", "w"))

ggplot(data = et.df, aes(x = note, y = freq)) + geom_point()

ggplot(data = et.df, aes(x = note, y = freq)) + geom_point() + coord_trans(y=log_trans(halfstep))


###################
# Harmonic Series #
###################

fundamental <- 440
overtones <- 63

series <- fundamental*1:overtones

# Finding how many times we divide by 2 before it's between 440 and 880
# It uses logarithms, it's a whole thing
series.oct <- series/(2^floor(log(series/fundamental, base=2)))




harm.df <- data.frame(freq = series.oct, 
                      octave = floor(log(series/440, base=2)),
                      overtone = 1:overtones)


ggplot(data = et.df) + geom_hline(aes(yintercept = freq, color = key)) + 
  geom_point(data = harm.df, aes(x = octave, y = freq), color = "red") +
  scale_color_manual(values = c("grey60", "grey10")) +
  coord_trans(y=log_trans(halfstep))

ggplot(data = et.df) + geom_hline(aes(yintercept = freq)) + 
  geom_line(data = harm.df, aes(x = octave, y = freq, group = freq), 
            color = "red", size = 1.5) +
  #geom_point(data = harm.df, aes(x = octaves, y = freq), color = "red") +
  coord_trans(y=log_trans(halfstep))


##########
# Fifths #
##########

fundamental <- 440

scale.pre <- 440*(3/2)^(0:11)
scale <- scale.pre/(2^floor(log(scale.pre/fundamental, base=2)))

fifths.df <- data.frame(note = 1:12, freq = sort(scale))

ggplot(data = et.df) + geom_hline(aes(yintercept = freq, color = key)) + 
  geom_point(data=fifths.df, aes(x = note, y = freq), color = "red") +
  scale_color_manual(values = c("grey60", "grey10")) +
  coord_trans(y=log_trans(halfstep))


###############
# Pythagorean #
###############


fundamental <- 440

scale.up.pre <- 440*(3/2)^(0:6)
scale.up <- scale.up.pre/(2^floor(log(scale.up.pre/fundamental, base=2)))

scale.down.pre <- 440*(2/3)^(1:6)
scale.down <- scale.down.pre/(2^floor(log(scale.down.pre/fundamental, base=2)))

pyth.df <- data.frame(note = c(1:7, 7:12), freq = sort(c(scale.up, scale.down)))

ggplot(data = et.df) + geom_hline(aes(yintercept = freq, color = key)) + 
  geom_point(data=pyth.df, aes(x = note, y = freq), color = "red") +
  scale_color_manual(values = c("grey60", "grey10")) +
  coord_trans(y=log_trans(halfstep))


###################
# Just Intonation #
###################

fundamental <- 440

scale <- fundamental*c(1, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8, 2)

just.df <- data.frame(note = c(1, 3, 5, 6, 8, 10, 12, 13),
                      freq = sort(scale))

ggplot(data = et.df) + geom_hline(aes(yintercept = freq, color = key)) + 
  geom_point(data=just.df, aes(x = note, y = freq), color = "red") +
  scale_color_manual(values = c("grey60", "grey10")) +
  coord_trans(y=log_trans(halfstep))



#####################
# All low fractions #
#####################

max.denom <- 10
fundamental <- 440

# Up and down
fracs.both <- expand.grid(int1 = 1:(2*max.denom), int2 = 1:max.denom) %>%
  mutate(up = int1/int2, down = 2*int2/int1,
         highest.int = pmax(int1, int2)) %>%
  filter(int1 > int2, int1 < 2*int2) %>%
  gather(key = "direction", value = "rat", up, down) %>%
  mutate(num = ifelse(direction == "up", int1, 2*int2),
         denom = ifelse(direction == "up", int2, int1)) %>%
  group_by(rat, direction) %>% filter(highest.int == min(highest.int)) %>% ungroup() %>%
  mutate(freq = fundamental*rat)


ggplot(data = et.df) + geom_hline(aes(yintercept = freq, color = key)) + 
  geom_point(data=fracs.both, aes(x = highest.int, y = freq, color = direction)) +
  scale_color_manual(values = c("b" = "grey60", "w" = "grey10",
                                "down" = "red", "up" = "blue")) +
  labs(x = "Highest Integer in Ratio") +
  coord_trans(y=log_trans(halfstep))



####################################
# All Intervals in Just Intonation #
####################################

fundamental <- 440

names <- c("c", "d", "e", "f", "g", "a", "b")
names <- factor(names, levels=names, ordered = T)

rat.nums <- c(1, 9, 5, 4, 3, 5, 15)
rat.denoms <- c(1, 8, 4, 3, 2, 3, 8)


rats <- array(0, dim = c(8, 8, 2))

for (i in 1:8) {
  for (j in 1:8) {
    rats[i, j, 1] <- rat.nums[j]*rat.denoms[i]
    rats[i, j, 2] <- rat.denoms[j]*rat.nums[i]
  }
}

ints.df <- data.frame(note1 = names, note2 = rep(names, each = length(names)),
                      num1 = rat.nums, denom1 = rat.denoms,
                      num2 = rep(rat.nums, each = length(names)), 
                      denom2 = rep(rat.denoms, each = length(names))) %>%
  mutate(rat1 = num1/denom1, rat2 = num2/denom2,
         num2 = ifelse(rat2 < rat1, 2*num2, num2),
         num = num2*denom1, denom = denom2*num1,
         rat = num/denom, freq = num/denom*fundamental)
         

# Relative to Equal Tempered
ggplot(data = et.df) + geom_hline(aes(yintercept = freq, color = key)) + 
  geom_point(data=ints.df, aes(x = note1, y = freq)) +
  scale_color_manual(values = c("b" = "grey60", "w" = "grey10")) +
  labs(x = "Highest Integer in Ratio") +
  coord_trans(y=log_trans(halfstep))

# Relative to Just Intonation
just <- ints.df %>% filter(note1 == "c")


ggplot(data = just) + geom_hline(aes(yintercept = freq), color = "blue") + 
  #geom_hline(data = et.df, aes(yintercept = freq, color = key)) + 
  geom_point(data=ints.df, aes(x = note1, y = freq)) +
  scale_color_manual(values = c("b" = "grey60", "w" = "grey10")) +
  labs(x = "Highest Integer in Ratio") +
  coord_trans(y=log_trans(halfstep))



####################
# Shared Overtones #
####################

f1 <- 440
f2 <- f1*7/6

max.over <- 20

overtones.f1 <- data.frame(freq = f1*(1:max.over), x1 = 1, x2 = 2)
overtones.f2 <- data.frame(freq = f2*(1:max.over), x1 = 2, x2 = 3)

overtones <- rbind(overtones.f1, overtones.f2) %>% filter(freq <= max(overtones.f1$freq))

ggplot(data = overtones, aes(x = x1, xend = x2, y = freq, yend = freq)) + geom_segment()














