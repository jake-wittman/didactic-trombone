library(gm)
rotateScale <- function(scale, start_pos) {
  c(scale[start_pos:length(scale)], scale[1:(start_pos - 1)])
}
# C4 midi is 60. Subtract/Add 12 to change octave
# Bb4 (Bb above staff) is 70
# Pedal F is 29
# WWHWWWH
scale_major <- c(0, 2, 2, 1, 2, 2, 2, 1)
shifted <- rotateScale(scale_major, 2)
starting_pitch <- 36
num_octaves <- 3
min_pitch <- 29
max_pitch <- 70
pitch <- starting_pitch
total <- rep(scale_major, times = num_octaves)
for (i in 1:length(total)) {
  new_pitch <- pitch[i - 1] + total[i]
  pitch <- c(pitch, new_pitch)
}
pattern <- c(pitch, rev(pitch))
pattern[pattern < max_pitch & test > min_pitch]
