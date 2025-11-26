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
min_pitch <- 29 # Pedal F
max_pitch <- 72 # C5
pitch <- starting_pitch
total <- rep(scale_major, times = num_octaves)
for (i in 1:length(total)) {
  new_pitch <- pitch[i - 1] + total[i]
  pitch <- c(pitch, new_pitch)
}
pattern <- c(pitch, rev(pitch))
pattern <- rle(pattern[pattern <= max_pitch & pattern >= min_pitch])$values

c(36, 38, 40, 41, 43, 45, 47, 48)
c(40, 41, 43, 45, 47, 48, 50)
c(36, 40, 38, 41, 40, 43, 41, 45, 43, 47, 45, 48, 47, 50, 48)

# Testing interweaving the vectors
A = c(36, 38, 40, 41, 43, 45, 47, 48)
B = c(40, 41, 43, 45, 47, 48, 50)

# Pad the shorter vector with NA
max_len <- max(length(A), length(B))
length(A) <- max_len
length(B) <- max_len

C <- as.vector(rbind(A, B))
C <- C[!is.na(C)] # Remove NA values
print(C)

.shiftPattern <- function(shift, scale) {
  scale_shifted <- scale[shift:length(scale)]
  length(scale_shifted) <- length(scale)
  out <- as.vector(rbind(scale, scale_shifted))
  out <- out[!is.na(out)]
  return(out)
}


.generateScale <- function(
  scale_key,
  scale_pattern,
  starting_pitch,
  num_octaves,
  min_pitch,
  max_pitch
) {
  pitch <- starting_pitch
  total <- rep(scale_pattern, times = num_octaves)
  for (i in 1:length(total)) {
    new_pitch <- pitch[i - 1] + total[i]
    pitch <- c(pitch, new_pitch)
  }
  pattern <- c(pitch, rev(pitch))
  out <- rle(pattern[pattern <= max_pitch & pattern >= min_pitch])$values
  out
}


generateScalePattern <- function(
  scale_key,
  major_or_minor,
  scale_pattern,
  shift,
  starting_pitch,
  num_octaves,
  min_pitch = 29,
  max_pitch = 72
) {
  keys <- data.frame(
    major_name = c(
      'C-',
      'G-',
      'D-',
      'A-',
      'E-',
      'B-',
      'F',
      'C',
      'G',
      'D',
      'A',
      'E',
      'B',
      'F#',
      'C#'
    ),
    minor_name = c(
      'A-',
      'E-',
      'B-',
      'F',
      'C',
      'G',
      'D',
      'A',
      'E',
      'B',
      'F#',
      'C#',
      'G#',
      'D#',
      'A#'
    ),
    value = -7:7
  )
  scale <- .generateScale(
    scale_pattern = scale_pattern,
    starting_pitch = starting_pitch,
    num_octaves = num_octaves,
    shift = shift,
    min_pitch = min_pitch,
    max_pitch = max_pitch
  )

  if (major_or_minor == 'major') {
    key <- keys$value[keys$major_name == scale_key]
  } else if (major_or_minor == 'minor') {
    key <- keys$value[keys$minor_name == scale_key]
  }

  pattern <- .shiftPattern(scale = scale)

  Music() +
    Meter(4, 4) +
    Line(pattern, durations = c(0.5, rep(0.25, 6))) +
    Clef('F') +
    Key(key)
}

generateScalePattern(
  scale_key = 'C',
  major_or_minor = 'major',
  scale_pattern = scale_major,
  shift = 3,
  starting_pitch = 36,
  num_octaves = 3
)

generateScalePattern(
  scale_key = 'A-',
  major_or_minor = 'major',
  scale_pattern = scale_major,
  shift = 3,
  starting_pitch = 32,
  num_octaves = 3
)

generateScalePattern(
  scale_key = 'A-',
  major_or_minor = 'major',
  scale_pattern = scale_major,
  shift = 4,
  starting_pitch = 32,
  num_octaves = 3
)
