library(dplyr)


# Dimensions of a circle
r <- 1
A <- r^2*pi
U <- 2*r*pi
paste("The area of a circle with radius ", r, " is ", A, "whereas its circumference is ", U)


# Sequence of numbers
seq1 <- seq(from = 0, to = 20, length.out = 5)
seq2 <- seq(from = pi, to = 100*pi, length.out = 5)

# Gauss sum
vec1 <- seq(from = 1, to = 50)
vec2 <- seq(from = 51, to = 100)
GaussSum <- sum(vec1 + rev(vec2))

# Magic trick algorithm
y <- 5
x <- 2*(y + 1)
x <- x/2
x <- x -1                 
print(x)

# Vectors
rivers <- datasets::rivers
class(rivers) # numeric
length(rivers) # length is 141
mean <- mean(rivers)
median <-  median(rivers)
min <-min(rivers)
max <-  max(rivers)
quantile33 <- quantile(rivers, 0.33)

# Data frames
quakes <- datasets::quakes
print(quakes)
dim(quakes)
mag <- quakes$mag
max_mag <- max(mag)
geopos_max_mag <- subset(quakes, mag == max_mag) |> select(lat, long)
