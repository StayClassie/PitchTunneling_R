library(tidyverse)
library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)
library(lme4)
library(pitchRx)



MetsData <- read.csv(file="20190924-ChaseField-1.csv", header=TRUE, sep=",")


MetsData %>% filter (pitcher == pitcher) %>%
  select(pitcher, pitch_type_auto, inning, batter, pa_pitch_number, pitch_call, rel_speed, 
         vert_rel_angle, horz_rel_angle, spin_rate, spin_axis,
         rel_height, rel_side, extension, vert_break, horz_break, zone_time,
         pfx_x, pfx_z, x0, y0, z0, vx0, vy0, vz0, ax0, ay0, az0,
         plate_loc_x, plate_loc_z) -> PitchData

PitchData %>%
  filter(str_detect(pitcher, "Flaherty, Jack")) -> JackFlaherty

JackFlaherty %>%
  filter(str_detect(batter, "Almonte, Abraham")) -> FlahertySequence

FlahertySequence %>%
  filter(str_detect(inning, "6")) -> FlahertySequence

FlahertySequence %>%
  filter(pa_pitch_number > 3) -> FlahertySequence

FlahertySequence %>%
  filter(str_detect(pitch_type_auto, "Fastball")) -> FlahertySequenceFastball

FlahertySequenceFastball %>%
  select(x0, ax0, vx0, y0, ay0, vy0, z0, az0, vz0, zone_time) -> FlahertyFastballLoc


FlahertySequence %>%
  filter(str_detect(pitch_type_auto, "Slider")) -> FlahertySequenceSlider


FlahertySequenceSlider %>%
  select(x0, ax0, vx0, y0, ay0, vy0, z0, az0, vz0, zone_time) -> FlahertySliderLoc


TH <- theme(plot.title = element_text(colour = "blue", 
                                      size = 18, 
                                      hjust = 0.5, vjust = 0.8, angle = 0))

JackFlaherty %>%
  mutate(tunnel = (rel_height - abs(z0))) -> JackFlaherty

##FlahertySequenceFastball %>%
## mutate(vx0 = abs(vx0)) -> FlahertySequenceFastball


ggplot(JackFlaherty, aes(rel_side, rel_height, color= pitch_type_auto)) + 
  geom_point() +
  xlim(-4, 4) +
  ylim(5, 7) +
  TH + ggtitle("Jack Flaherty Release Points")


ggplot(JackFlaherty, aes(x0, z0, color= pitch_type_auto)) + 
  geom_point() +
  xlim(-4, 4) +
  ylim(5, 7) +
  TH + ggtitle("Jack Flaherty Release Points")


ggplot(JackFlaherty, aes(plate_loc_x, plate_loc_z, color= pitch_type_auto)) + 
  geom_point() +
  xlim(-5, 5) +
  ylim(-6, 6) +
  TH + ggtitle("Jack Flaherty Strike Zone")


ggplot(JackFlaherty, aes(pfx_x, pfx_z, color= pitch_type_auto)) + 
  geom_point() +
  xlim(-11, 11) +
  ylim(-11, 11) +
  TH + ggtitle("Jack Flaherty Pitch Movement")



## D = vit + 1/2 at^2
## (vi + vf)/2 = D/t
## a = (vf - vi)/t
## vf^2 = vi^2 + 2aD


#x = x0 + xv0t + 1/2axt
#y = y0 + yv0t + 1/2ayt 
#z = z0 +zv0t + 1/2azt




x0 = FlahertyFastballLoc$x0
#x0 = -2.33355
ax0 = FlahertyFastballLoc$ax0
#ax0 = -7.43187
vx0 = FlahertyFastballLoc$vx0
#vx0 = 2.933
y0 = FlahertyFastballLoc$y0
#y0 = 50
ay0 = FlahertyFastballLoc$ay0
#ay0 = 31.5545
vy0 = FlahertyFastballLoc$vy0
#vy0 = -140.347
z0 = FlahertyFastballLoc$z0
#z0 = 5.647
az0 = FlahertyFastballLoc$az0
#az0 = -18.6293
vz0 = FlahertyFastballLoc$vz0
#vz0 = -3.323
t = FlahertyFastballLoc$zone_time
#t = 0.3902

##pitchloc <- function(x0, ax0, vx0, y0, ay0, vy0, z0, az0, vz0, t){
  
##  x <- x0 + vx0 * t + 0.5 * ax0 * I(t^2)
## y <- y0 + vy0 * t + 0.5 * ay0 * I(t^2)
## z <- z0 + vz0 * t + 0.5 * az0 * I(t^2)
 ## if(length(t) == 1) {
 ##   loc <- c(x, y, z)
 ## } else{
 ##  loc <- cbind(x, y, z)
##  }
##  return(loc)
##}
##FlahertyResults <- pitchloc(x0, ax0, vx0, y0, ay0, vy0, z0, az0, vz0, t)
##FlahertyResults

pitch_trajectory <- function (x0, ax0, vx0, y0, ay0, vy0, z0, az0, vz0, interval = 0.01){
  cross_p <- (-1 * vy0 - sqrt(I(vy0^2) - 2 * y0 * ay0)) / ay0
  tracking <- t(sapply(seq(0, cross_p, interval), pitchloc,
                       x0 = x0, ax0 = ax0, vx0 = vx0, 
                       y0 = y0, ay0 = ay0, vy0 = vy0, 
                       z0 = z0, az0 = az0, vz0 = vz0 ))
  colnames(tracking) <- c("x", "y", "z")
  tracking <- data.frame(tracking)
  return(tracking)
}

x0 = FlahertyFastballLoc$x0
#x0 = -2.33355
ax0 = FlahertyFastballLoc$ax0
#ax0 = -7.43187
vx0 = FlahertyFastballLoc$vx0
#vx0 = 2.933
y0 = FlahertyFastballLoc$y0
#y0 = 50
ay0 = FlahertyFastballLoc$ay0
#ay0 = 31.5545
vy0 = FlahertyFastballLoc$vy0
#vy0 = -140.347
z0 = FlahertyFastballLoc$z0
#z0 = 5.647
az0 = FlahertyFastballLoc$az0
#az0 = -18.6293
vz0 = FlahertyFastballLoc$vz0
#vz0 = -3.323
t = FlahertyFastballLoc$zone_time
#t = 0.3902
FlahertyFastballResult <- pitch_trajectory(x0, ax0, vx0, y0, ay0, vy0, z0, az0, vz0, interval = 0.01)

Sx0 = FlahertySliderLoc$x0

Sax0 = FlahertySliderLoc$ax0

Svx0 = FlahertySliderLoc$vx0

Sy0 = FlahertySliderLoc$y0

Say0 = FlahertySliderLoc$ay0

Svy0 = FlahertySliderLoc$vy0

Sz0 = FlahertySliderLoc$z0

Saz0 = FlahertySliderLoc$az0

Svz0 = FlahertySliderLoc$vz0

St = FlahertySliderLoc$zone_time




FlahertySliderResult <- pitch_trajectory(Sx0, Sax0, Svx0, Sy0, Say0, Svy0, Sz0, Saz0, Svz0, interval = 0.01)



FlahertySliderResult %>%
  select(x, y) -> FSRXY

FlahertySliderResult %>%
  select(z, y) -> FSRZY

FlahertyFastballResult %>%
  select(x, y) -> FFRXY

newRow <- data.frame(x = NA, y = NA)
FFRXY <- rbind(FFRXY, newRow)
newRow2 <- data.frame(x = NA, y = NA)
FFRXY <- rbind(FFRXY, newRow2)
newRow3 <- data.frame(x = NA, y = NA)
FFRXY <- rbind(FFRXY, newRow3)
newRow4 <- data.frame(x = NA, y = NA)
FFRXY <- rbind(FFRXY, newRow4)

FFRXY <- cbind(FFRXY, sliderx = FSRXY$x)

FlahertyFastballResult %>%
  select(z, y) -> FFRZY

SnewRow <- data.frame(z = NA, y = NA)
FFRZY <- rbind(FFRZY, SnewRow)
SnewRow2 <- data.frame(z = NA, y = NA)
FFRZY <- rbind(FFRZY, SnewRow2)
SnewRow3 <- data.frame(z = NA, y = NA)
FFRZY <- rbind(FFRZY, SnewRow3)
SnewRow4 <- data.frame(z = NA, y = NA)
FFRZY <- rbind(FFRZY, SnewRow4)


FFRZY <- cbind(FFRZY, sliderz = FSRZY$z)

##plot(FFRXY$y, FFRXY$x, type = "b", col = "red", lwd = 2, pch=20, xlab  = "Distance", 
 ##    ylab = "XLocation")
##lines(FSRXY$y, FSRXY$x, type = "b", col = "purple", lwd = 2, pch = 20)


ggplot(FFRXY, aes(y = FFRXY$y)) +
  geom_line(aes(x = FFRXY$x, color= "Fastball")) + xlab("Horizontal Pitch Location") + ylab("Distance to Home") +
  geom_line(aes(x = FFRXY$sliderx, color = "Slider")) + ggtitle("Horizontal View of Pitch Tunneling") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 28.3, 30, 35, 40, 45, 50)) +
  scale_x_continuous(breaks = c(-2.5, -2.25, -2, -1.75, -1.5, -1.25, -1, -.75, -.5, -.25, 0, .25, .5, .75))


ggplot(FFRZY, aes(y = FFRZY$y)) + xlab("Vertical Position of Pitch Location") + ylab("Distance to Home") +
  geom_line(aes(x = FFRZY$z, color = "Fastball")) +
  geom_line(aes(x = FFRZY$sliderz, color = "Slider"))+ ggtitle("Vertical View of Pitch Tunneling") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 28.3, 30, 35, 40, 45, 50)) + scale_x_continuous(breaks = c(0, 0.25, .5, .75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, 6))





