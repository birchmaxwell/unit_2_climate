##### Script for Class Unit 2 #####
##### Activity One, Melting Ice Sheets #####

# Data and Equations

ant_ice_loss <- read.table("data/antarctica_mass_200204_202111.txt", 
                          skip=31, sep="", header = FALSE,
                          col.names = c("decimal_date", "mass_Gt", "sigma_Gt")) 
grn_ice_loss <- read.table("data/greenland_mass_200204_202111.txt", 
                          skip=31, sep="", header = FALSE,
                          col.names = c("decimal_date", "mass_Gt", "sigma_Gt"))
library(tidyverse)
bw <- theme_bw() + theme(panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank())
install.packages("ggpmisc")
library(ggpmisc)
my.formula <- y ~ x
EqStat <- stat_poly_eq(formula = my.formula, aes(label = paste(..eq.label..,
                                                               ..rr.label..,
                                                               ..p.value.label..,
                                                               sep = "~~~~")), 
                       parse = TRUE, label.x = "right", label.y = "top")

# Basic Trends

ggplot(data = ant_ice_loss, mapping = aes(x = decimal_date, y = mass_Gt)) +
  geom_point(aes(x = decimal_date, y = mass_Gt), alpha = 1/3) + 
  geom_smooth(se = T, color = "black") + bw + EqStat +
  labs(title = "Antarctic Ice Loss", x = "Date", y = "Mass(Gt)")
ggplot(data = grn_ice_loss, mapping = aes(x = decimal_date, y = mass_Gt)) +
  geom_point(aes(x = decimal_date, y = mass_Gt), alpha = 1/3) + 
  geom_line(se = T, method = lm, color = "black") + bw + EqStat + #Look at this one for HW
  labs(title = "Greenland Ice Loss", x = "Date", y = "Mass(Gt)")

summary(ant_ice_loss)
summary(grn_ice_loss)

ggplot(show.legend = TRUE) +
  geom_point(data = ant_ice_loss, aes(x = decimal_date, y = mass_Gt), 
             show.legend = TRUE, color = "Brown") + 
  geom_smooth(data = ant_ice_loss, aes(x = decimal_date, y = mass_Gt), 
              method = lm, show.legend = TRUE, color = "brown") + 
  geom_point(data = grn_ice_loss, aes(x = decimal_date, y = mass_Gt),
             show.legend = TRUE, color = "Blue") + 
  geom_smooth(data = grn_ice_loss, aes(x = decimal_date, y = mass_Gt), 
              method = lm, show.legend = TRUE, color = "blue") +
  bw + labs(title = "Ice Loss", x = "Date", y = "Mass(Gt)", 
            caption = "Melting of Ice Sheets. 
            The Red Line is representitive of Antarctica. 
            The Blue Line is representitive of Greenland")
              # Not sure why I can't get a proper legend in this graph 

# Trend with Confidence Intervals

ant_ice_loss$Upper_CI = c(ant_ice_loss$mass_Gt+2*ant_ice_loss$sigma_Gt)
ant_ice_loss$Lower_CI = c(ant_ice_loss$mass_Gt-2*ant_ice_loss$sigma_Gt)
grn_ice_loss$Upper_CI = c(grn_ice_loss$mass_Gt+2*grn_ice_loss$sigma_Gt)
grn_ice_loss$Lower_CI = c(grn_ice_loss$mass_Gt-2*grn_ice_loss$sigma_Gt)
all_ice_loss <- data.table(can) # if we wanted below on one graph we would
                                # created a data table with each column defined
                                # as the parameters above, then graph it all in one.
                                # I was lazy and did not feel like typing all that out.

library(reshape2)
aic <- melt(ant_ice_loss, na.rm = TRUE, id.vars = "decimal_date")
ggplot(data = aic, mapping = aes(x = decimal_date, y = value, color = variable)) +
  geom_point() + 
  geom_smooth(se = F, method = lm) + bw  +
  labs(title = "Antarctic Ice Loss", x = "Date", y = "Mass(Gt)") +
  scale_color_manual(values = c("blue", "brown", "purple", "orange"))
gic <- melt(grn_ice_loss, na.rm = TRUE, id.vars = "decimal_date")
ggplot(data = gic, mapping = aes(x = decimal_date, y = value, color = variable)) +
  geom_point(show.legend = FALSE) + 
  geom_smooth(se = F, method = lm, show.legend = FALSE) + bw  +
  labs(title = "Greenland Ice Loss", x = "Date", y = "Mass(Gt)") +
  scale_color_manual(values = c("blue", "brown", "purple", "orange"))
aicg <- ggplot(data = aic, mapping = aes(x = decimal_date, y = value, color = variable)) +
  geom_point() + 
  geom_line(se = F) + bw  +
  labs(title = "Antarctic Ice Loss", x = "Date", y = "Mass(Gt)") +
  scale_color_manual(values = c("blue", "brown", "purple", "orange"))
gicg <- ggplot(data = gic, mapping = aes(x = decimal_date, y = value, color = variable)) +
  geom_point(show.legend = FALSE) + 
  geom_line(se = F, show.legend = FALSE) + bw  +
  labs(title = "Greenland Ice Loss", x = "Date", y = "Mass(Gt)") +
  scale_color_manual(values = c("blue", "brown", "purple", "orange"))
library(patchwork)
gicg + aicg

# Box Plots of Min Ice Extent and Rate of Ice Loss
min(ant_ice_loss$mass_Gt)
min(grn_ice_loss$mass_Gt)
big_loss <- data.frame("Location" = c("Antarctica", "Greenland"),
                       Loss = c(min(ant_ice_loss$mass_Gt)*-1, min(grn_ice_loss$mass_Gt)*-1))
ggplot(data = big_loss, aes(x = Location, y = Loss, fill = Location)) + 
  geom_bar(stat = "identity") + bw +
  scale_fill_manual(values = c("Brown", "Blue")) + labs(y = "Total Ice Loss (Gt)")

CIG <- (grn_ice_loss[[1 , 2]] - grn_ice_loss[[203 , 2]]) / 
  (grn_ice_loss[[1 , 1]] - grn_ice_loss[[203 , 1]]) * -1
CIA <- (ant_ice_loss[[1 , 2]] - ant_ice_loss[[203 , 2]]) / 
  (ant_ice_loss[[1 , 1]] - ant_ice_loss[[203 , 1]]) * -1
CII <- data.frame("Location" = c("Antarctica", "Greenland"),
                  Ice_Loss_Rate = c(CIA, CIG))
ggplot(data = CII, aes(x = Location, y = Ice_Loss_Rate, fill = Location)) + 
  geom_bar(stat = "identity", show.legend = F) + bw +
  scale_fill_manual(values = c("Brown", "Blue")) + labs(title = "WE ARE LOSING ICE!",
                                                        y = "Ice Loss Rate (Gt/yr)")


##### Activity Two, Boolean Logic #####

# Basic Logic
vec <- c(1,0,2,1,2)
vec[c(T, F, F, T, F)]

x <- 5
x < 4
x == 4
x > 4
x %in% vec
y <- 2
y %in% vec 
y == vec
y != vec

world_oceans = data.frame(ocean = c("Atlantic", "Pacific", "Indian", "Arctic", "Southern"),
                          area_km2 = c(77e6, 156e6, 69e6, 14e6, 20e6),
                          avg_depth_m = c(3926, 4028, 3963, 3953, 4500))

world_oceans$avg_depth_m > 4000
world_oceans[world_oceans$avg_depth_m > 4000, "ocean"]
world_oceans$ocean[world_oceans$avg_depth_m > 4000]
sum(c(1,2,3))
sum(world_oceans$avg_depth_m > 4000)

3 == 1 + 2
0.3 == 0.1 + 0.2 # why this false
0.3 - (0.1+0.2) # everything is in binary... decimals are never exact in
                # most coding languages, so they make it really really really small
error_threshold <- 0.00001 # how to fix this!
abs(0.3 == 0.1 + 0.2) < error_threshold

# More Advanced Functionality 

x > 2
x < 9
2 < x < 9 # cant do both
x > 2 & x < 9 # separate with a "&" but remember to include BOTH STUFF
x > 2 & x < 4
x > 2 | x < 4 # "|" is the "or"

world_oceans[world_oceans$avg_depth_m < 4000 & world_oceans$area_km2 < 50e6, ]

vec2 <- c(1,2,3,NA)
vec2 == NA # R gets confused with NA's
is.na(vec2)
any(is.na(vec2))
all(is.na(vec2))


##### Activity Three, COnditional Logic #####

# IF, THEN statements... essentially

num <- -2
if(num < 0){num = num*-1}
num
if(num < 1){num = !(num*50)} # ! for not does not work in here!
num 

if(num < 0){print(" 2 is a negative??? ")
  num = num*-1
  print(" now is it positive ")
} else{print("lol u fucked up")}
