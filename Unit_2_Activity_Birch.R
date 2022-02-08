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


##### Activity Three, Conditional Logic #####

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


a <- 70
b <- 85
if(a > b){
  print("a won!")
} else if(a < b){
  print("b won")
} else if(a == b){
  print("twas a tie")
}


# ifelse()
d <- 4
final <- ifelse(d != 0, 1/d, NA)
final

e <- c(1,2,3,4,-5)
final2 <- as.data.frame(ifelse(e != 0, 3/e, NA))
final2

##### Activity Four, Mauna Load and CO2 #####

url <- 'ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt'
co2 <- read.table(url, col.names = c("year", "month", "decimal_date", 
                                    "monthly_average", "deseasonalized", 
                                    "n_days", "st_dev_days", "monthly_mean_uncertainty"))

library(tidyverse)
install.packages("ggpmisc")
library(ggpmisc)
my.formula <- y ~ x
EqStat <- stat_poly_eq(formula = my.formula, aes(label = paste(..eq.label..,
                                                               ..rr.label..,
                                                               ..p.value.label..,
                                                               sep = "~~~~")), 
                       parse = TRUE, label.x = "left", label.y = "top")
bw <- theme_bw() + theme(panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank())

ggplot(data = co2, mapping = aes(x = decimal_date, y = monthly_average)) +
  geom_point(aes(x = decimal_date, y = monthly_average), alpha = 1/3) + 
  geom_smooth(se = T, color = "red") + bw + EqStat +
  labs(title = "CO2 Increase", x = "Date", y = "CO2 (ppm)")

co2$seasonal_cycle <- co2$monthly_average - co2$deseasonalized
ggplot(data = co2, mapping = aes(x = decimal_date, y = seasonal_cycle)) +
  geom_line(aes(x = decimal_date, y = seasonal_cycle)) + 
  bw + labs(title = "Seasonal Trends", x = "Date", y = "CO2 Difference (ppm)") +
  xlim(2015, 2021)

a1 <- mean(co2$seasonal_cycle[co2$month == 1])
a2 <- mean(co2$seasonal_cycle[co2$month == 2])
a3 <- mean(co2$seasonal_cycle[co2$month == 3])
a4 <- mean(co2$seasonal_cycle[co2$month == 4])
a5 <- mean(co2$seasonal_cycle[co2$month == 5])
a6 <- mean(co2$seasonal_cycle[co2$month == 6])    # Note how fucking annoying this is
a7 <- mean(co2$seasonal_cycle[co2$month == 7])
a8 <- mean(co2$seasonal_cycle[co2$month == 8])
a9 <- mean(co2$seasonal_cycle[co2$month == 9])
a10 <- mean(co2$seasonal_cycle[co2$month == 10])
a11 <- mean(co2$seasonal_cycle[co2$month == 11])
a12 <- mean(co2$seasonal_cycle[co2$month == 12])
co2_mothly_cycle <- data.frame(month = c(1:12), detrended = c(a1,a2,a3,a4,
                                                              a5,a6,a7,a8,
                                                              a9,a10,a11,a12))
ggplot(data = co2_mothly_cycle, mapping = aes(x = month, y = detrended)) +
  geom_line(aes(x = month, y = detrended), alpha = 1) + 
  bw + labs(title = "Seasonal Cycle", x = "Month", y = "CO2 (ppm)")
    # NOW WE LEARN LOOPS SO WE DO NOT HAVE TO DO THAT CRAP ABOVE

##### Activity Five FOR LOOPS #####
for(i in c(1:4)){
  print("one loop")
}

CMC_with_loop <- data.frame(month = c(1:12), detrended = for(i in (co2$seasonal_cycle[co2$month == i])){
  mean(co2$seasonal_cycle[co2$month == i])
}) # i tried IDK

words = c("my", "second", "for", "loop")
for(word in words){print(word)}                               

# indexing with a loop
my_vec <- c(1,3,4,5,3,1)
n <- length(my_vec)
my_vec_sq <- rep(NA, n)
for(i in seq(n)){
  my_vec_sq[1] = my_vec[i]^2
}
my_vec_sq

# practice
thing <- c(1:100)
num <- length(thing)
for(i in seq(thing)){
  num[1] = factorial(thing[i])
}

# for loop
vec3 <- c(1,3,5,7)
total <- 0
for(i in seq(length(vec3))){
  total = total + vec3[i]
  print(total)
}
# nested for loop, good for matrix !!!!!!!!!!!! #####
mat <- matrix(c(2,5,6,3,2,4), nrow = 2, byrow = TRUE)
mat_squared <- matrix(c(NA), nrow =2, ncol =3)
for(i in c(1:2)){
  for(j in c(1:3)){
    mat_squared[i, j] = mat[i, j]^2      # can run this to look at the matrix "mat"
  }
  print(mat_squared)
}

for(i in seq(dim(mat)[1])){
  for(j in seq(dim(mat)[2])){
    mat_squared[i, j] = mat[i, j]^2      # can run this to look any matrix, use this for Tads HW
  }
  print(mat_squared)
}


# while loop, repeating a code (like a nested loop, but) based on a condition #####
  # condition MUST at some point be false or else u get infinite loop!

x <- 1
while( x > 0){
  x <- x + 1
}            # infinite loop

x # this is what the number became before we hit the STOP

y <- -2
while( x < 20){
  y <- y + 6
}
y # amount of loops that ran before you reached your condition

# fishing game
rnorm(1, mean = 2, sd = 1)
total_catch <- 0
catch_limit <- 50
n_fish <- 0
while(total_catch < catch_limit){
  n_fish <- n_fish + 1
  fish_weight <- rnorm(1, mean = 2, sd =1)
  total_catch <- total_catch + fish_weight
  print(n_fish)
}

##### Activity Six, Arctic Sea Ice #####
url = 'ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/N_seaice_extent_daily_v3.0.csv'
arctic_ice = read.delim(url, skip=2, sep=",", header=FALSE, col.names = 
                          c("Year", "Month", "Day", "Extent", "Missing", "Source_Data"))
head(arctic_ice)
tail(arctic_ice)
library(lubridate)

arctic_ice$date <- make_date(year = arctic_ice$Year, month = arctic_ice$Month, 
                             day = arctic_ice$Day)
head(arctic_ice)

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

ggplot(data = arctic_ice, mapping = aes(x = date, y = Extent)) +
  geom_point(aes(x = date, y = Extent), alpha = 1/3) + 
  geom_line(color = "black") + bw + EqStat +
  labs(title = "Ice Loss", x = "Date", y = "Mass(Gt)")

arctic_ice_coverage <- data.frame(year <- seq(min(arctic_ice$Year) + 1, 
                                              max(arctic_ice$Year) - 1),
                                  extent_annual_avg <- NA,
                                  extent_5yr_avg <- NA
                                              )

#check to see if what you want works
head(arctic_ice_coverage)
mean(arctic_ice$Extent[arctic_ice$Year == arctic_ice_coverage$year[1]])
# then throw that bitch in there... this one is for single extent annual average
for(i in seq(dim(arctic_ice_coverage)[1])){
  arctic_ice_coverage$extent_annual_avg....NA[i] <- 
    mean(arctic_ice$Extent[arctic_ice$Year == arctic_ice_coverage$year[i]])
  print(arctic_ice_coverage)
}
# now for 5 yr average
for(i in seq(dim(arctic_ice_coverage)[1])){
  y <- seq(arctic_ice_coverage$year....seq.min.arctic_ice.Year....1..max.arctic_ice.Year....[i]- 2,
           arctic_ice_coverage$year....seq.min.arctic_ice.Year....1..max.arctic_ice.Year....[i] + 2)
  arctic_ice_coverage$extent_5yr_avg....NA <- mean(arctic_ice$Extent[arctic_ice$Year %in% y ==
                                                                       arctic_ice_coverage$year[i]])
  print(arctic_ice_coverage)
}
# im not doing the 5 year average. This syntax sucks and she should have define variables above
# so code is not messy.
##### Activity Seven, Functions #####