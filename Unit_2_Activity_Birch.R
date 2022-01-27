##### Script for Class Unit 2 #####
##### Activity One, Melting Ice Sheets #####

ant_ice_loss <- read.table("data/antarctica_mass_200204_202111.txt", 
                          skip=31, sep="", header = FALSE,
                          col.names = c("decimal_date", "mass_Gt", "sigma_Gt")) 
print(ant_ice_loss)
grn_ice_loss <- read.table("data/greenland_mass_200204_202111.txt", 
                          skip=31, sep="", header = FALSE,
                          col.names = c("decimal_date", "mass_Gt", "sigma_Gt"))
print(grn_ice_loss)

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

ggplot(data = ant_ice_loss, mapping = aes(x = decimal_date, y = mass_Gt)) +
  geom_point(aes(x = decimal_date, y = mass_Gt), alpha = 1/3) + 
  geom_smooth(se = T, method = lm, color = "black") + bw + EqStat +
  labs(title = "Antarctic Ice Loss", x = "Date", y = "Mass(Gt)")
ggplot(data = grn_ice_loss, mapping = aes(x = decimal_date, y = mass_Gt)) +
  geom_point(aes(x = decimal_date, y = mass_Gt), alpha = 1/3) + 
  geom_smooth(se = T, method = lm, color = "black") + bw + EqStat +
  labs(title = "Greenland Ice Loss", x = "Date", y = "Mass(Gt)")

summary(ant_ice_loss)
summary(grn_ice_loss)



