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

ant_ice_loss$Upper_CI = c(ant_ice_loss$mass_Gt+2*ant_ice_loss$sigma_Gt)
ant_ice_loss$Lower_CI = c(ant_ice_loss$mass_Gt-2*ant_ice_loss$sigma_Gt)
grn_ice_loss$Upper_CI = c(grn_ice_loss$mass_Gt+2*grn_ice_loss$sigma_Gt)
grn_ice_loss$Lower_CI = c(grn_ice_loss$mass_Gt-2*grn_ice_loss$sigma_Gt)

library(reshape2)
aic <- melt(ant_ice_loss, na.rm = TRUE, id.vars = "decimal_date")
print(aic)
aicg <- ggplot(data = aic, mapping = aes(x = decimal_date, y = value, color = variable)) +
  geom_point() + 
  geom_smooth(se = F, method = lm) + bw  +
  labs(title = "Antarctic Ice Loss", x = "Date", y = "Mass(Gt)") +
  scale_color_manual(values = c("blue", "brown", "purple", "orange"))
gic <- melt(grn_ice_loss, na.rm = TRUE, id.vars = "decimal_date")
print(gic)
gicg <- ggplot(data = gic, mapping = aes(x = decimal_date, y = value, color = variable)) +
  geom_point(show.legend = FALSE) + 
  geom_smooth(se = F, method = lm, show.legend = FALSE) + bw  +
  labs(title = "Greenland Ice Loss", x = "Date", y = "Mass(Gt)") +
  scale_color_manual(values = c("blue", "brown", "purple", "orange"))
gicg + aicg

min(ant_ice_loss$mass_Gt)
min(grn_ice_loss$mass_Gt)

big_loss <- data.frame("Location" = c("Antarctica", "Greenland"),
                       Loss = c(min(ant_ice_loss$mass_Gt)*-1, min(grn_ice_loss$mass_Gt)*-1))

ggplot(data = big_loss, aes(x = Location, y = Loss, fill = Location)) + 
  geom_bar(stat = "identity") + bw +
  scale_color_manual(values = c("Brown", "Blue"))

        
                       