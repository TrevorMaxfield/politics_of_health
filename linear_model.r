require(gdata)
require(gplots)
library(maps)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(urbnmapr)
setwd("/Users/trevor/Desktop/DA Local/Project_Data")

load("project_loaded.RData")

########################################################################
############################### LINEAR #################################
########################################################################
names =  c('FIPS', 'State', 'County', 'YPLL', '% Fair/Poor', 'Physically Unhealthy Days', 
           'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', 'Physical Inactivity', 
           '% Excessive Drinking', 'MV Mortality Rate', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 
           'Preventable Hosp. Rate', '% Screened Mammogram', 'Graduation Rate', '% Some College', 
           '% Unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate', 
           '% < 18', '% 65 and over', '% African American', '% American Indian/Alaskan Native', 
           '% Asian', '% Native Hawaiian/Other Pacific Islander', '% Hispanic', '% Not Proficient in English', 
           '% Female', '% Rural', '% Diabetic', 'HIV Prevalence Rate', 'Household Income', '% Free or Reduced Lunch', 
           '% Drive Alone','dem_per','rep_per')

colnames(e_12) = names
colnames(e_16) = names
a_12 = e_12[,c(-1,-2,-3,-41)]
a_16 = e_16[,c(-1,-2,-3,-41)]

H = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,32,33,37)
D = c(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,34,35,36,37)

h_12 = a_12[,H]
h_16 = a_16[,H]

d_12 = a_12[,D]
d_16 = a_16[,D]

a_mod = lm(dem_per~., data=a_12)
summary(a_mod)
mae_a = sum(abs(a_12$dem_per - predict(a_mod, a_12)))/length(a_12$dem_per)
mae_at = sum(abs(a_16$dem_per - predict(a_mod, a_16)))/length(a_16$dem_per)

h_mod = lm(dem_per~., data=h_12)
summary(h_mod)
mae_h =  sum(abs(h_12$dem_per - predict(h_mod, h_12)))/length(h_12$dem_per)
mae_ht = sum(abs(h_16$dem_per - predict(h_mod, h_16)))/length(h_16$dem_per)

d_mod = lm(dem_per~., data=d_12)
summary(d_mod)
mae_d = sum(abs(d_12$dem_per - predict(d_mod, d_12)))/length(d_12$dem_per)
mae_dt = sum(abs(d_16$dem_per - predict(d_mod, d_16)))/length(d_16$dem_per)



################ Election Map 2016 predicted ################
dem_16 = merge(labels_16, e_16[,c(1,2)], by="FIPS")
dem_16$FIPS = sprintf("%05d", dem_16$FIPS)
dem_16$dem_per = predict(a_mod, e_16)
dem_16$dem_per = predict(d_mod, e_16)
dem_16$dem_per = predict(h_mod, e_16)

dem_16$dem_per[dem_16$dem_per >= 1 ] = 1

colors = brewer.pal(n = 11, name = 'RdBu')
dem_plot = left_join(dem_16[,c("FIPS","dem_per")], counties, by = c("FIPS" = "county_fips")) 

ggplot(dem_plot, aes(long, lat, group = dem_plot$group, fill = dem_per)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent, limits=c(0,1),
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Percentage Dem") +
  ggtitle("2016 Predicted Election Results (Health Factors), Vote Share per County")


# Predict electoral split
dem_16$FIPS = e_16$FIPS
dem_16 = left_join(dem_16[,c("FIPS","State","dem_per")],labels_12[,c("FIPS","total")], by="FIPS")
dem_16$votes = round(dem_16$dem_per*dem_16$total)
dem_states = aggregate(dem_16$votes, by=list(Category=dem_16$State), FUN=sum)
tot_states = aggregate(dem_16$total, by=list(Category=dem_16$State), FUN=sum)
dem_states$per = dem_states$x/tot_states$x
dem_states$rounded = round(dem_states$per)
dem_states$electoral = c(9,11,6,55,9,7,3,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)
dem_states$results = dem_states$rounded*dem_states$electoral

dem_total_electoral = sum(dem_states$results)-dem_states[21,6]-dem_states[29,6] + round(dem_states[21,3]*dem_states[21,4]) + round(dem_states[29,3]*dem_states[29,4])
rep_total_electoral = sum(dem_states$electoral) - dem_total_electoral


# 2016 actual 
act_states = aggregate(labels_16$democrat, by=list(Category=labels_16$state), FUN=sum)
act_tot_states = aggregate(labels_16$total, by=list(Category=labels_16$state), FUN=sum)
dem_states$actual = act_states[-2,"x"]/act_tot_states[-2,"x"]


#####################################################
################### 2019 Election ###################
#####################################################

a_pre = rbind(a_12, a_16)
a_pre = rbind(d_12, d_16)
a_pre = rbind(h_12, h_16)

pre_mod = lm(dem_per~., data=a_pre)
summary(pre_mod)
mae_pre = sum(abs(a_pre$dem_per - predict(pre_mod, a_pre)))/length(a_pre$dem_per)

dem_19 = merge(chr_19_full, e_16[,c(1,2)], by="FIPS") # Reduce to fips codes we have election data for
dem_19$FIPS = sprintf("%05d", dem_19$FIPS)
dem_19$dem_per = predict(pre_mod, dem_19)

dem_19$dem_per[dem_19$dem_per >= 1 ] = 1

colors = brewer.pal(n = 11, name = 'RdBu')
dem_plot = left_join(dem_19[,c("FIPS","dem_per")], counties, by = c("FIPS" = "county_fips")) 

ggplot(dem_plot, aes(long, lat, group = dem_plot$group, fill = dem_per)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent, limits=c(0,1),
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Percentage Dem") +
  ggtitle("2019 Predicted Election Results (Demographic Factors), Vote Share per County")


# Predict electoral split
dem_19 = merge(chr_19_full, e_16[,c(1,2)], by="FIPS") # Reduce to fips codes we have election data for
dem_19$dem_per = predict(pre_mod, dem_19)
dem_19$dem_per[dem_19$dem_per >= 1 ] = 1

dem_19 = left_join(dem_19[,c("FIPS","State.x","dem_per")],labels_16[,c("FIPS","total")], by="FIPS")
dem_19$votes = round(dem_19$dem_per*dem_19$total)
dem_states = aggregate(dem_19$votes, by=list(Category=dem_19$State), FUN=sum)
tot_states = aggregate(dem_19$total, by=list(Category=dem_19$State), FUN=sum)
dem_states$per = dem_states$x/tot_states$x
dem_states$rounded = round(dem_states$per)
dem_states$electoral = c(9,11,6,55,9,7,3,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)
dem_states$results = dem_states$rounded*dem_states$electoral

dem_total_electoral = sum(dem_states$results)
rep_total_electoral = sum(dem_states$electoral) - dem_total_electoral

#print(xtable(a, digits=5), include.rownames=FALSE)

#####################################################
################### 2019 States ###################
#####################################################
colors = brewer.pal(n = 11, name = 'RdBu')
dem_plot = left_join(dem_states[,c("Category","rounded")], counties, by = c("Category" = "state_name")) 

ggplot(dem_plot, aes(long, lat, group = dem_plot$group, fill =rounded)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors[c(3,9)], limits=c(0,1),
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Electoral Winner") +
  ggtitle("2019 Predicted Election Results (All Factors)")



