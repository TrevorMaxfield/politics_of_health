require(gdata)
require(gplots)
library(maps)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(urbnmapr)
setwd("/Users/trevor/Desktop/DA Local/Project_Data")


########################################################################
############################### NEURAL #################################
########################################################################

nn_16 = read.csv("predict_19_hel.csv")

# Don't use on 2019 
nn_16 = nn_16[,-1]
colnames(nn_16) = c("predicted", "actual")
nn_16$FIPS = sprintf("%05d", e_16$FIPS)

# For 2019 Predictions
colnames(nn_16) = c("step","predicted" )
f_19 = merge(labels_16[,c("FIPS","state")],chr_19_full[,c("FIPS","State")], by="FIPS")
nn_16$FIPS = sprintf("%05d", f_19$FIPS)


# MAP
colors = brewer.pal(n = 11, name = 'RdBu')
dem_plot = left_join(nn_16[,c("FIPS","predicted")], counties, by = c("FIPS" = "county_fips")) 

ggplot(dem_plot, aes(long, lat, group = dem_plot$group, fill = predicted)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent, limits=c(0,1),
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Percentage Dem") +
  ggtitle("2019 Predicted Election Results (Health Factors), Vote Share per County")


# Predict electoral split
nn_16$FIPS = e_16$FIPS

# 2019
nn_16$FIPS = f_19$FIPS

nn_16 = left_join(nn_16[,c("FIPS","predicted")],labels_12[,c("FIPS","state","total")], by="FIPS")
nn_16$votes = round(nn_16$predicted*nn_16$total)
dem_states = aggregate(nn_16$votes, by=list(Category=nn_16$state), FUN=sum)
tot_states = aggregate(nn_16$total, by=list(Category=nn_16$state), FUN=sum)
dem_states$per = dem_states$x/tot_states$x
dem_states$rounded = round(dem_states$per)
dem_states$electoral = c(9,11,6,55,9,7,3,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)
dem_states$results = dem_states$rounded*dem_states$electoral

dem_total_electoral = sum(dem_states$results)
rep_total_electoral = sum(dem_states$electoral) - dem_total_electoral

# 2016 Actual
act_states = aggregate(labels_16$democrat, by=list(Category=labels_16$state), FUN=sum)
act_tot_states = aggregate(labels_16$total, by=list(Category=labels_16$state), FUN=sum)
dem_states$actual = act_states[-2,"x"]/act_tot_states[-2,"x"]

#print(xtable(a, digits=5), include.rownames=FALSE)
