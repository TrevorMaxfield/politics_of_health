require(gdata)
require(gplots)
setwd("/Users/trevor/Desktop/DA Local/Project_Data")

load("project_loaded.RData")


########################################################################
############################### HEATMAP ################################
########################################################################


e_12 = merge(chr_12_full, labels_12[,c("FIPS", "dem_per", "rep_per")], by="FIPS")
e_16 = merge(chr_16_full, labels_16[,c("FIPS", "dem_per", "rep_per")], by="FIPS")
e_d = rbind(e_12,e_16)

library(RColorBrewer)
col = colorRampPalette(brewer.pal(n = 9, name = 'YlOrRd'))(50)
col = colorRampPalette(brewer.pal(n = 9, name = 'YlGnBu'))(50)


##################### HEALTH ######################
### 12 and 16 Results
cor_both = cor(x = e_d[,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,35,36)], y = e_d[,c(40,41)])
cor_both = cor_both[order(cor_both[,1]),]
heatmap(cor_both,Rowv=NA, Colv= NA,scale="column", col = col, mar=c(5,10), main="2012 and 2016 Health vs Vote Share", cexCol=1, cexRow = 1.7)

### 12 Only
cor_12 = cor(x = e_12[,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,35,36)], y = e_12[,c(40,41)])
cor_12 = cor_12[order(cor_12[,1]),]
heatmap(cor_12,Rowv=NA, Colv= NA,scale="column", col = col, mar=c(5,10), main="2012 Health vs Vote Share", cexCol=1, cexRow = 1.7) 

### 16 Only
cor_16 = cor(x = e_16[,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,35,36)], y = e_16[,c(40,41)])
cor_16 = cor_16[order(cor_16[,1]),]
heatmap(cor_16,Rowv=NA, Colv= NA,scale="column", col = col, mar=c(5,10), main="2016 Health vs Vote Share", cexCol=1, cexRow = 1.7)

#hm <- heatmap.2(cor_both, scale="column", Rowv=NA, Colv=NA,
#               col = col, ## using your colors
#                dendrogram = "none",  ## to suppress warnings
#                margins=c(5,0), cexRow=1.0, cexCol=1.0, key=TRUE, keysize=1.5,
#                trace="none")
#legend("left", fill = myCol)

##################### Demographics ######################
### 12 and 16 Results
cor_both = cor(x = e_d[,c(19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,37,38,39)], y = e_d[,c(40,41)])
cor_both = cor_both[order(cor_both[,1]),]
heatmap(cor_both,Rowv=NA, Colv= NA,scale="column", col = col, mar=c(5,0), main="2012 and 2016 Demographics vs Vote Share", cexCol=1)

### 12 Only
cor_12 = cor(x = e_12[,c(19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,37,38,39)], y = e_12[,c(40,41)])
cor_12 = cor_12[order(cor_12[,1]),]
heatmap(cor_12,Rowv=NA, Colv= NA,scale="column", col = col, mar=c(5,10), main="2012 Demographics vs Vote Share", cexCol=1, cexRow =1.7) 

### 16 Only
cor_16 = cor(x = e_16[,c(19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,37,38,39)], y = e_16[,c(40,41)])
cor_16 = cor_16[order(cor_16[,1]),]
heatmap(cor_16,Rowv=NA, Colv= NA,scale="column", col = col, mar=c(5,10), main="2016 Demographics vs Vote Share", cexCol=1, cexRow =1.7) 

#hm <- heatmap.2(cor_both, scale="column", Rowv=NA, Colv=NA,
#               col = col, ## using your colors
#                dendrogram = "none",  ## to suppress warnings
#                margins=c(5,0), cexRow=1.0, cexCol=1.0, key=TRUE, keysize=1.5,
#                trace="none")
#legend("left", fill = myCol)


########################################################################
############################### US MAPS ################################
########################################################################
library(maps)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(urbnmapr)

######## ELECTION RESULTS ########

dem_16 = labels_16
dem_16$FIPS = sprintf("%05d", labels_16$FIPS)
colors = brewer.pal(n = 11, name = 'RdBu')
dem_plot = left_join(dem_16[,c("FIPS","dem_per")], counties, by = c("FIPS" = "county_fips")) 

ggplot(dem_plot, aes(long, lat, group = group, fill = dem_per)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent, limits=c(0,1),
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Percentage Dem") +
  ggtitle("2016 Election Results, Vote Share per County")


######## Fair/Poor Health ########

fair_16 = chr_16_full[,c("FIPS","% Fair/Poor")]
fair_16$"% Fair/Poor"=fair_16$"% Fair/Poor"/100
colnames(fair_16) = c("FIPS","fp")
fair_16$FIPS = sprintf("%05d", fair_16$FIPS)

colors = brewer.pal(n = 9, name = 'Reds')
fair_plot = left_join(fair_16, counties, by = c("FIPS" = "county_fips")) 

ggplot(fair_plot, aes(long, lat, group = group, fill = fp)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "% Fair/Poor Health") +
  ggtitle("2016 Percentage Fair/Poor Health per County")



######## Free/Reduced Lunch ########

d_16 = chr_16_full[,c("FIPS","% Free or Reduced Lunch")]
colnames(d_16) = c("FIPS","d")
d_16$"d"=d_16$"d"/100
d_16$FIPS = sprintf("%05d", d_16$FIPS)

colors = brewer.pal(n = 9, name = 'Blues')
d_plot = left_join(d_16, counties, by = c("FIPS" = "county_fips")) 

ggplot(d_plot, aes(long, lat, group = group, fill = d)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "% Free or Reduced Lunch") +
  ggtitle("2016 % Free or Reduced Lunch per County")


######## 2012 Election ########

dem_12 = labels_12
dem_12$FIPS = sprintf("%05d", labels_12$FIPS)
colors = brewer.pal(n = 11, name = 'RdBu')
dem_plot = left_join(dem_12[,c("FIPS","dem_per")], counties, by = c("FIPS" = "county_fips")) 

ggplot(dem_plot, aes(long, lat, group = group, fill = dem_per)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent, limits=c(0,1),
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Percentage Dem") +
  ggtitle("2012 Election Results, Vote Share per County")


######## 2016 HIV ########

d_12 = chr_16_full[,c("FIPS","HIV Prevalence Rate")]
colnames(d_12) = c("FIPS","d")
d_12$FIPS = sprintf("%05d", d_12$FIPS)
d_12 = left_join(d_12, counties, by = c("FIPS" = "county_fips")) 

# Create quantiles
no_classes <- 6
labels <- c()
quantiles <- quantile(d_12$d,
                      probs = seq(0, 1, length.out = no_classes + 1))
# Custom labels, rounding
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2),
                             " – ",
                             round(quantiles[idx + 1], 2)))
}
# Minus one label to remove the odd ending one
labels <- labels[1:length(labels)-1]

# Create new variable for fill
d_12$quantiles <- cut(d_12$d, breaks = quantiles,
                                labels = labels,
                                include.lowest = T)


colors = brewer.pal(n = 6, name = 'Greens')

ggplot(d_12, aes(long, lat, group = group, fill = quantiles), colors=colors) +
  geom_polygon(color = NA) +
  scale_fill_discrete(labels = labels) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "HIV Prevalence Rate") +
  ggtitle("2016 HIV Prevalence Rate by County")


######## 2012 % Uninsured ########

d_16 = chr_12_full[,c("FIPS","% Uninsured")]
colnames(d_16) = c("FIPS","d")
d_16$"d"=d_16$"d"/100
d_16$FIPS = sprintf("%05d", d_16$FIPS)

colors = brewer.pal(n = 9, name = 'Blues')
d_plot = left_join(d_16, counties, by = c("FIPS" = "county_fips")) 

ggplot(d_plot, aes(long, lat, group = group, fill = d)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "% Uninsured") +
  ggtitle("2012 % Uninsured per County ")


######## 2012 % Single Parent ########

d_16 = chr_12_full[,c("FIPS","% Single-Parent Households")]
colnames(d_16) = c("FIPS","d")
d_16$"d"=d_16$"d"/100
d_16$FIPS = sprintf("%05d", d_16$FIPS)

colors = brewer.pal(n = 9, name = 'Greens')
d_plot = left_join(d_16, counties, by = c("FIPS" = "county_fips")) 

ggplot(d_plot, aes(long, lat, group = group, fill = d)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "% Single Parent") +
  ggtitle("2012 % Single Parent Households per County ")


######## 2012/2016 % Rural ########

d_16 = chr_16_full[,c("FIPS","% Rural")]
colnames(d_16) = c("FIPS","d")
d_16$"d"=d_16$"d"/100
d_16$FIPS = sprintf("%05d", d_16$FIPS)

colors = brewer.pal(n = 9, name = 'YlOrBr')
d_plot = left_join(d_16, counties, by = c("FIPS" = "county_fips")) 

ggplot(d_plot, aes(long, lat, group = group, fill = d)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "% Rural") +
  ggtitle("2016 % Rural Population per County")


######## 2016 % African American ########

d_16 = chr_16_full[,c("FIPS","% African American")]
colnames(d_16) = c("FIPS","d")
d_16$"d"=d_16$"d"/100
d_16$FIPS = sprintf("%05d", d_16$FIPS)

colors = brewer.pal(n = 9, name = 'Greens')
d_plot = left_join(d_16, counties, by = c("FIPS" = "county_fips")) 

ggplot(d_plot, aes(long, lat, group = group, fill = d)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "% African American") +
  ggtitle("2016 % African American per County ")


######## 2016 % Physically Inactive ########

d_16 = chr_16_full[,c("FIPS","Physical Inactivity")]
colnames(d_16) = c("FIPS","d")
d_16$"d"=d_16$"d"/100
d_16$FIPS = sprintf("%05d", d_16$FIPS)

colors = brewer.pal(n = 9, name = 'Blues')
d_plot = left_join(d_16, counties, by = c("FIPS" = "county_fips")) 

ggplot(d_plot, aes(long, lat, group = group, fill = d)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(colors=colors, labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "% Physically Inactive") +
  ggtitle("2016 % Physically Inactive Adults per County ")


######## 2012 HIV ########

d_12 = chr_12_full[,c("FIPS","HIV Prevalence Rate")]
colnames(d_12) = c("FIPS","d")
d_12$FIPS = sprintf("%05d", d_12$FIPS)
d_12 = left_join(d_12, counties, by = c("FIPS" = "county_fips")) 

# Create quantiles
no_classes <- 6
labels <- c()
quantiles <- quantile(d_12$d,
                      probs = seq(0, 1, length.out = no_classes + 1))
# Custom labels, rounding
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2),
                             " – ",
                             round(quantiles[idx + 1], 2)))
}
# Minus one label to remove the odd ending one
labels <- labels[1:length(labels)-1]

# Create new variable for fill
d_12$quantiles <- cut(d_12$d, breaks = quantiles,
                      labels = labels,
                      include.lowest = T)


colors = brewer.pal(n = 6, name = 'Greens')

ggplot(d_12, aes(long, lat, group = group, fill = quantiles), colors=colors) +
  geom_polygon(color = NA) +
  scale_fill_discrete(labels = labels) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "HIV Prevalence Rate") +
  ggtitle("2012 HIV Prevalence Rate by County")


