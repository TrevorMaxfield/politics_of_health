require(gdata)
setwd("/Users/trevor/Desktop/DA Local/Project_Data")

########################################################################
############################### Political ##############################
########################################################################
# State Level 1976-2016
pres = read.table("Political/1976-2016-president.tab", header=TRUE, sep="\t")
hous = read.table("Political/1976-2018-house.tab", header=TRUE, sep="\t")
sena = read.table("Political/1976-2018-senate.tab", header=TRUE, sep="\t")
# County (FIPS) level 2000, 2004, 2008, 2012, 2016 Presidential
c_pres = read.table("Political/countypres_2000-2016.tab", header=TRUE, sep="\t")


s_pres = pres[which(pres$party == "democrat"), c("year", "state", "candidatevotes")]
colnames(s_pres)[3] = "democrat"
s_pres = merge(s_pres, pres[which(pres$party == "republican"), c("year", "state", "candidatevotes")], by=c("year", "state"))
colnames(s_pres)[4] = "republican"
s_pres = merge(s_pres, pres[which(pres$party == "republican"), c("year", "state", "totalvotes")], by=c("year", "state"))


########################################################################
########################### Political Labels ###########################
########################################################################
# 2004
d_2004 = c_pres[which(c_pres$year == 2004 & c_pres$party == "democrat" & !is.na(c_pres$candidatevotes)),9]
r_2004 = c_pres[which(c_pres$year == 2004 & c_pres$party == "republican" & !is.na(c_pres$candidatevotes)),9]

sum(d_2004) / (sum(d_2004) + sum(r_2004)) # 0.4874498
sum(r_2004) / (sum(d_2004) + sum(r_2004)) # 0.5125502

# 2008
d_2008 = c_pres[which(c_pres$year == 2008 & c_pres$party == "democrat" & !is.na(c_pres$candidatevotes)),9]
r_2008 = c_pres[which(c_pres$year == 2008 & c_pres$party == "republican" & !is.na(c_pres$candidatevotes)),9]

sum(d_2008) / (sum(d_2008) + sum(r_2008)) # 0.5367901
sum(r_2008) / (sum(d_2008) + sum(r_2008)) # 0.4632099

# 2012
labels_12 = c_pres[which(c_pres$year == 2012 & c_pres$party == "democrat" & !is.na(c_pres$FIPS)),c("FIPS", "state", "county", "candidatevotes" )]
colnames(labels_12)[4] = "democrat"
labels_12$republican = c_pres[which(c_pres$year == 2012 & c_pres$party == "republican" & !is.na(c_pres$FIPS)),"candidatevotes"]
labels_12$total = labels_12$democrat + labels_12$republican
labels_12$dem_per = labels_12$democrat/labels_12$total
labels_12$rep_per = labels_12$republican/labels_12$total


sum(labels_12$democrat)/sum(labels_12$total)   # 0.520106
sum(labels_12$republican)/sum(labels_12$total) # 0.479894

labels_16 = c_pres[which(c_pres$year == 2016 & c_pres$party == "democrat" & !is.na(c_pres$FIPS) & !is.na(c_pres$candidatevotes)),c("FIPS", "state", "county", "candidatevotes" )]
colnames(labels_16)[4] = "democrat"
labels_16$republican = c_pres[which(c_pres$year == 2016 & c_pres$party == "republican" & !is.na(c_pres$FIPS) & !is.na(c_pres$candidatevotes)),"candidatevotes"]
labels_16$total = labels_16$democrat + labels_16$republican
labels_16$dem_per = labels_16$democrat/labels_16$total
labels_16$rep_per = labels_16$republican/labels_16$total

sum(labels_12$democrat)/sum(labels_12$total)   # 0.5111087
sum(labels_12$republican)/sum(labels_12$total) # 0.4888913

########################################################################
############################# DEMOGRAPHICS #############################
########################################################################
# Education
edu = read.csv("Socio\ Economic\ Factors/Education.csv")
# Population
pop = read.csv("Socio\ Economic\ Factors/pop1970-2010.csv")
pop_est = read.csv("Socio\ Economic\ Factors/PopulationEstimates.csv")
# Poverty
pov = read.csv("Socio\ Economic\ Factors/PovertyEstimates.csv")
pov_hist = read.xls("Socio\ Economic\ Factors/Poverty-Rates-by-County-1960-2010.xlsm", sheet=2)
colnames(pov_hist) = lapply(pov_hist[1,], as.character)
# Uemployment
unemp = read.csv("Socio\ Economic\ Factors/Unemployment.csv")

########################################################################
############################# CHR Data 2019 ############################
########################################################################
chr_19 = read.csv("CHR/CHR\ 2019/analytic_data2019.csv")

chr_19_rankings = read.xls(xls= "CHR/CHR\ 2019/2019\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=2)
colnames(chr_19_rankings)[c(1,2,3,4,6,8)] = lapply(chr_19_rankings[1,c(1,2,3,4,6,8)], as.character)
chr_19_rankings = chr_19_rankings[-1,]
# Subrankings
chr_19_sub_rankings = read.xls(xls= "CHR/CHR\ 2019/2019\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=3)
colnames(chr_19_sub_rankings)[c(-5,-7,-9,-11,-13,-15)] = lapply(chr_19_sub_rankings[1,c(-5,-7,-9,-11,-13,-15)], as.character)
chr_19_sub_rankings = chr_19_sub_rankings[-1,]
# Join these columns
chr_19_r = merge(chr_19_rankings, chr_19_sub_rankings,by=c("FIPS","State","County","# of Ranked Counties"))

### VALUES ###
chr_19_measure= read.xls(xls= "CHR/CHR\ 2019/2019\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=4)
colnames(chr_19_measure)[c(-4,-9,-41)] = lapply(chr_19_measure[1,c(-4,-9,-41)], as.character)
chr_19_measure = chr_19_measure[-1,]

chr_19_add_measure= read.xls(xls= "CHR/CHR\ 2019/2019\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=6)
colnames(chr_19_add_measure)[c(-4,-8,-12,-29,-44,-48)] = lapply(chr_19_add_measure[1,c(-4,-8,-12,-29,-44,-48)], as.character)
chr_19_add_measure = chr_19_add_measure[-1,]

### Create vector of data to be used in modeling
v =  c('FIPS', 'State', 'County', 'Premature.death', '% Fair/Poor', 'Physically Unhealthy Days', 'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', 'Physical.inactivity', '% Excessive Drinking', '# Driving Deaths', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 'Preventable Hosp. Rate', '% Screened', 'Graduation Rate', '% Some College', '% Unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate')
v2 = c('FIPS', '% < 18', '% 65 and over', '% African American', '% American Indian/Alaskan Native', '% Asian', '% Native Hawaiian/Other Pacific Islander', '% Hispanic', '% Not Proficient in English', '% Female', '% Rural', '% Diabetic', 'HIV Prevalence Rate', 'Household Income', '% Free or Reduced Lunch', '# Rural')
# v[!v %in% colnames(chr_19_measure)]  # Find the mislabeled columns
chr_19_data = chr_19_measure[,v]
chr_19_data$'# Driving Deaths' = chr_19_add_measure$'MV Mortality Rate'
colnames(chr_19_data)[11] = 'MV Mortality Rate'

chr_19_data = merge(chr_19_data, chr_19_add_measure[,v2], by='FIPS')
chr_19_data$'# Rural' = chr_19_measure$'% Drive Alone'
colnames(chr_19_data)[39] = '% Drive Alone'


########################################################################
############################# CHR Data 2018 ############################
########################################################################
chr_18 = read.csv("CHR/CHR\ 2018/analytic_data2018_0.csv")

chr_18_rankings = read.xls(xls= "CHR/CHR\ 2018/2018\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=2)
colnames(chr_18_rankings)[c(1,2,3,4,6,8)] = lapply(chr_18_rankings[1,c(1,2,3,4,6,8)], as.character)
chr_18_rankings = chr_18_rankings[-1,]
# Subrankings
chr_18_sub_rankings = read.xls(xls= "CHR/CHR\ 2018/2018\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=3)
colnames(chr_18_sub_rankings)[c(-5,-7,-9,-11,-13,-15)] = lapply(chr_18_sub_rankings[1,c(-5,-7,-9,-11,-13,-15)], as.character)
chr_18_sub_rankings = chr_18_sub_rankings[-1,]
# Join these columns
chr_18_r = merge(chr_18_rankings, chr_18_sub_rankings,by=c("FIPS","State","County","# of Ranked Counties"))

### VALUES ###
chr_18_measure= read.xls(xls= "CHR/CHR\ 2018/2018\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=4)
colnames(chr_18_measure)[c(-4,-9,-41)] = lapply(chr_18_measure[1,c(-4,-9,-41)], as.character)
chr_18_measure = chr_18_measure[c(-1,-3144),]

chr_18_add_measure= read.xls(xls= "CHR/CHR\ 2018/2018\ County\ Health\ Rankings\ Data\ -\ v2.xls", sheet=6)
colnames(chr_18_add_measure)[c(-4,-8,-12,-29,-44,-48)] = lapply(chr_18_add_measure[1,c(-4,-8,-12,-29,-44,-48)], as.character)
chr_18_add_measure = chr_18_add_measure[-1,]

### Create vector of data to be used in modeling
v =  c('FIPS', 'State', 'County', 'Premature.death', '% Fair/Poor', 'Physically Unhealthy Days', 'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', 'Physical.inactivity', '% Excessive Drinking', '# Driving Deaths', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 'Preventable Hosp. Rate', '% Mammography', 'Graduation Rate', '% Some College', '% Unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate')
v2 = c('FIPS', '% < 18', '% 65 and over', '% African American', '% American Indian/Alaskan Native', '% Asian', '% Native Hawaiian/Other Pacific Islander', '% Hispanic', '% Not Proficient in English', '% Female', '% Rural', '% Diabetic', 'HIV Prevalence Rate', 'Household Income', '% Free or Reduced Lunch', '# Rural')
# v[!v %in% colnames(chr_18_measure)]  # Find the mislabeled columns
chr_18_data = chr_18_measure[,v]
chr_18_data$'# Driving Deaths' = chr_18_add_measure$'X.31'
colnames(chr_18_data)[11] = 'MV Mortality Rate'

chr_18_data = merge(chr_18_data, chr_18_add_measure[,v2], by='FIPS')
chr_18_data$'# Rural' = chr_18_measure$'% Drive Alone'
colnames(chr_18_data)[39] = '% Drive Alone'


########################################################################
############################# CHR Data 2016 ############################
########################################################################
chr_16 = read.csv("CHR/CHR\ 2016/analytic_data2016.csv")

### RANKINGS ###
chr_16_rankings = read.xls(xls= "CHR/CHR\ 2016/2016\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=2)
colnames(chr_16_rankings)[c(1,2,3,4,6,8)] = lapply(chr_16_rankings[1,c(1,2,3,4,6,8)], as.character)
chr_16_rankings = chr_16_rankings[-1,]
# Subrankings
chr_16_sub_rankings = read.xls(xls= "CHR/CHR\ 2016/2016\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=3)
colnames(chr_16_sub_rankings)[c(-5,-7,-9,-11,-13,-15)] = lapply(chr_16_sub_rankings[1,c(-5,-7,-9,-11,-13,-15)], as.character)
chr_16_sub_rankings = chr_16_sub_rankings[-1,]
# Join these columns
chr_16_r = merge(chr_16_rankings, chr_16_sub_rankings,by=c("FIPS","State","County","# of Ranked Counties"))

### VALUES ###
chr_16_measure= read.xls(xls= "CHR/CHR\ 2016/2016\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=4)
colnames(chr_16_measure)[c(-4,-9,-41)] = lapply(chr_16_measure[1,c(-4,-9,-41)], as.character)
chr_16_measure = chr_16_measure[c(-1,-3143),]

chr_16_add_measure= read.xls(xls= "CHR/CHR\ 2016/2016\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=6)
colnames(chr_16_add_measure)[c(-4,-8,-12,-29,-44,-48)] = lapply(chr_16_add_measure[1,c(-4,-8,-12,-29,-44,-48)], as.character)
chr_16_add_measure = chr_16_add_measure[c(-1,-3143),]

### Create vector of data to be used in modeling
v =  c('FIPS', 'State', 'County', 'Years of Potential Life Lost Rate', 'Poor.or.fair.health', 'Physically Unhealthy Days', 'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', '% Physically Inactive', '% Excessive Drinking', 'Premature.death', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 'Preventable Hosp. Rate', '% Mammography', 'Graduation Rate', '% Some College', '% Unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate')
v2 = c('FIPS', '% < 18', '% 65 and over', '% African American', '% American Indian/Alaskan Native', '% Asian', '% Native Hawaiian/Other Pacific Islander', '% Hispanic', '% Not Proficient in English', '% Female', '% Rural', '% Diabetic', 'HIV Prevalence Rate', 'Household Income', '% Free Lunch', '# Rural')
# v[!v %in% colnames(chr_16_measure)]  # Find the mislabeled columns
chr_16_data = chr_16_measure[,v]
chr_16_data$'Premature.death' = chr_16_add_measure$'MV Mortality Rate'
colnames(chr_16_data)[11] = 'MV Mortality Rate'

chr_16_data = merge(chr_16_data, chr_16_add_measure[,v2], by='FIPS')
chr_16_data$'# Rural' = chr_16_measure$'% Drive Alone'
colnames(chr_16_data)[39] = '% Drive Alone'


########################################################################
############################# CHR Data 2015 ############################
########################################################################
chr_15 = read.csv("CHR/CHR\ 2015/analytic_data2015.csv")

### RANKINGS ###
chr_15_rankings = read.xls(xls= "CHR/CHR\ 2015/2015\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=2)
colnames(chr_15_rankings) = lapply(chr_15_rankings[1,], as.character)
chr_15_rankings = chr_15_rankings[-1,]
# Subrankings
chr_15_sub_rankings = read.xls(xls= "CHR/CHR\ 2015/2015\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=3)
colnames(chr_15_sub_rankings) = lapply(chr_15_sub_rankings[1,], as.character)
chr_15_sub_rankings = chr_15_sub_rankings[-1,]
# Join these columns
chr_15_r = merge(chr_15_rankings, chr_15_sub_rankings,by=c("FIPS","State","County","# of Ranked Counties"))

### VALUES ###
chr_15_measure= read.xls(xls= "CHR/CHR\ 2015/2015\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=4)
colnames(chr_15_measure) = lapply(chr_15_measure[1,], as.character)
chr_15_measure = chr_15_measure[c(-1,-3143),]

chr_15_add_measure= read.xls(xls= "CHR/CHR\ 2015/2015\ County\ Health\ Rankings\ Data\ -\ v3.xls", sheet=6)
colnames(chr_15_add_measure) = lapply(chr_15_add_measure[1,], as.character)
chr_15_add_measure = chr_15_add_measure[c(-1,-3143),]


### Create vector of data to be used in modeling
v =  c('FIPS', 'State', 'County', 'Years of Potential Life Lost Rate', '% Fair/Poor', 'Physically Unhealthy Days', 'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', '% Physically Inactive', '% Excessive Drinking', '# Deaths', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 'Preventable Hosp. Rate', '% Mammography', 'Graduation Rate', '% Some College', '% Unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate')
v2 = c('FIPS', '% < 18', '% 65 and over', '% African American', '% American Indian/ Alaskan Native', '% Asian', '% Native Hawaiian/ Other Pacific Islander', '% Hispanic', '% Not Proficient in English', '% Female', '% Rural', '% Diabetic', 'HIV Prevalence Rate', 'Household Income', '% Free Lunch', '# < 18')
# v[!v %in% colnames(chr_15_measure)]  # Find the mislabeled columns
chr_15_data = chr_15_measure[,v]
chr_15_data$'# Deaths' = chr_15_add_measure$'MV Mortality Rate'
colnames(chr_15_data)[11] = 'MV Mortality Rate'

chr_15_data = merge(chr_15_data, chr_15_add_measure[,v2], by='FIPS')
chr_15_data$'# < 18' = chr_15_measure$'% Drive Alone'
colnames(chr_15_data)[39] = '% Drive Alone'


########################################################################
############################# CHR Data 2012 ############################
########################################################################
chr_12 = read.csv("CHR/CHR\ 2012/analytic_data2012.csv")

### RANKINGS ###
chr_12_rankings = read.xls(xls= "CHR/CHR\ 2012/2012\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=2)
colnames(chr_12_rankings)[c(1,2,3,4,6,8)] = lapply(chr_12_rankings[1,c(1,2,3,4,6,8)], as.character)
chr_12_rankings = chr_12_rankings[-1,]
# Subrankings
chr_12_sub_rankings = read.xls(xls= "CHR/CHR\ 2012/2012\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=3)
colnames(chr_12_sub_rankings)[c(-5,-7,-9,-11,-13,-15)] = lapply(chr_12_sub_rankings[1,c(-5,-7,-9,-11,-13,-15)], as.character)
chr_12_sub_rankings = chr_12_sub_rankings[-1,]
# Join these columns
chr_12_r = merge(chr_12_rankings, chr_12_sub_rankings,by=c("FIPS","State","County","# of Ranked Counties"))

### VALUES ###
chr_12_measure= read.xls(xls= "CHR/CHR\ 2012/2012\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=4)
colnames(chr_12_measure) = lapply(chr_12_measure[1,], as.character)
chr_12_measure = chr_12_measure[c(-1,-3143),]

chr_12_add_measure= read.xls(xls= "CHR/CHR\ 2012/2012\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=6)
colnames(chr_12_add_measure) = lapply(chr_12_add_measure[1,], as.character)
chr_12_add_measure = chr_12_add_measure[-1,]


### Create vector of data to be used in modeling
v =  c('FIPS', 'State', 'County', 'YPLL Rate', '% Fair/Poor', 'Physically Unhealthy Days', 'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', '% Physically Inactive', '% Excessive Drinking', 'MV Mortality Rate', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 'ACSC Rate', '% Mammography', '% AFGR', '% PSED', '% Unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate')
v2 = c('FIPS', '< 18', '65 and over', 'African American', 'American Indian/ Alaskan Native', 'Asian', 'Native Hawaiian/ Other Pacific Islander', 'Hispanic', '% not proficient in English', 'Female', 'Rural', '% diabetic', 'HIV Rate', 'Household Income', '% Free lunch', '% Drive Alone')
# v[!v %in% colnames(chr_12_measure)]  # Find the mislabeled columns
chr_12_data = chr_12_measure[,v]

chr_12_data = merge(chr_12_data, chr_12_add_measure[,v2], by='FIPS')


########################################################################
############################# CHR Data 2011 ############################
########################################################################
chr_11 = read.csv("CHR/CHR\ 2011/analytic_data2011.csv")
### RANKINGS ###
chr_11_rankings = read.xls(xls= "CHR/CHR\ 2011/2011\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=2)
colnames(chr_11_rankings) = lapply(chr_11_rankings[1,], as.character)
chr_11_rankings = chr_11_rankings[-1,]
# Subrankings
chr_11_sub_rankings = read.xls(xls= "CHR/CHR\ 2011/2011\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=3)
colnames(chr_11_sub_rankings) = lapply(chr_11_sub_rankings[1,], as.character)
chr_11_sub_rankings = chr_11_sub_rankings[-1,]
# Join these columns
chr_11_r = merge(chr_11_rankings, chr_11_sub_rankings,by=c("FIPS","State","County","# of Ranked Counties"))

### VALUES ###
chr_11_measure= read.xls(xls= "CHR/CHR\ 2011/2011\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=4)
colnames(chr_11_measure) = lapply(chr_11_measure[1,], as.character)
chr_11_measure = chr_11_measure[c(-1,-3143),]

chr_11_add_measure= read.xls(xls= "CHR/CHR\ 2011/2011\ County\ Health\ Rankings\ National\ Data_v2_0.xls", sheet=6)
colnames(chr_11_add_measure) = lapply(chr_11_add_measure[1,], as.character)
chr_11_add_measure = chr_11_add_measure[-1,]


### Create vector of data to be used in modeling
v =  c('FIPS', 'State', 'County', 'YPLL Rate', '% Fair/Poor', 'Physically Unhealthy Days', 'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', 'Deaths', '% Excessive Drinking', 'MV mortality rate', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 'ACSC Rate', 'Mammography Rate', 'AFGR', 'PSED', '% unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate')
v2 = c('FIPS', 'Percent  < 18', 'Percent 65 and over', 'Percent African American', 'Percent American Indian/ Alaskan Native', 'Percent Asian', 'Percent Native Hawaiian/ Other Pacific Islander', 'Percent Hispanic', '% not proficient in English', 'Percent Female', 'Percent Rural', 'Diabetes', 'HIV rate', 'Household income', '% free lunch', '% Drive Alone')
chr_11_data = chr_11_measure[,v]
chr_11_data$Deaths = chr_11_add_measure$'% physically inactive'
colnames(chr_11_data)[11] = '% physically inactive'

chr_11_data = merge(chr_11_data, chr_11_add_measure[,v2], by='FIPS')

########################################################################
########################### GROUP PROCESSING ###########################
########################################################################

names =  c('FIPS', 'State', 'County', 'YPLL', '% Fair/Poor', 'Physically Unhealthy Days', 
           'Mentally Unhealthy Days', '% LBW', '% Smokers', '% Obese', 'Physical Inactivity', 
           '% Excessive Drinking', 'MV Mortality Rate', 'Teen Birth Rate', '% Uninsured', 'PCP Rate', 
           'Preventable Hosp. Rate', '% Screened Mammogram', 'Graduation Rate', '% Some College', 
           '% Unemployed', '% Children in Poverty', '% Single-Parent Households', 'Violent Crime Rate', 
           '% < 18', '% 65 and over', '% African American', '% American Indian/Alaskan Native', 
           '% Asian', '% Native Hawaiian/Other Pacific Islander', '% Hispanic', '% Not Proficient in English', 
           '% Female', '% Rural', '% Diabetic', 'HIV Prevalence Rate', 'Household Income', '% Free or Reduced Lunch', '% Drive Alone')
colnames(chr_19_data) = names
colnames(chr_18_data) = names
colnames(chr_16_data) = names
colnames(chr_15_data) = names
colnames(chr_12_data) = names
colnames(chr_11_data) = names

# DATA IS WAY TOO LARGE IN 19
chr_19_data$'Preventable Hosp. Rate' = chr_18_data$'Preventable Hosp. Rate' 

chr_19_data[,c(-2,-3)] = sapply(sapply(chr_19_data[,c(-2,-3)],as.character), as.numeric)
chr_18_data[,c(-2,-3)] = sapply(sapply(chr_18_data[,c(-2,-3)],as.character), as.numeric)
chr_16_data[,c(-2,-3)] = sapply(sapply(chr_16_data[,c(-2,-3)],as.character), as.numeric)
chr_15_data[,c(-2,-3)] = sapply(sapply(chr_15_data[,c(-2,-3)],as.character), as.numeric)
chr_12_data[,c(-2,-3)] = sapply(sapply(chr_12_data[,c(-2,-3)],as.character), as.numeric)
chr_11_data[,c(-2,-3)] = sapply(sapply(chr_11_data[,c(-2,-3)],as.character), as.numeric)

########################################################################
############################# HANDLE NA'S ##############################
########################################################################

library(DMwR)
chr_11_full = knnImputation(chr_11_data, k = 10, scale = T, meth = "weighAvg")
chr_12_full = knnImputation(chr_12_data, k = 10, scale = T, meth = "weighAvg")
chr_15_full = knnImputation(chr_15_data, k = 10, scale = T, meth = "weighAvg")
chr_16_full = knnImputation(chr_16_data, k = 10, scale = T, meth = "weighAvg")
chr_18_full = knnImputation(chr_18_data, k = 10, scale = T, meth = "weighAvg")
chr_19_full = knnImputation(chr_19_data, k = 10, scale = T, meth = "weighAvg")


########################################################################
############################# SAVE TO CSV ##############################
########################################################################
write.csv(chr_11_data,"data/hNA11.csv", row.names = FALSE)
write.csv(chr_12_data,"data/hNA12.csv", row.names = FALSE)
write.csv(chr_15_data,"data/hNA15.csv", row.names = FALSE)
write.csv(chr_16_data,"data/hNA16.csv", row.names = FALSE)
write.csv(chr_18_data,"data/hNA18.csv", row.names = FALSE)
write.csv(chr_19_data,"data/hNA19.csv", row.names = FALSE)

write.csv(chr_11_full,"data/h11.csv", row.names = FALSE)
write.csv(chr_12_full,"data/h12.csv", row.names = FALSE)
write.csv(chr_15_full,"data/h15.csv", row.names = FALSE)
write.csv(chr_16_full,"data/h16.csv", row.names = FALSE)
write.csv(chr_18_full,"data/h18.csv", row.names = FALSE)
write.csv(chr_19_full,"data/h19.csv", row.names = FALSE)

write.csv(labels_12,"data/e12.csv", row.names = FALSE)
write.csv(labels_16,"data/e16.csv", row.names = FALSE)
sum(is.na(labels_12))
sum(is.na(labels_16))




