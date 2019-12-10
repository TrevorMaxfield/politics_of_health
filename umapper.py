#!/usr/bin/env python3
import numpy as np
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import pandas as pd

import umap as umap

# Indices of health vs demographic factors (after removing FIPS, State, County)
HI = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,31,32]
DI = [15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,33,33,34,35]

# From https://umap-learn.readthedocs.io/en/latest/basic_usage.html

def umapper(x, y, hd=False, title=""):
	"""
		x : Numeric dataframe.  Dimension reduction performed on it.
		y : Numeric dataframe (1D).  Labels for x values.
		hd: Whether to use HI if "h", DI if "d", or both if False
		title: "UMAP Project" + title is title of plot
	"""
	reducer = umap.UMAP()
	reducer = umap.UMAP(random_state=314) #, min_dist=0.3,n_neighbors=4)
	if hd == "h":
		x = x.iloc[:,HI]
	elif hd == "d":
		x = x.iloc[:,DI]
	reducer.fit(x)
	embedding = reducer.transform(x)
	print(embedding.shape)
	plt.scatter(embedding[:, 0], embedding[:, 1], c=y, cmap= 'RdBu', s=5)
	plt.gca().set_aspect('equal', 'datalim')
	cbar = plt.colorbar(boundaries=np.arange(0,1.1,0.1))
	cbar.set_ticks(np.arange(0,1.1,0.1))
	cbar.ax.set_ylabel('% Democrat Vote Share per County', labelpad=15, rotation=270)
	plt.title('UMAP Projection ' + title, fontsize=18)
	plt.show()


# Elections
e12 = pd.read_csv("data/e12.csv")
e16 = pd.read_csv("data/e16.csv")


# 2011
h11 = pd.read_csv("data/h11.csv")

all_11 = h11.merge(e12[["FIPS","dem_per"]], on ="FIPS")
e_11_valid = all_11.pop("dem_per")
a11 =(all_11.iloc[:,3:]-all_11.iloc[:,3:].mean())/all_11.iloc[:,3:].std()


# 2012
h12 = pd.read_csv("data/h12.csv")

all_12 = h12.merge(e12[["FIPS","dem_per"]], on ="FIPS")
e_12_valid = all_12.pop("dem_per")
a12 =(all_12.iloc[:,3:]-all_12.iloc[:,3:].mean())/all_12.iloc[:,3:].std()


# 2015
h15 = pd.read_csv("data/h15.csv")

all_15 = h15.merge(e16[["FIPS","dem_per"]], on ="FIPS")
e_15_valid = all_15.pop("dem_per")
a15 =(all_15.iloc[:,3:]-all_15.iloc[:,3:].mean())/all_15.iloc[:,3:].std()


# 2016
h16 = pd.read_csv("data/h16.csv")

all_16 = h16.merge(e16[["FIPS","dem_per"]], on ="FIPS")
e_16_valid = all_16.pop("dem_per")
a16 =(all_16.iloc[:,3:]-all_16.iloc[:,3:].mean())/all_16.iloc[:,3:].std()


# 2019
h19 = pd.read_csv("data/h19.csv")
all_19 = h19.merge(e16[["FIPS","dem_per"]], on ="FIPS")
e_16_valid = all_19.pop("dem_per")
a19 =(all_19.iloc[:,3:]-all_19.iloc[:,3:].mean())/all_19.iloc[:,3:].std()



umapper(a11, e_11_valid, "h", "2011 Health vs 2012 Election")
umapper(a11, e_11_valid, "d", "2011 Demographics vs 2012 Election")
umapper(a12, e_12_valid, "h", "2012 Health vs 2012 Election")
umapper(a12, e_12_valid, "d", "2012 Demographics vs 2012 Election")
umapper(a12, e_12_valid, False, "2012 All vs 2012 Election")

umapper(a11.merge(a12, left_index=True, right_index=True), e_12_valid, False, "2011 & 2012 All vs 2012 Election")
umapper(a11.iloc[:,HI].merge(a12.iloc[:,HI], left_index=True, right_index=True), e_12_valid, False, "2011 & 2012 Health vs 2012 Election")
umapper(a11.iloc[:,DI].merge(a12.iloc[:,DI], left_index=True, right_index=True), e_12_valid, False, "2011 & 2012 Demographics vs 2012 Election")

umapper(a15, e_15_valid, "h", "2015 Health vs 2016 Election")
umapper(a15, e_15_valid, "d", "2015 Demographics vs 2016 Election")
umapper(a16, e_16_valid, "h", "2016 Health vs 2016 Election")
umapper(a16, e_16_valid, "d", "2016 Demographics vs 2016 Election")
umapper(a16, e_16_valid, False, "2016 All vs 2016 Election")

umapper(a15.merge(a16, left_index=True, right_index=True), e_16_valid, False, "2015 & 2016 All vs 2012 Election")
umapper(a15.iloc[:,HI].merge(a16.iloc[:,HI], left_index=True, right_index=True), e_16_valid, False, "2015 & 2016 Health vs 2012 Election")
umapper(a15.iloc[:,DI].merge(a16.iloc[:,DI], left_index=True, right_index=True), e_16_valid, False, "2015 & 2016 Demographics vs 2012 Election")


