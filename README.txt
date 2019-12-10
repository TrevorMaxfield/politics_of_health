Code for Trevor Maxfield's Data Analytics (ITWS 4600 Fall 2019) semester project on the politics of health.

read_data.r
-----------
Load data sets from County Health Rankings and MIT Election Lab.  Reformats and selects factors.  
Save to workspace to load into other r files.

eda.r
-----
Initial data exploration, including heatmap and urbanmappr code.

linear_model.r
--------------
Build and evaluate a linear model on the data from read_data as well as produce maps of the results.  
Also evalutes electoral college results.

neural_model.r
--------------
Read in the results from nn.py and produce maps/electoral college results.

umapper.py
----------
Framework to apply UMAP dimension reduction to the health and demographic data against vote share.

nn.py
-----
Keras neural network framework for the convolutional model.
