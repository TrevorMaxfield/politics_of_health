import numpy as np
import pandas as pd

import tensorflow as tf

from tensorflow import feature_column, keras
from tensorflow.keras import layers, models
from sklearn.model_selection import train_test_split

"""
	df = df_og.copy()
	df = df.drop(columns=["FIPS"])
	labels = df.pop('dem_per')
	df = (df-m)/s  # Normalize
	ds = tf.data.Dataset.from_tensor_slices((np.expand_dims(np.transpose(df.values.reshape(len(df),2,36),(0,2,1)), axis=3),
											 labels))
"""

H = list(range(0,17))+list(range(36,53))
D = list(range(17,36))+list(range(53,72))

BATCH_SIZE = 32

# Adapted from https://www.tensorflow.org/tutorials/structured_data/feature_columns
def make_ds(df_og, m, s, shuffle=True, bs=32, select=[]):
	ds = tf.data.Dataset.from_tensor_slices(transform(df_og, m, s, select))
	if shuffle:
		ds = ds.shuffle(buffer_size=len(df_og))
	ds = ds.batch(bs).repeat()
	return ds

# Transform data to network format (no dataset)
def transform(df_og, m, s, select=[]):
	df = df_og.copy()
	df = df.drop(columns=["FIPS"])
	labels = df.pop('dem_per')
	df = (df-m)/s  # Normalize
	rows = 36
	if select != []:
		rows = len(select)//2
		df = df.iloc[:,select]
	return (np.expand_dims(np.transpose(df.values.reshape(len(df),2,rows),(0,2,1)), axis=3), labels)


###############################
######## 2012 ELECTION ########
###############################
e12 = pd.read_csv("data/e12.csv")

# 2011
h11 = pd.read_csv("data/h11.csv")
all_11 = h11.iloc[:, [0] + list(range(3, 18)) + [34,35] + list(range(18, 34)) + [36, 37, 38]]

# 2012
h12 = pd.read_csv("data/h12.csv")
all_12 = h12.iloc[:, [0] + list(range(3, 18)) + [34,35] + list(range(18, 34)) + [36, 37, 38]]

# Combine
a_12 = all_12.merge(all_11, on="FIPS")
a_12 = a_12.merge(e12[["FIPS", "dem_per"]], on="FIPS")

MEAN = a_12.iloc[:,1:73].mean()
ST_D = a_12.iloc[:,1:73].std()

###############################
######## 2016 ELECTION ########
###############################
e16 = pd.read_csv("data/e16.csv")

# 2015
h15 = pd.read_csv("data/h15.csv")
all_15 = h15.iloc[:, [0] + list(range(3, 18)) + [34,35] + list(range(18, 34)) + [36, 37, 38]]

# 2016
h16 = pd.read_csv("data/h16.csv")
all_16 = h16.iloc[:, [0] + list(range(3, 18)) + [34,35] + list(range(18, 34)) + [36, 37, 38]]

# Combine
a_16 = all_16.merge(all_15, on="FIPS")
a_16 = a_16.merge(e16[["FIPS", "dem_per"]], on="FIPS")

################################
######## 2020 ELECTION? ########
################################
h18 = pd.read_csv("data/h18.csv")
all_18 = h18.iloc[:, [0] + list(range(3, 18)) + [34,35] + list(range(18, 34)) + [36, 37, 38]]
h19 = pd.read_csv("data/h19.csv")
all_19 = h19.iloc[:, [0] + list(range(3, 18)) + [34,35] + list(range(18, 34)) + [36, 37, 38]]

a_19 = all_18.merge(all_19, on="FIPS")
a_19 = a_19[a_19["FIPS"].isin(e16["FIPS"])]  # Select only FIPS found in election results from 2016 (Alaska issues)

###############################
######## SPLIT UP DATA ########
###############################

train, val = train_test_split(a_16, test_size=0.04)
test = a_19
print(len(train), 'train examples')
print(len(val), 'validation examples')
print(len(test), 'test examples')


train_ds = make_ds(train,MEAN, ST_D, bs=BATCH_SIZE, select=[])
val_ds = make_ds(val,MEAN, ST_D, shuffle=False, bs=BATCH_SIZE, select=H)
test_ds = make_ds(test,MEAN, ST_D, shuffle=False, bs=BATCH_SIZE, select=[])

################################
######## DEFINE NETWORK ########
################################
# From https://www.tensorflow.org/tutorials/images/cnn
# https://www.tensorflow.org/tutorials/keras/regression
model = models.Sequential()

model.add(layers.Conv2D(36, (1, 2), strides=(1,1), activation="relu", input_shape=(17,2,1)))
model.add(layers.Flatten())
model.add(layers.Dense(64, activation="relu"))
model.add(layers.Dense(64, activation="relu"))
model.add(layers.Dense(64, activation="relu"))
model.add(layers.Dense(64, activation="relu"))
model.add(layers.Dense(1, activation="sigmoid"))

# Keras stuff
optimizer = tf.keras.optimizers.RMSprop(0.001)
early_stop = keras.callbacks.EarlyStopping(monitor='val_loss', patience=20)

# Create/show model
model.compile(optimizer=optimizer,
              loss='mae',
              metrics=['mae', 'mse'])
model.summary()

# The patience parameter is the amount of epochs to check for improvement


early_history = model.fit(train_ds, 
						  validation_data=val_ds,
						  validation_steps=len(val)//BATCH_SIZE,
						  steps_per_epoch=len(train)//BATCH_SIZE,
                    	  epochs=500, 
                    	  callbacks=[early_stop])

model.evaluate(test_ds, steps=len(test)//BATCH_SIZE)



model.save('models/early_all_2_72C_Leaky_TestVal_256.h5')
#new_model = keras.models.load_model('path_to_my_model.h5')
#https://www.tensorflow.org/guide/keras/train_and_evaluate

out = model.evaluate(test_ds, steps=len(test)//BATCH_SIZE)
print(out)

model.predict(test_ds,steps=1)


# Create a csv of predictions
data_16,label_16 = transform(a_16,MEAN,ST_D,[])
pre = model.predict(data_16)
pre_df = pd.DataFrame(pre)
pre_df["actual"] = label_16
pre_df.to_csv("predict_demographics_16.csv")


###################################
######## 2012 & 2016 MODEL ########
###################################
# Dataset can't handle 6000 rows on my computer for some reason ? 
train = train.append(a_12)
x,y= transform(train, MEAN, ST_D, H)
early_history = model.fit(x,y.values, 
						  batch_size= 32,
						  validation_data=val_ds,
						  validation_steps=len(val)//BATCH_SIZE,
                    	  epochs=500, 
                    	  callbacks=[early_stop])

# 2019 Predictions
test = a_19
test = (test-MEAN)/ST_D 
test = test.drop(columns=["FIPS"])
test = test.iloc[:,H]
test = np.expand_dims(np.transpose(test.values.reshape(len(test),2,17),(0,2,1)), axis=3)
pre = np.squeeze(model.predict(test))
pre_df = pd.DataFrame(pre)
pre_df.to_csv("predict_19_hel.csv")





