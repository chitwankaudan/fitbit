import numpy as np
import pandas as pd
import argparse

def gen_datasets(raw_file, sleep_file, wake_file):
	# Load raw dataset
	data = pd.read_csv(raw_file)

	# Conver to datetimes
	data['Start Time'] = pd.to_datetime(data['Start Time'])
	data['End Time'] = pd.to_datetime(data['End Time'])

	# Generate dates
	sleepdf = gen_dates(data, 'sleep')
	wakedf = gen_dates(data, 'wake')

	# Add asleep indicators
	sleepdf = add_sleep_indicators(data, sleepdf)
	wakedf = add_sleep_indicators(data, wakedf)

	# Save datasets to disk
	sleepdf.to_csv(sleep_file, index=False)
	wakedf.to_csv(wake_file, index=False)

def gen_dates(data, df_type):
	if df_type == 'sleep':
		colname = 'Start Time'
	else:
		colname = 'End Time'

	# Start and end hours for sleep/wake times
	start_hour = data[colname].dt.hour.min()
	end_hour = data[colname].dt.hour.max() + 1

	days = data[colname].dt.date
	start_datetime = data[colname].dt.date.min() + pd.DateOffset(hours=int(start_hour))
	end_datetime = data[colname].dt.date.max() + pd.DateOffset(hours=int(end_hour))
	dts = pd.date_range(start_datetime, end_datetime, freq='60S')

	# Build empty array
	output = pd.DataFrame()
	output['Datetime'] = dts
	output['Asleep'] = 0
	output = output.loc[(output['Datetime'].dt.date).isin(days),]
	output = output.loc[output['Datetime'].dt.hour >= start_hour, ]
	output = output.loc[output['Datetime'].dt.hour < end_hour, ]
	output['Time Offset'] = np.tile(np.arange(1, 60*(end_hour-start_hour)+1), days.shape[0])
	return output

def add_sleep_indicators(data, output):
	# Asleep=1 when I am sleeping
	for i in range(data.shape[0]):
	    sleeptime = data.loc[i, 'Start Time']
	    waketime = data.loc[i, 'End Time']
	    output.loc[(output['Datetime'] >= sleeptime) &\
	    	(output['Datetime'] < waketime), 'Asleep'] = 1
	return output

def parse_arguments():
	parser = argparse.ArgumentParser(description='Creates sleep indicator dataset.')
	parser.add_argument('-raw_file', type=str, default="./raw_sleepdata.csv", 
		help='Specifiy relative file path to raw data file')
	parser.add_argument('-sleep_file', type=str, default="./sleepdf.csv", 
		help='Specify where to save clean sleep dataset')
	parser.add_argument('-wake_file', type=str, default="./wakedf.csv", 
		help='Specify where to save clean wake dataset')
	args = parser.parse_args()
	return args

if __name__ == '__main__':
	args = parse_arguments()
	gen_datasets(args.raw_file, args.sleep_file, args.wake_file)
	print("Sleep and Wake indicator datasets created!")


