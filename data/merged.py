import numpy as np
import pandas as pd
import argparse


def gen_merged_df(activity_data, sleep_data, canvas_data, merged_data):
	a = pd.read_csv(activity_data)
	s = pd.read_csv(sleep_data)
	c = pd.read_csv(canvas_data)

	a, s, c = prep_for_merge(a, s, c)
	# Merge datasets
	sa = a.merge(s, on='Date')
	csa = sa.merge(c, on='Date', how='outer')

	# Add assignment features
	assign = extract_assign_data(csa)
	data = sa.merge(assign, on='Date', how='outer')

	# Add week level assignment data
	data = add_weeklevel_assign(data)

	# Add weekend and spring break indicators
	data = add_weekend_break(data)

	# Clean final df
	data = clean_final_df(data)

	# Save df
	data.to_csv(merged_data, index=False)
	
def add_weekend_break(data):
	# Add is weekend
	data['Is Weekend'] = np.where(pd.to_datetime(data['Date']).dt.dayofweek > 4, 1,0)
	# Add is break
	springbreak = [12, 13]
	data['Is Break'] = np.where(((data['weeknum']==12)|(data['weeknum']==13)),1,0) 
	return data

def add_weeklevel_assign(data):
	# Add weeknum
	data['weeknum'] = pd.to_datetime(data['Date']).dt.week
	assign_week = pd.DataFrame(np.sum(data.groupby(['weeknum'])['homework',
	                                          'project','exam'].sum(), 1), 
	                           columns=['Weekly Assign Num'])
	assign_week.reset_index(inplace=True)
	data = data.merge(assign_week, on='weeknum')
	return data

def prep_for_merge(a, s, c):
	# Data Cleaning
	s['Start Time'] = pd.to_datetime(s['Start Time'])
	s['End Time'] = pd.to_datetime(s['End Time'])
	a['Date'] = pd.to_datetime(a['Date'])
	c['DUE'] = pd.to_datetime(c['DUE'])
	# Change datetime format to prep for merge
	s['Date'] = s['Start Time'].dt.strftime('%m/%d/%y')
	a['Date'] = a['Date'].dt.strftime('%m/%d/%y')
	c['Date'] = (pd.DatetimeIndex(c['DUE']) - pd.DateOffset(1)).strftime('%m/%d/%y') #night before assignment is due
	return (a, s, c)


def extract_assign_data(csa):
	# Extract variables from assignment summary data
	csa['homework'] = 0
	csa['project'] = 0
	csa['exam'] = 0
	for i in range(csa.shape[0]):
	    
	    if pd.isna(csa.loc[i,'SUMMARY']):
	        continue
	    txt = csa.loc[i,'SUMMARY'].lower().replace('assignment', 'homework')
	    txt = txt.replace('test', 'exam')
	    
	    if 'exam' in txt:
	        csa.loc[i,'exam'] = 1
	    elif 'homework' in txt:
	        csa.loc[i,'homework'] = 1
	    elif 'project' in txt:
	        csa.loc[i,'project'] = 1

	assign = csa.groupby(['Date'])['homework','project','exam'].sum()
	return assign


def clean_final_df(data):
	drop_col = ['Date', 'Floors', 'weeknum']
	data.drop(columns=drop_col, inplace=True)
	data = data[pd.notna(data['Start Time'])]
	data.reset_index(drop=True, inplace=True)

	# turn into columns into float so R doesn't mistake them as categorical
	int_cols = ['Calories Burned', 'Steps', 'Minutes Sedentary', 'Activity Calories']
	for col in int_cols:
	    data.loc[:,col] = data[col].str.replace(',', '')
	    data.loc[:,col] = data[col].astype(np.float)
	return data


def parse_arguments():
	parser = argparse.ArgumentParser(description='Creates merged dataset \
		with activity, assignment and sleep data.')
	parser.add_argument('-a', type=str, default="./activitydata.csv", 
		help='Specifiy relative file path to activity data file')
	parser.add_argument('-s', type=str, default="./raw_sleepdata.csv", 
		help='Specifiy relative file path to raw sleep data file')
	parser.add_argument('-c', type=str, default="./canvasdata.csv", 
		help='Specifiy relative file path to canvas data file')
	parser.add_argument('-m', type=str, default="./mergeddf.csv", 
		help='Specify where to save merged dataset')
	args = parser.parse_args()
	return args


if __name__ == '__main__':
	args = parse_arguments()
	gen_merged_df(args.a, args.s, args.c, args.m)
	print("Merged dataset created!")


