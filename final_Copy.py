# github.com/colingoodman

import subprocess
import os
import csv
import pandas
from datetime import datetime

# designate working directory
#directory = r"C:\Users\Colin\Desktop\workspace"
#os.chdir(directory)

# US Census API key
api_key = "c53a5080701d109ad2d52d51b482a50be585c6b6"

# obtain directions from users via CLI
# determines working directory and what methods need to be executed
def init_input():
  #print('Directory: ', directory, '\n')

  print('1 - Pull census data\n2 - Transform census data\n3 - All of the above\nSelect option: ')
  user_command = input()
  states = []
  geography = ''
  year = ''

  if(user_command == '3'):
    print('All of the above selected.')
    print('Enter state(s): ')
    states = input().split(' ')
    print('Enter geography: county, tract, or block')
    geography = input()
    print('Enter year: 2016 to most recent American Community Survey year')
    year = input()
  elif(user_command == '1' or user_command == '3'):
    print('Pull census data selected.')
    print('Enter state(s): ')
    states = input().split(' ')
    print('Enter geography: county, tract, or block')
    geography = input()
    print('Enter year: 2016 to most recent American Community Survey year')
    year = input()
  elif(user_command == '2'):
    print('Transform census data selected.')
    print('Enter file(s): ')
    states = input().split(' ')
  else:
    print('invalid input')
    exit()

  return states, user_command, geography, year


# executes R census pull script
def r_script(states_r, geography, year):
  print('Executing R script. This may take a while!\nOutput will be logged.')

  command = []
  command.append('Rscript')
  command.append('source_Copy.R')
  command.append(geography)
  command.append(year)
  #command = ['Rscript', 'source.R', geography] # R needs to be installed

  print(states_r)

  for state in states_r:
    command.append(state)

  print(command)
  
  # cmd = ['Rscript', 'source.R', geography, states_r] # deprecated
  x = subprocess.check_output(command, universal_newlines = True)

  # due to how this subprocess thing works, at least 7 arguments are passed into R
  # the R script is equipped to handle this properly.
  # example: /bin/exec/R --slave --no-restore --file=source.R --args county Kansas

  # save log on r script to a text file
  #log_title = str(datetime.now()) + '.txt'
  log_title = 'temp.txt'
  log = open(log_title,'w')
  log.write(x)
  log.close()
  print('Census pull log saved to ', log_title)

  return 0 # r script saves file as "State.csv"


# creates a data table from a state table (from a file)
# e.g. "wyoming.csv" -> processed 2d array
# returns a list of lists and a list of dimensions
def read_state_file(file_name):
  new_table = [] # the resulting table; what this method returns
  dim_given = [] # list of dimensions that are used

  # temp is a list of all the dimensions we may need
  temp = ['B01001e26', 'B01001e27', 'B01001e3', 'B01002e1', 'B01003e1', 'B02001e3', 'B02001e4', 'B02001e5',
                'B02001e6', 'B02001e7', 'B02001e8', 'B03003e3', 'B08134e1', 'B08134e61', 'B09002e1', 'B09002e15',
                'B09002e3', 'B09002e4', 'B09002e5', 'B09002e6', 'B09002e7', 'B09019e38', 'B09020e1', 'B09020e21',
                'B15003e1', 'B15003e16', 'B15003e15','B15003e14', 'B15003e13', 'B15003e12', 'B15003e11','B15003e10',
                'B15003e9', 'B15003e8', 'B15003e7', 'B15003e6', 'B15003e5', 'B15003e4', 'B15003e3', 'B15003e2',
                'B17021e1',
                'B17021e2', 'B19001e14', 'B19001e15', 'B19001e16', 'B19001e17', 'B19055e1', 'B19055e2', 'B19301e1',
                'B22010e2', 'B23025e1', 'B23025e2', 'B23025e5', 'B25001e1', 'B25002e1', 'B25002e3', 'B25007e1',
                'B25008e1', 'B25010e1', 'B25033e1', 'B25033e12', 'B25033e6', 'B25044e10', 'B25044e3', 'B25056e1',
                'B25064e1', 'B25075e1', 'B25075e10', 'B25075e11', 'B25075e12', 'B25075e13', 'B25075e14', 'B25075e15',
                'B25075e16', 'B25075e17', 'B25075e18', 'B25075e19', 'B25075e2', 'B25075e20', 'B25075e21', 'B25075e22',
                'B25075e23', 'B25075e24', 'B25075e25', 'B25075e3', 'B25075e4', 'B25075e5', 'B25075e6', 'B25075e7',
                'B25075e8', 'B25075e9', 'B25077e1', 'C16002e1', 'C16002e10', 'C16002e13', 'C16002e4', 'C16002e7',
                'C23023e1', 'C23023e14', 'C23023e3', 'C240101e52', 'C240101e56', 'C24010e1', 'C24010e16', 'C24010e19',
                'C24010e20', 'C24010e30', 'C24010e34', 'C24010e38', 'C24010e5', 'C24010e66', 'C24010e70', 'C45010e55',
                'C24010e55',]

  with open(file_name, encoding='utf-8') as csvfile: # opens the input file
    csvreader = csv.reader(csvfile)
    fields = next(csvreader)

    for row in csvreader: # each row is data for 1 dimension/variable on 1 geographic area
      variable = row[3] # get variable name
      variable = variable.replace('_00','e') # cleaning up variable name for use
      variable = variable.replace('_0','e')

      # this block tracks what dimensions are used
      exists = False
      for i in temp: # determines if this row is needed
        if variable == i:
          exists = True
          if i not in dim_given:
            dim_given.append(i)
      if exists == False:
        continue

      # create a new row for this location (with all its dimensions), append it to the table
      geo = row[1]
      location = row[2]
      estimate = row[4]
      moe = row[5]

      new_row = [geo, location, variable, estimate, moe]
      new_table.append(new_row)

  return new_table, dim_given


# produces table w/ columns by dimension, 1 row per block group
# e.g. processed 2d array -> reduced data table
# method saves a CSV file '[state]_reduced.csv'
def block_group_merge(input_table, dim_given, file_name):
  output_file_name = file_name.replace('.csv','_Reduced.csv')
  new_dict = {}  # empty dictionary

  # duplicates appear in input_table, need to remedy
  tmp = pandas.DataFrame(input_table)
  tmp = tmp.drop_duplicates(keep='first')

  for index, row in tmp.iterrows():
    print(row)
    geoid = row[0]
    new_tuple = (row[2], row[3])

    if geoid in new_dict:
      new_dict[geoid].append(new_tuple) # add tuple to existing list of tuples
    else:
      new_dict[geoid] = [new_tuple]  # produce new list with our new tuple inside

  new_table = [['location']]
  for x in dim_given:
    new_table[0].append(x)

  for entry_list in new_dict: # for each list (value) in this dictionary
    new_row = [entry_list]
    for item in new_dict[entry_list]: # for each tuple in this list of tuples
      new_row.append(item[1])
    new_table.append(new_row)

  # output file
  with open(output_file_name, "w+") as my_csv:
    csvWriter = csv.writer(my_csv, delimiter=',')
    csvWriter.writerows(new_table)
    my_csv.close()

  return output_file_name


def normalize_fields(key, row):
  new_row = [row[0]] # start with GEOID

  for variable in key: # conducts every needed operation, adds to the row being made
    variable_name = variable[0]

    if len(variable) > 2: # the complexity in here is due to operations with varying #s of input variables
      index = 1
      function = 0

      while index < len(variable) - 1:
        function += row[variable[index]]
        index += 1

      denom = row[variable[len(variable) - 1]]
      if denom != 0:
        result = function / denom
      else:
        result = 0 # double check this, not sure if denom should ever be 0
        print('Potential issue: normalization variable (', variable[len(variable) - 1], 'on', row[0], ') is 0.')

    else: # some "operations" require no math; they are 1 variable (e.g. median age)
      result = row[variable[1]]

    new_row.append(result)

  return new_row


# This function takes a reduced data table (CSV) and returns a final table
# return is True if successful; method saves a CSV file
def build_final(target_file_name):
  input_file = pandas.read_csv(target_file_name) # grab target file
  output_file_name = target_file_name.replace('_Reduced.csv','_Final_Table.csv') # reformat file name

  # obtains and formats the operation key
  key = [] # each entry in key is an operation to be applied (e.g. % in poverty = # in poverty / total pop.)
  with open('Key.csv', encoding='utf-8') as csvfile:
    csvreader = csv.reader(csvfile)
    for row in csvreader:
      row[0] = row[0].replace('\ufeff','')
      new_list = []
      for i in row:
        if i != '': # if we haven't reached the end of the row
          new_list.append(i)
      key.append(new_list)

  new_table = []
  # iterates thru entire input file, builds new table row by row
  for index, row in input_file.iterrows():
    new_table.append(normalize_fields(key, row))

  # header for new table
  header = ['GEOID']
  for column_header in key:
    header.append(column_header[0])

  # convert dataframe to csv and output to file
  df = pandas.DataFrame.from_records(new_table)
  df.columns = header
  df.to_csv(output_file_name, index = False)

  return(True)
# end of new methods

# ####### Road Map
# 1: Pulled Census data, Wyoming.csv
# 2: Processed 2d array
# 3: Reduced csv file
# 4: Final csv file

# return states, user_command, geography

state_targets, command, year, geographic_area = init_input()

if(command == '1' or command == '3'):
  print('Obtaining census data')
  r_script(state_targets, geographic_area, year)
if(command == '2' or command == '3'):
  print('formatting tables')
  new = []
  for s in state_targets:
    new.append(s + '.csv')

  for s in new:
    a, b = read_state_file(s)
    o = block_group_merge(a, b, s)
    build_final(o)

print('# end of script')
# end of file
