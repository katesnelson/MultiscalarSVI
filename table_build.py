# Colin Goodman 2019

import csv
import pandas

# Build directory of needed columns
def build_need():
    col_need = [] # list of needed columns

    with open('col_constr.csv', encoding='utf-8') as csvfile:
        csvreader = csv.reader(csvfile)
        fields = next(csvreader)

        for row in csvreader:
            item = row[2]
            item = item.replace('+', '')
            res = item.split()

            if len(res) > 1:
                for col in res:
                    new_tuple = (col, row[1])
                    col_need.append(new_tuple)
            else:
                new_tuple = (res, row[1])
                col_need.append(new_tuple)

    return col_need


# for given tuple, find full col name in cbg_meta
def grab_meta_name(entry):
    file = 'cbg_meta.csv'

    with open(file, encoding='utf-8') as item:
        csv_reader = csv.reader(item)

        for row in csv_reader:
            if row[0] == entry[0]:  # if codes match, set meta field name into tuple
                entry = [entry[0], entry[1], entry[2], row[1]]

    return entry


# Build directory of source columns
def build_source(col_need):
    col_have = []
    files = ['tables/cbg_b00.csv', 'tables/cbg_b01.csv', 'tables/cbg_b02.csv', 'tables/cbg_b03.csv', 'tables/cbg_b07.csv',
            'tables/cbg_b08.csv', 'tables/cbg_b09.csv', 'tables/cbg_b11.csv', 'tables/cbg_b12.csv', 'tables/cbg_b14.csv',
            'tables/cbg_b15.csv', 'tables/cbg_b19.csv', 'tables/cbg_b20.csv', 'tables/cbg_b21.csv', 'tables/cbg_b22.csv',
            'tables/cbg_b23.csv', 'tables/cbg_b25.csv', 'tables/cbg_b27.csv', 'tables/cbg_b99.csv', 'tables/cbg_c17.csv',
            'tables/cbg_c24.csv', 'tables/cbg_c16.csv']

    for file in files:
        with open(file, encoding='utf-8') as item:
            csv_reader = csv.reader(item)
            header = next(csv_reader)

        for i in header:
            for j in col_need:
                if i == j[0]:
                    new_tuple = (j[0], j[1], file, "meta_field_name") # kvp - column code, column name, file where found
                    new_tuple = grab_meta_name(new_tuple)
                    col_have.append(new_tuple)

    return col_have


# Check that we grabbed everything:
def check():
    print('We found the following columns: ')
    for i in col_need:
        for j in col_have:
            if i == j:
                print(j)
                col_need.remove(j)

    print('We did not find the following columns: ')
    for i in col_need:
        print(i)

    return 0


# Assemble CSV of desired columns
def build_csv():
    # col_have is (code, name, file)

    df = pandas.read_csv('tables/cbg_b00.csv')
    output = df.census_block_group.to_frame()
    output.to_csv('table_variables.csv')


def build_csv_complete(col_have):
    df = pandas.read_csv('tables/cbg_b00.csv')
    output = df.census_block_group.to_frame()
    output.to_csv('table_variables.csv')

    for col in col_have:
        full_df = pandas.read_csv(col[2])
        output.join(full_df[col[0]].to_frame())
        output[output.shape[1], 0] = col[1]

    output.to_csv('table_variables.csv')


def csv_append(col_, NUM):
    output = pandas.read_csv('table_variables.csv')

    full_df = pandas.read_csv(col_[NUM][2])
    target = col_[NUM][0]
    column = full_df[target].tolist()
    # print(column)
    output[col_[NUM][1]] = column

    output.to_csv('table_variables.csv')


def multi_append(col_):
    index = 0
    for col in col_:
        csv_append(col, index)
        index += 1


# Run methods

col_need = build_need()
col_have = build_source(col_need)
build_csv()
multi_append(col_have)
#csv_append(col_have)

# end
