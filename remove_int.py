import csv
import re


filename = "tracts.csv"

rows = []
new = []

with open(filename, 'r') as csvfile:
    csvreader = csv.reader(csvfile)
    fields = next(csvreader)
    
    for row in csvreader:
        rows.append(row)

for row in rows:
    row = re.sub("[^0-9]", "", str(row[2]))
    new.append(int(row))

new = list(dict.fromkeys(new))
new.sort()
file = open("Tract_Results.txt", "a")
for entry in new:
    entry = str(entry)
    entry = 'Census Tract ' + entry
    # print(entry)
    file.writelines(entry + "\n")

file.close()
