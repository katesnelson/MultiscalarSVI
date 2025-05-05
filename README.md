


To run the python - R interactive script

1. Go to the folder/directory holding the python and R scripts
2. Hold Shift + right-clisk and select "Open PowerShell window here"
3. Type python final_Copy.py to start running the python script.
3. Enter responses to the prompts
	a) if you want to pull information for all states type 'ALL', for state by state data you can type in the full name 
	or state abbreviation, for multiple states separate state names/abbreviation with a space
	b) for census tract data type 'tract' for block group data type 'block group'
	c) To just transform data enter the name of the file without the file ending
	
To modify the data used for SVI construction

1. Add your census api key to the source.R script under ##KEY###.
2. If adding any new variables, add the required census variable IDs to the Key.csv and to the temp list in final.py line 92
3. If the new variables draw from a new census table add the table ID to source.R line 43
4. If adding new variables, add them to the col_constr.csv file, including how any normalization or arthimetic needs to be computed.



If you encounter an error like this 
"UnicodeDecodeError: 'utf-8' codec can't decode byte 0xf1 in position 5574: invalid continuation byte"
you probably have a census table with an encoding error. Find the offending state file and use Notepad++ to
convert the encoding to utf-8. Then reurn the table transformation part of the script.