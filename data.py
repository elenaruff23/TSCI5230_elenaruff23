# hash comments out entire line
''' 
comment with 3 
quotes (double or single) 
to comment out a block of code

Vector in R is like list in Python (things inside them referred to by position)
List in R is like dictionary in Python (things inside them referred to by name)


Run these from the R console, NOT directly from this .py script
library(reticulate)
py_require(c('numpy','matplotlib','scikit-learn','pandas'))
repl_python()

'''

import numpy as np
import math 
import pandas as pd
from pathlib import Path
import matplotlib.pyplot as plt

data_location = Path("/Users/elenaruff/Downloads/archive/")
output_location = Path("palpatir_output")
reference_year = 2025
trend_start_year = 2023

if not data_location.is_dir():
  raise FileNotFoundError("set data location to archive folder")

#.glob method lists all files that match the wild card syntax
dat = {csv_file.name:pd.read_csv(csv_file) for csv_file in data_location.glob("*.csv")}
type(dat) #should be dict because of {} format of dat = variable
dat.keys() #similar to names(dat) in R, but .keys() is specific to dict in Python
dat["patients.csv"].dtypes #check variable type for all columns 
test = dat["patients.csv"].assign(BIRTHDATE=pd.to_datetime(dat["patients.csv"]["BIRTHDATE"]))


#11.18.26 in class lists and dictionaries and related .methods and function examples 
#functions stand alone, .method is bound to a particular object and type available will depend on the type of object
#Python lists do not change with (+) like in R, original list stays intact
my_list = [1,2,3,4]
my_list_2 = ["one","two","three","four"]
my_list_3 = my_list + my_list_2
my_list_3
#use.extend([] or another defined list) to change the original
my_list_3.extend(["apple","blueberry"])
my_list_3

#to sort a list use sorted() function, but cannot combine int and str within the list
#sorted(my_list_2)
#or list.sort()
#my_list_2.sort()
#optional argument (key = str) to force it to sort as if all values are strings 
my_list_3.sort(key = str)
#optional argument (reverse = True) to reverse the sorting 

#len() for length of list 
len(my_list_3)

#list.insert(location, value to be inserted at location)
my_list_3.insert(0,0)
my_list_3 

#list.remove(value) removes first instance of that value 
my_list_3.remove(0)
my_list_3

#list.index(value) to find location of a value in the list 
my_list_3.index("apple")

#list.count() to count num of instances of a value 
my_list_3.count("apple")

#list comprehension is python version of vector arithmetic in R (not modification of original my_list)
[xx for xx in my_list]
[xx + 1 for xx in my_list]
[xx*2 for xx in my_list]
["a" for xx in my_list]
[[xx,xx+1] for xx in my_list]

#filtering lists 
[xx for xx in my_list_3 if isinstance(xx, int)]
[xx for xx in my_list_3 if isinstance(xx, str)]
[xx for xx in my_list_3 if isinstance(xx, int) and xx >2]

#Dictionaries 
{"one":1,"two":2, "three":3, "four":4}
dict(one = 1, two = 2, three = 3, four = 4)
zip(my_list_2,my_list)
my_dictionary = dict(zip(my_list_2,my_list))
my_dictionary

#forcing values from dictionary back into a list 
[xx for xx in my_dictionary.values()]

#other methods for returning dictionary keys and all items 
[xx for xx in my_dictionary.keys()]
my_dictionary.items()

#Dictionary comprehension
{xx:yy for xx,yy in my_dictionary.items()}
{xx:yy for xx,yy in my_dictionary.items() if xx in ["one","four"]}
{xx:yy for xx,yy in my_dictionary.items() if xx.startswith("t")}
{xx:yy for xx,yy in my_dictionary.items() if xx.startswith("t") and yy > 2}
{xx.upper():yy for xx,yy in my_dictionary.items() if xx.startswith("t") and yy > 2}

#transforming into numbered dictionary for character values 
{yy:xx for xx,yy in my_dictionary.items()}

#flatten dictionary into list of vectors 
[f"{xx} is {yy}" for xx,yy in my_dictionary.items() if xx in ["one","four"]]

#{} output gives dictionary, while [] output gives you a list 

#Homework: use the .assign() method to convert all date columns to actual dates using pd.to_datetime()

