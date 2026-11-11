# hash comments out entire line
''' 
comment with 3 
quotes (double or single) 
to comment out a block of code

Vector in R is like list in Python (things inside them referred to by position)
List in R is like dictionary in Python (things inside them referred to by name)

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


