# Assignment Tidyverse Basics

### Problem Statement
This assignment will focus on basic functions of R with an emphasis on
`tidyverse` implementations. `tidyverse` is a collection of packages, pioneered
by Hadley Wickham and RStudio, that looks to standardize procedures,
functionality, and syntax in R.

To gain familiarity with R, we will be working with a microarray dataset that
contains gene probe expression data for various samples collected from cancer
patients. In bioinformatics, it is common to have multiple datasets for your
different modes of data (i.e. microarray expression data is kept separate
from clinical data detailing the samples). You will get an opportunity to work
with both of these datasets, and be required to cross reference between the two.

### Required Readings

Please read sections 6.3 through 6.10 in the textbook. Section 6.3 starts [here](https://bu-bioinfo.github.io/r-for-biological-sciences/data-wrangling.html#importing-data)

### Learning Objectives and Skill List
- Install various packages needed for analysis
- Load Data
- Gain familiarity with common `tidyverse` operations such as `groupby()`,
`mutate`, and `summarize`.
- Create a small plot to display results
- Utilize R Markdown to create an attractive format for sharing data.

### Instructions
Our main focus for this assignment are installing packages, manipulating data,
and summarizing important statistics across both samples and features (genes).

Please accept the GitHub classroom link provided on blackboard for this
assignment.

The project is laid out as such:  
```
main.R
report.Rmd
reference_report.html
test_main.R
```
Each step of the assignment is explained in the R markdown file, `report.Rmd`.
There you will find a list of tasks to explicity implement functions in your
empty `main.R` script. The `main.R` script contains skeletons of each function
you'll need to implement, explaining what each function should do, the parameters
it expects to receive, and what type of output is expected to be returned. A
reference report, `reference_report.html` is also provided. Assuming you successfully
implement all the functions in `main.R`, your generated report should look
identical to the information displayed in `reference_report.html`. In this way,
you can use `reference_report.html` as a guide to determine if you are correctly
implementing your functions.

Here is the suggested workflow for developing and checking your code in this
assignment:

1. `main.R` contains function definitions, including signature descriptions, for
  a number of functions, but the bodies of those functions are currently blank
2. `report.Rmd` has code chunks that call functions defined in `main.R` - you do
  not need to write anything in the Rmd file (but you may)
3. Your task is to read the function descriptions and the text in the Rmarkdown
  document and fill in the function bodies to produce the desired behavior in
  main.R
4. You can test your work by executing individual code chunks in report.Rmd and
  comparing your output to the example compiled report in the repo
5. In the workflow, you will go back and forth between developing code in `main.R`
  and running code chunks in report.Rmd
6. In addition to inspecting your report results, also run
  `testthat::test_file('test_main.R')` to ensure they work correctly.
7. When you have developed function bodies for all the functions and executed
  all the code chunks in the report successfully, you should be able to knit the
  entire report

### Hints

* When developing the `period_to_underscore()` function, you might find the
[`stringr::str_replace_all()`](https://stringr.tidyverse.org/reference/str_replace.html)
function helpful. The `pattern` argument to these functions is interpreted as
a regular expression or "regex" for short. A regular expression is a sequence of 
characters (i.e. a string) written in a language that describes patterns in text, 
similar to "Find and Replace" operations in word processing software, but is more 
powerful and flexible in the kinds of patterns it can detect. Some characters have 
special meaning in regular expressions, one of which is the `.` character. In order
to identify the literal period character like we are trying to do, we must instruct 
the regular expression to do so by either escaping the character with `\\.` or place 
it in a range with `[.]`. Either of these two methods will work to replace a literal `.`
with `_`. See the section on regular expressions in the course textbook for more 
information. 

* If you are getting **type conversion** errors when loading in your expression
CSV file, check to make sure you aren't supplying your own column names. The
CSV file has column headers already, and supplying your own will cause the first
line to be read in as data. Since the first row are character values in this
case, all of the other values in the columns will be coerced to characters as
well, instead of reading them in as numbers. Always get in the habit of 
assuming that your CSVs may not be perfectly formatted and quickly checking its
structure using a command like `cat` or `head`.
