# Gender paygap project

This repository contains our project files for the course 'Survey Data Analysis' at Utrecht University. The analysis and code can be found in the [R markdown file](https://github.com/JasperHG90/SDA/blob/master/report_SDA_GPG.Rmd). 

We separated data preprocessing, dependency management and utility functions from the main analysis file and stored them in their own R files. These files are called in the Rmd file by using the `source()` function. For more information, see:

1. [install_dependencies.R](https://github.com/JasperHG90/SDA/blob/master/R/utilities/install_dependencies.R) for the R file that installs missing dependencies to the user's directory.
2. [preprocess_data.R](https://github.com/JasperHG90/SDA/blob/master/R/utilities/preprocess_data.R) for the R file that takes the [original dataset](https://github.com/JasperHG90/SDA/blob/master/data/gender_pay_gap.Rds) and preprocesses it such that it is ready to be used for analysis. This process will only be called **once** if the preprocessed data does not yet exist in the [data](https://github.com/JasperHG90/SDA/tree/master/data) folder.
3. [functions.R](https://github.com/JasperHG90/SDA/blob/master/R/utilities/functions.R) for the R file that defines utility functions (`stratify()`, `neyman()` and `sample_size()`) and their documentation.

The accompanying presentation for this project can be found [here](https://docs.google.com/presentation/d/13akRqr7Rc_xBN7ihqFFdrieKUvlg8HwmQgiHGjH_pWU/edit?usp=sharing)

This repository contains project files for the Gender Paygap project for the course 'Survey Data Analysis' at Utrecht University.

It contains the following files

```text
+--- .gitignore
+--- assignment
      +---
+--- data
      +---
+--- documentation
      +---
+--- literature 
      +---
+--- practical.Rproj
+--- questions
+--- R
      +---
+--- README.md
```

## Making sense of the forward-operating pipe ('%>%'): ----

The forward-operating pipe is a method of coding that allows you to 'pipe' results from one command to the next.

Instead of doing something like this:

```r
library(dplyr)
mean(sample(1:1000, 5))
```

Where we take the mean of a random sample of numbers ranging from 1:1000, we can do this:

```r
sample(1:1000, 5) %>% 
  mean()
```

Which looks a LOT cleaner.  If you want to see what the intermediate output looks like (e.g. the results of the above after the sample() command), you can just comment the pipe out, like this:

```r
sample(1:1000, 5) #%>%
mean()

```

## Getting started

Clone this repository and ...

## Main analysis

Can be found here ...

