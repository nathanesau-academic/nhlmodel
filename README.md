# nhlmodel

This project aims to explain the number of points an NHL team gets using linear
regression.

# Description of files

* Report: contains *report.pdf*
* Slides: contains presentation slides *slides.pdf*
* R: contains R code using in analysis
* Data: contains data used in analysis. See data sources below.

# Data sources

* Salary data is from [nhl numbers](http://stats.nhlnumbers.com)
* Team stats are from [hockey reference](http://hockey-reference.com)

# Instructions

1. Run the webscrapers.

	* R/get_data.R
	* R/get_new_data.R
	* R/get_salary_data.R

2. Fit the models.

	* R/fit_models.R

3. Predict on new data

	* R/predict_data.R

You will need to change the setwd() command at the top of each R folder.
