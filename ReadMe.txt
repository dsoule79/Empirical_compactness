Empirical compactness
This repository provides the code to calculate the geographically adjusted compactness of congressional districts ( see https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5278125 for details).

The code assumes that you have loaded census data into the Census data folder for the congressional sessions of interest and for the US border and for US counties.  These can be sourced from:
108 congress 2003 – 2004: https://www2.census.gov/geo/tiger/TIGER2010/CD/108/
113 congress 2013 – 2014: https://www2.census.gov/geo/tiger/TIGER2013/CD/
118 congress 2023 – 2024: https://www2.census.gov/geo/tiger/TIGER2023/CD/
Counties 2023: https://www2.census.gov/geo/tiger/TIGER2023/COUNTY/
Boundary files 2023:
https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2019.html#list-tab-1883739534
The programs CDxxxdata.ipynb are Jupyter based python codes that reads the respective census file, calculates the  Polsby Popper compactness score of each county and outputs in a common format this data and some descriptive statistics for each district.  The output files are placed in the Process data folder.
The data compile Rcode reads each of the three congressional process data files, combines then into one master data file and calculates 21 descriptive statistics for each district.
The Beta Regression Rcode performs a regression on the compactness data as a function of the district statistics previously calculated and outputs a model for compactness that can be used to calculate the relative compactness percentile for each district after adjusting for the state’s geography.
The Outlier detection Rcode performs the two outlier tests described in the paper and provides various geographic plots.


