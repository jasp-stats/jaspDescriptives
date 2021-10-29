Descriptives
===

Descriptives allows the user to obtain basic descriptive statistics, histograms and density plots, correlation plots, boxplots, and frequency tables.

### Input
-------

#### Assignment Box 
- Variables: All variables of interest.
- Split: Can be split by a categorical variable such as experimental condition.
- Tranpose descriptive table: Tranposes the main table
- Frequency tables: Displays a frequency table for each variable.
  - Maximum distinct values: Frequency tables are shown for each variables that has fewer distinct observations than the specified number.
- Stem and Leaf tables: A table that shows all numeric observation from small to large. The observations are split into a "stem", the first digit(s), and a "leaf", the subsequent digit.
  - scale: Controls the table length. Increasing the scale leads to a coarser table whereas decreasing the scale leads to a smoother table.

### Plots
- Distribution Plots: For continuous variables, displays a histogram. For nominal and ordinal variables, displays a frequency distribution.
  - Display density (only for continuous variables): Displays a density based on a nonparametric density estimator.
- Correlation Plot: For continuous variables, displays histograms, density plots, and scatterplots.
- Boxplots: For continuous variables, displays a boxplot.
  - Label outliers: Optionally, the outliers are labelled. Outliers are based on the interquartile range (IQR), i.e., [25th percentile] - 1.5 × IQR and [75th percentile] + 1.5 × IQR.
  - Color: Displays in color.
    - Has selectable boxplot, violin, and jitter elements for displaying the distribution of the data.

### Statistics
- Central Tendency (only for continuous variables):
  - Mode: Mode of the data points; if more than one mode exists, only the first is reported.
  - Median: Median of the data points.
  - Mean: Arithmetic mean of the data points
- Percentile Values:
  - Quartiles: Displays the 25th, 50th, and 75th percentiles of the data points.
  - Cut points for x equal groups: Displays the cut points that divide the data into x equal groups; default is 4 equal groups.
  - Percentiles: Displays the xth percentile; percentile values must be separated by comma.
- Dispersion (only for continuous variables):
  - S.E. Mean: Standard error of the mean.
  - Std. deviation: Standard deviation of the data points.
  - Coefficient of Variation: The coefficient of variation gives us the relative dispersion of the data, in contrast to the standard deviation, which gives the absolute dispersion. For this purpose, the standard deviation is divided by the mean value, so that the unit is truncated away.
  - MAD: Median absolute deviation of the data points.
  - MAD Robust: Median absolute deviation of the data points, adjusted by a factor for asymptotically normal consistency.
  - IQR: Interquartile range of the data points; 75th percentile - 25th percentile.
  - Variance: Variance of the data points.
  - Range: Range of the data points; maximum - minimum.
  - Minimum: Minimum value of the data points.
  - Maximum: Maximum value of the data points. 
- Distribution:
  - Skewness: Skewness of the distribution of the data points.
  - Kurtosis: Kurtosis of the distribution of the data points.
  - Shapiro-Wilk test
  - Sum: Sum of the data points.

### Output
-------
#### Descriptive Statistics
- Valid: Number of valid cases.
- Missing: Number of missing values.
- Mode: Mode of the data points.
- Median: Median of the data points.
- Mean: Arithmetic mean of the data points.
- Std. Error of Mean: Standard error of the mean.
- Std. Deviation: Standard deviation of the data points.
- MAD: Median absolute deviation
- IQR: Interquartile range
- Variance: Variance of the data points.
- Skewness: Skewness of the distribution of the data points.
- Std. Error of Skewness: Standard error of the skewness.
- Kurtosis: Kurtosis of the distribution of the data points.
- Std. Error of Kurtosis: Standard error of kurtosis.
- Shapiro-Wilk: Value of the Shapiro-Wilk statistic.
- P-value of Shapiro-Wilk: p-value of Shapiro-Wilk statistic.
- Range: Range of the data points.
- Minimum: Minimum value of the data points.
- Maximum: Maximum value of the data points.
- Quartiles: 25th, 50th, and 75th percentiles of the data points.
- Cut points for x equal groups: Cut points that divide the data into x equal groups.
- Percentiles: Displays the xth percentiles.
- Sum: Sum of the data points.

#### Distribution Plots
- For continuous variables, displays a histogram and the fit of a nonparametric density estimator.
- For nominal and ordinal variables, displays a frequency distribution.

#### Correlation Plot
- Displays a matrix of plots between continuous variables, with scatterplots between the variables in the off-diagonal entries, and histograms and density plots in the diagonal entries.
 The line represents the fit of a 1st, 2nd, 3rd, or 4th order polynomial (the selection is based on the Bayesian information criterion; Schwarz, 1978).

#### Boxplots
- For continuous variables, displays a boxplot. Optionally, the outliers are labelled. Outliers are based on the interquartile range (IQR), i.e., [25th percentile] - 1.5 × IQR and [75th percentile] + 1.5 × IQR. Can also display in color, and has selectable boxplot, violin, and jitter elements for displaying the distribution of the data. This can be split by a categorical variable such as experimental condition.

#### Frequency Tables (nominal and ordinal variables)
- Displays a frequency table for each variable.
  - Frequency: Frequency of occurrence of each value.
  - Percent: Percentage of occurrence of each value.
  - Valid Percent: Valid percentage of occurrence of each value.
  - Cumulative Percent: Cumulative percentage.

#### Stem and Leaf tables
- Displays the spread of a variable.
  - Stem: the first digit(s).
  - Leaf: the first digit after the stem.

### References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics, 6*, 461-464.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

### R Packages
---
- ggplot2
- ggrepel
- grid
- stats
- stringr

