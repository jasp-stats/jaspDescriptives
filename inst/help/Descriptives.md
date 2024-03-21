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
- Pareto plots: Can only be displayed for nominal and ordinal variables. Displays an ordered descending frequency distribution. By default, a cumulative line is added indicating the proportional contribution of each factor/level within the variable.
  - Parteo rule: Draws two additonal lines, one vertically and one horizontally, that intersect and stop at the height of the input number (scales with cumulative line). By default, it is set to 95%. This can be changed into the desired percentage.
- Likert plots: Can only be displayed for nominal and ordinal variables. Displays a horizontally stacked bar plot, generating a new bar for every additional variable added.
  - Assume all variables share the same levels: Displays one plot containing all variables. Variables require the same number of levels.
  - Adjustable font size for vertical axis: Controls the font size of variable names on the vertical axis. By default, the normal font size is used, three more sizes are available.
- Density plots: Can only be displayed for scale (continuous) variables. Uses the kernel density estimation to visualize the distribution of a numeric variable. 
  - Separate densities: By placing a nominal or ordinal variable in this box, different distributions corresponding to the different levels of the variable will be displayed.
  - Transparency: Adjustable transparency for color within area below the density line (ranges from 0 to 100).

### Statistics
- Central Tendency (only for continuous variables):
  - Mode: Mode of the data points; if more than one mode exists, only the first is reported. For nominal and ordinal data, the mode is the most frequent observed value. For continuous data, the mode is the value with highest density estimate (see 'Distribution Plots' -> 'Display density'). If a footnote about multimodality for continuous variables is reported, we recommend visualizing the data to check for multimodality.
  - Median: Median of the data points.
  - Mean: Arithmetic mean of the data points
- Percentile Values:
  - Quartiles: Displays the 25th, 50th, and 75th percentiles of the data points.
  - Cut points for x equal groups: Displays the cut points that divide the data into x equal groups; default is 4 equal groups.
  - Percentiles: Displays the xth percentile; percentile values must be separated by comma.
- Dispersion (only for continuous variables):
  - Std. deviation: Standard deviation of the data points.
  - Coefficient of variation: The Coefficient of variation gives us the relative dispersion of the data, in contrast to the standard deviation, which gives the absolute dispersion. For this purpose, the standard deviation is divided by the mean value, so that the unit is truncated away.
  - MAD: Median absolute deviation of the data points.
  - MAD robust: Median absolute deviation of the data points, adjusted by a factor for asymptotically normal consistency.
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
- Inference:
  - S.E. mean: Standard error of the mean.
  - Confidence interval for the mean:
    - Width: width of the confidence interval.
    - Method: How should the confidence interval be computed? By default, we use a `T model`, which yields results identical to a one-sample t-test. Alternative options are a normal model (\\\\(\\bar{x} \\pm z_{95}\\times SE\\\\)), or `Bootstrap`.
  - Confidence interval for the std. deviation: a confidence interval for the standard deviation based on bootstrap samples.
  - Confidence interval for the variance: a confidence interval for the variance based on bootstrap samples.
  - Bootstrap samples: the number of bootstrap samples to be used.
- Association matrix:
  - Covariance: Covariance value.
  - Correlation: Pearson's correlation coefficient.
  - Use: How to deal with missing values? 
    - Everything: use all observations, resulting in NA when there are missing values.
    - Complete observations: missing values are handled by casewise deletion (i.e., only use rows of the data set that are complete).
    - Pairwise complete observations: use all complete pairs of observations on those variables. This can result in covariance or correlation matrices which are not positive semi-definite.


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

#### Pareto plots
- Displays the counts of each factor/level within the variable in a descending order. The y-axis represents the frequency (counts as grey bars) of each factor/level, the x-axis represents the factors/levels of the variable in an ordered sequence.
- By default, a cumulative line is drawn indicating the proportional contribution of each factor. A second vertical axis to the right side of the graph scales with this cumulative line and represents percentages to enable the description of the cumulative line.
- If "Pareto rule" is enabled, the two new lines enable a more precise assessment of factor/level contribution to the overall contribution (in percent) by using different input numbers.

#### Likert plots
- Displays a horizontally stacked bar chart showing the contribution of levels within a variable in percent. Order of levels depends on defined order in the JASP data table. A legend below the graph provides an overview of levels and their respective colors in the graph.
  - The y-axis represents the variables used, the x-axis represents the percentages. Percentage contribution of all lower-order (below the middle level) and higher-order (above the middle level) levels are displayed on their respective side of the graph.
  - The graph displays percentages on the x-axis as positive in both directions. Reason for the chosen display (in two directions) is the graphs usefulness in survey research where levels often follow a Likert based order (e.g., high - low, likely - unlikely, agreement - disagreement). Therefore, the graph contains a split between levels at their median.
  - The number of variable levels determines the number of layers displayed. Layers represent the percentage distribution of the levels of the variable under investigation.
  - If the variables contain an uneven amount of levels, the middle level is displayed as a grey block in the middle of the stacked bar with its percentage contribution on top.
- Available font sizes: normal, small, medium, large.

#### Frequency plots
- Displays the distribution of a numeric variable by either a density or histogram plot. 
	- Density: The y-axis represents the probability density for the kernel density estimation (probability per unit on the x-axis).
	- Histogram: The y-axis represents the frequency
		- When separate frequencies are specified, the bars can be combined in various ways: Stack (stacked), Identify (behind each other), Dodge (next to each other). 
- The x-axis represents the variables used.
- Appearance can be manipulated by adjusting color palette and transparency.

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

