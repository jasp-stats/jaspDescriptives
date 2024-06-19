//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

// All Analysis forms must be built with the From QML item
Form
{
	info: qsTr("Descriptives allows the user to obtain basic descriptive statistics, histograms and density plots, correlation plots, boxplots, and frequency tables.")
	infoBottom: "## " + qsTr("References") + "\n"
				+	"- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company." + "\n"
				+	"- Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics, 6*, 461-464." + "\n"
				+	"- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers." + "\n"
				+ "\n---\n"
				+ "## " + qsTr("R Packages") + "\n"
				+	"- ggplot2\n"
				+	"- ggrepel\n"
				+	"- grid\n"
				+	"- stats\n"
	columns: 1

	Formula
	{
		rhs: "variables"
	}

	VariablesForm
	{
		infoLabel: qsTr("Input")
		AvailableVariablesList	{ name: "allVariablesList"								}
		AssignedVariablesList	{ name: "variables";		title: qsTr("Variables");	info: qsTr("All variables of interest.") }
		AssignedVariablesList	{ name: "splitBy";			title: qsTr("Split");		info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["nominal"];	id: splitBy; minLevels: 2; maxLevels: 256 } // without maxLevels entering a continuous variable can freeze/ crash jasp, so we need an arbitrary maximum
	}

	CheckBox
	{
		name	: "descriptivesTableTransposed"
		label	: qsTr("Transpose descriptives table")
		info	: qsTr("Tranposes the main table")
		checked	: false
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{
			title: qsTr("Sample size")
			CheckBox { name: "valid";			label: qsTr("Valid");	checked: true	}
			CheckBox { name: "missing";			label: qsTr("Missing");	checked: true	}
		}

		Group
		{
			title:	qsTr("Quantiles")
			info:	qsTr("Percentile Values")

			CheckBox { name: "quartiles";	label: qsTr("Quartiles"); info: qsTr("Displays the 25th, 50th, and 75th percentiles of the data points.") }
			CheckBox
			{
				name:				"quantilesForEqualGroups"; label: qsTr("Cut points for: ")
				infoLabel:			qsTr("Cut points for x equal groups")
				info:				qsTr("Displays the cut points that divide the data into x equal groups; default is 4 equal groups.")
				childrenOnSameRow:	true

				IntegerField
				{
					name:			"quantilesForEqualGroupsNumber"
					min:			2
					max:			1000
					defaultValue:	4
					afterLabel:		qsTr(" equal groups")
				}
			}

			CheckBox
			{
				name:				"percentiles"
				label:				qsTr("Percentiles:")
				info:				qsTr("Displays the xth percentile; percentile values must be separated by comma.")
				childrenOnSameRow:	true

				TextField
				{
					inputType:	"doubleArray"
					name:		"percentileValues"
					fieldWidth: 60
				}
			}
		}

		Group
		{
			title: qsTr("Central tendency")
			infoLabel: qsTr("Central Tendency (only for continuous variables)")

			CheckBox { name: "mode";			label: qsTr("Mode");	info: qsTr("Mode of the data points; if more than one mode exists, only the first is reported. For nominal and ordinal data, the mode is the most frequent observed value. For continuous data, the mode is the value with highest density estimate (see 'Distribution Plots' -> 'Display density'). If a footnote about multimodality for continuous variables is reported, we recommend visualizing the data to check for multimodality.")	}
			CheckBox { name: "median";			label: qsTr("Median");	info: qsTr("Median of the data points.")					}
			CheckBox { name: "mean";			label: qsTr("Mean");	info: qsTr("Arithmetic mean of the data points") ;	checked: true	}
		}

		Group
		{
			title:	qsTr("Distribution")

			CheckBox { name: "skewness";		label: qsTr("Skewness");			info: qsTr("Skewness of the distribution of the data points.") }
			CheckBox { name: "kurtosis";		label: qsTr("Kurtosis");			info: qsTr("Kurtosis of the distribution of the data points.") }
			CheckBox { name: "shapiroWilkTest";	label: qsTr("Shapiro-Wilk test");	info: qsTr("Shapiro-Wilk test") }
			CheckBox { name: "sum";				label: qsTr("Sum");					info: qsTr("Sum of the data points.") }
		}

		Group
		{
			title:				qsTr("Dispersion")
			infoLabel:			qsTr("Dispersion (only for continuous variables)")
			columns:			2
			Layout.columnSpan:	parent.columns

			CheckBox { name: "sd";							label: qsTr("Std. deviation");				info: qsTr("Standard deviation of the data points."); checked: true	}
			CheckBox { name: "coefficientOfVariation";		label: qsTr("Coefficient of variation");	info: qsTr("The Coefficient of variation gives us the relative dispersion of the data, in contrast to the standard deviation, which gives the absolute dispersion. For this purpose, the standard deviation is divided by the mean value, so that the unit is truncated away.") }
			CheckBox { name: "mad";							label: qsTr("MAD");							info: qsTr("Median absolute deviation of the data points.") }
			CheckBox { name: "madRobust";					label: qsTr("MAD robust");					info: qsTr("Median absolute deviation of the data points, adjusted by a factor for asymptotically normal consistency.") }
			CheckBox { name: "iqr";							label: qsTr("IQR");							info: qsTr("Interquartile range of the data points; 75th percentile - 25th percentile.") }
			CheckBox { name: "variance";					label: qsTr("Variance");					info: qsTr("Variance of the data points.") }
			CheckBox { name: "range";						label: qsTr("Range");						info: qsTr("Range of the data points; maximum - minimum.") }
			CheckBox { name: "minimum";						label: qsTr("Minimum");						info: qsTr("Minimum value of the data points."); checked: true	}
			CheckBox { name: "maximum";						label: qsTr("Maximum");						info: qsTr("Maximum value of the data points."); checked: true	}
		}


		Group
		{
			title:		qsTr("Inference")

			CheckBox { name: "seMean";	label: qsTr("S.E. mean"); info: qsTr("Standard error of the mean.") }

			CheckBox
			{
				name: "meanCi"
				label: qsTr("Confidence interval for mean")

				CIField
				{
					name: "meanCiLevel"
					label: qsTr("Width")
					info: qsTr("width of the confidence interval.")
				}

				DropDown
				{
					name: "meanCiMethod"
					label: qsTr("Method")
					id: ciMethod
					indexDefaultValue: 0
					info: qsTr("How should the confidence interval be computed? By default, we use a `T model`, which yields results identical to a one-sample t-test. Alternative options are a normal model (%1), or `Bootstrap`.").arg("$\\bar{x} \\pm z_{95} \\times SE$")
					values:
					[
						{label: qsTr("T model"),	value: "oneSampleTTest"},
						{label: qsTr("Normal model"),	value: "normalModel"},
						{label: qsTr("Bootstrap"),		value: "bootstrap"}
					]
				}
			}

			CheckBox
			{
				name: "sdCi"
				label: qsTr("Confidence interval for std. deviation")
				info: qsTr("a confidence interval for the standard deviation based on bootstrap samples.")

				CIField
				{
					name: "sdCiLevel"
					label: qsTr("Width")
				}
			}

			CheckBox
			{
				name: "varianceCi"
				label: qsTr("Confidence interval for variance")
				info: qsTr("a confidence interval for the variance based on bootstrap samples.")

				CIField
				{
					name: "varianceCiLevel"
					label: qsTr("Width")
				}
			}

			Group
			{
				title: qsTr("Bootstrap confidence interval options")

				IntegerField
				{
					name:			"ciBootstrapSamples"
					label:			qsTr("Bootstrap samples")
					info: qsTr("the number of bootstrap samples to be used.")
					defaultValue:	1000
					min:			1;
					max:			50000;
				}
			}
		}
		Group
		{
			title:	qsTr("Association matrix")

			CheckBox { name: "covariance";		label: qsTr("Covariance");	info: qsTr("Covariance value."); id:covariance}
			CheckBox { name: "correlation";		label: qsTr("Correlation");	info: qsTr("Pearson's correlation coefficient."); id:correlation}

			DropDown
			{
				name: "associationMatrixUse"
				id : associationMatrixUse
				label: qsTr("Use")
				info: qsTr("How to deal with missing values?")
				enabled:  covariance.checked || correlation.checked
				indexDefaultValue: 0
				values:
				[
					{label: qsTr("Everything"),							value: "everything",			info: qsTr("use all observations, resulting in NA when there are missing values.") },
					{label: qsTr("Complete observations"),				value: "complete.obs",			info: qsTr("missing values are handled by casewise deletion (i.e., only use rows of the data set that are complete).") },
					{label: qsTr("Pairwise compelete observations"),	value: "pairwise.complete.obs",	info: qsTr("use all complete pairs of observations on those variables. This can result in covariance or correlation matrices which are not positive semi-definite.") }
				]
			}
		}

		CheckBox { name: "statisticsValuesAreGroupMidpoints"; label: qsTr("Values are group midpoints"); debug: true }
	}

	Section
	{
		title: qsTr("Basic plots")
		columns: 2

		Group
		{
			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				CheckBox
				{
					name: "distributionPlots";	label: qsTr("Distribution plots");	id:	distributionPlots
					info: qsTr("For continuous variables, displays a histogram and the fit of a nonparametric density estimator. For nominal and ordinal variables, displays a frequency distribution.")
				}
				CheckBox
				{
					name: "correlationPlots";	label: qsTr("Correlation plots");	id:	correlationPlots
					info: qsTr("Displays a matrix of plots between continuous variables, with scatterplots between the variables in the off-diagonal entries, and histograms and density plots in the diagonal entries. The line represents the fit of a 1st, 2nd, 3rd, or 4th order polynomial (the selection is based on the Bayesian information criterion; Schwarz, 1978).")
				}
			}

			Group
			{
				enabled: distributionPlots.checked || correlationPlots.checked

				indent:		true
				CheckBox {	name: "distributionAndCorrelationPlotDensity";		label: qsTr("Display density")						}
				CheckBox {	name: "distributionAndCorrelationPlotRugMarks";		label: qsTr("Display rug marks")					}
				DropDown {
					name: "distributionAndCorrelationPlotHistogramBinWidthType"
					label: qsTr("Bin width type")
					indexDefaultValue: 0
					values:
						[
						{label: qsTr("Sturges"),				value: "sturges"},
						{label: qsTr("Scott"),					value: "scott"},
						{label: qsTr("Doane"),					value: "doane"},
						{label: qsTr("Freedman-Diaconis"),		value: "fd"	},
						{label: qsTr("Manual"),					value: "manual"	}
					]
					id: binWidthType
				}
				DoubleField
				{
					name:			"distributionAndCorrelationPlotHistogramManualNumberOfBins"
					label:			qsTr("Number of bins")
					defaultValue:	30
					min:			3;
					max:			10000;
					enabled:		binWidthType.currentValue === "manual"
				}
			}
		}

		Group
		{
			CheckBox {				name: "intervalPlot";	label: qsTr("Interval plots")					}
			CheckBox {				name: "qqPlot";			label: qsTr("Q-Q plots")						}
			CheckBox {				name: "pieChart";		label: qsTr("Pie charts")						}
			CheckBox {				name: "dotPlot";		label: qsTr("Dot plots")						}
		}

		Group
		{
			title: qsTr("Categorical plots")

			CheckBox
			{
				name: 	"paretoPlot"
				label: 	qsTr("Pareto plots")
				info:	qsTr("Displays the counts of each factor/level within the variable in a descending order. The y-axis represents the frequency (counts as grey bars) of each factor/level, the x-axis represents the factors/levels of the variable in an ordered sequence.") + "<br>"
						+ qsTr("By default, a cumulative line is drawn indicating the proportional contribution of each factor. A second vertical axis to the right side of the graph scales with this cumulative line and represents percentages to enable the description of the cumulative line.") + "<br>"
						+ qsTr("If \"Pareto rule\" is enabled, the two new lines enable a more precise assessment of factor/level contribution to the overall contribution (in percent) by using different input numbers.")

				CheckBox
				{
					name: 				"paretoPlotRule"
					label: 				qsTr("Pareto rule")
					childrenOnSameRow: 	true

					CIField { name: 	"paretoPlotRuleCi" }
				}
			}

			CheckBox
			{
				name: 	"likertPlot"
				label: 	qsTr("Likert plots")
				info:	qsTr("Displays a horizontally stacked bar chart showing the contribution of levels within a variable in percent. Order of levels depends on defined order in the JASP data table. A legend below the graph provides an overview of levels and their respective colors in the graph.")
					+ "<ul>"
					+	"<li>" + qsTr("The y-axis represents the variables used, the x-axis represents the percentages. Percentage contribution of all lower-order (below the middle level) and higher-order (above the middle level) levels are displayed on their respective side of the graph.") + "</li>"
					+	"<li>" + qsTr("The graph displays percentages on the x-axis as positive in both directions. Reason for the chosen display (in two directions) is the graphs usefulness in survey research where levels often follow a Likert based order (e.g., high - low, likely - unlikely, agreement - disagreement). Therefore, the graph contains a split between levels at their median.") + "</li>"
					+	"<li>" + qsTr("The number of variable levels determines the number of layers displayed. Layers represent the percentage distribution of the levels of the variable under investigation.") + "</li>"
					+	"<li>" + qsTr("If the variables contain an uneven amount of levels, the middle level is displayed as a grey block in the middle of the stacked bar with its percentage contribution on top.") + "</li>"
					+ "</ul>"

				CheckBox
				{
					name: 				"likertPlotAssumeVariablesSameLevel"
					label: 				qsTr("Assume all variables share the same levels")
					childrenOnSameRow: 	true
				}

				DropDown
				{
					id: 				likertPlotAdjustableFontSize
					name: 				"likertPlotAdjustableFontSize"
					label: 				qsTr("Adjustable font size for vertical axis")
					indexDefaultValue: 	0
					values:
					[
						{label: qsTr("Normal"), 	value: "normal"},
						{label: qsTr("Small"),		value: "small"},
						{label: qsTr("Medium"),		value: "medium"},
						{label: qsTr("Large"),		value: "large"}
					]
				}
			}
		}
	}


	Section
	{
		title: qsTr("Customizable plots")
		columns: 1

		ColorPalette{}

		CheckBox
		{
			name: "boxPlot";
			label: qsTr("Boxplots")
			info: qsTr("For continuous variables, displays a boxplot. Optionally, the outliers are labelled. Outliers are based on the interquartile range (IQR), i.e., [25th percentile] - 1.5 × IQR and [75th percentile] + 1.5 × IQR. Can also display in color, and has selectable boxplot, violin, and jitter elements for displaying the distribution of the data. This can be split by a categorical variable such as experimental condition.")
			Group {
				columns: 2
				Group {
					CheckBox {	name: "boxPlotBoxPlot";			label: qsTr("Boxplot element"); checked: true	}
					CheckBox {	name: "boxPlotViolin";			label: qsTr("Violin element")					}
					CheckBox {	name: "boxPlotJitter";			label: qsTr("Jitter element")					}
				}
				Group {
					CheckBox {  name: "boxPlotColourPalette";		label: qsTr("Use color palette")				}
					CheckBox {	name: "boxPlotOutlierLabel";		label: qsTr("Label outliers")					}
				}
			}
		}


		CheckBox
		{
			name: "scatterPlot";	label: qsTr("Scatter plots")
			columns: 2
			RadioButtonGroup
			{
				name:	"scatterPlotGraphTypeAbove";
				title:	qsTr("Graph above scatter plot")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton { value: "histogram";	label: qsTr("Histogram")					}
				RadioButton { value: "none";		label: qsTr("None")							}
			}
			RadioButtonGroup
			{
				name:	"scatterPlotGraphTypeRight";
				title:	qsTr("Graph right of scatter plot")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton { value: "histogram";	label: qsTr("Histogram")					}
				RadioButton { value: "none";		label: qsTr("None")							}
			}
			CheckBox
			{
				name: "scatterPlotRegressionLine"
				label: qsTr("Add regression line")
				checked: true
				RadioButtonGroup
				{
					name:	"scatterPlotRegressionLineType";
					RadioButton { value: "smooth";	label: qsTr("Smooth");	checked: true	}
					RadioButton { value: "linear";	label: qsTr("Linear")					}
				}

				CheckBox
				{
					name: "scatterPlotRegressionLineCi"
					label: qsTr("Show confidence interval")
					checked: true
					childrenOnSameRow: true
					CIField {	name: "scatterPlotRegressionLineCiLevel" }
				}
			}
			CheckBox
			{
				enabled: splitBy.count > 0
				name: "scatterPlotLegend"
				label: qsTr("Show legend")
				checked: true
			}
		}

		CheckBox
		{

			name: 		"densityPlot"
			label: 		qsTr("Frequency plots")
			columns: 2

			VariablesForm
			{
				preferredHeight: 100 * preferencesModel.uiScale

				AvailableVariablesList
				{
					name: 				"densityPlotVariables"
					source: 			[{ name: "allVariablesList", discard: ["variables", "splitBy"], use: "type=ordinal|nominal"}]
				}

				AssignedVariablesList
				{
					name: 				"densityPlotSeparate"
					id: 				densityPlotSeparate
					singleVariable: 	true
					title: 				qsTr("Separate frequencies:")
					allowedColumns: 	["nominal"]
				}
			}

			RadioButtonGroup
			{
				name:	"densityPlotType"
				id: 	densityPlotType
				title:	qsTr("Type:")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton
				{
					value: "histogram"
					label: qsTr("Histogram")
					RadioButtonGroup
					{
						name:	"customHistogramPosition";
						id: 	customHistogramPosition
						title:	qsTr("How to combine separate frequencies")
						RadioButton { value: "stack";		label: qsTr("Stack");		checked: true	}
						RadioButton { value: "identity";		label: qsTr("Identity")					}
						RadioButton { value: "dodge";	label: qsTr("Dodge")							}
					}
				}
			}

			DoubleField
			{
				name:			"densityPlotTransparency"
				label:			qsTr("Transparency")
				fieldWidth:		32
				defaultValue:	20
				min:			0
				max:			100
				enabled: densityPlotSeparate.count > 0 && ((densityPlotType.value === "density") || (densityPlotType.value === "histogram" && customHistogramPosition.value === "identity"))
			}
		}
		CheckBox
		{

				name: 		"heatmapPlot"
				label: 		qsTr("Tile heatmaps for selected variables")
				columns: 1

			VariablesForm
			{
				preferredHeight: 100 * preferencesModel.uiScale
				AvailableVariablesList
				{
					name: "heatmapVariables"
					source: [{ name: "allVariablesList", discard: ["variables", "splitBy"], use: "type=ordinal|nominal"}]
				}
				AssignedVariablesList
				{
					name: "heatmapHorizontalAxis"
					label: qsTr("Horizontal axis:")
					singleVariable: true
					allowedColumns: ["nominal"]
					minLevels: 2
				}
				AssignedVariablesList
				{
					name: "heatmapVerticalAxis"
					label: qsTr("Vertical axis:")
					singleVariable: true
					allowedColumns: ["nominal"]
					minLevels: 2
				}
			}

			DoubleField { name: "heatmapTileWidthHeightRatio"; label: qsTr("Width to height ratio of tiles"); negativeValues: false; defaultValue: 1}

			CheckBox
			{
				columns: 2
				name: "heatmapDisplayValue"
				label: qsTr("Display value")
				RadioButtonGroup
				{
					name: "heatmapStatisticContinuous"
					title: qsTr("For scale variables")
					RadioButton { value: "mean";		label: qsTr("Mean");	checked: true }
					RadioButton { value: "median";		label: qsTr("Median") }
					RadioButton { value: "identity";	label: qsTr("Value itself") }
					RadioButton { value: "length";		label: qsTr("Number of observations") }
				}

				RadioButtonGroup
				{
					name: "heatmapStatisticDiscrete"
					title: qsTr("For nominal and ordinal variables")
					RadioButton { value: "mode";		label: qsTr("Mode");	checked: true }
					RadioButton { value: "identity";	label: qsTr("Value itself") }
					RadioButton { value: "length";		label: qsTr("Number of observations") }
				}
				DoubleField { name: "heatmapDisplayValueRelativeTextSize"; label: qsTr("Relative text size"); negativeValues: false; defaultValue: 1 }

			}
			CheckBox { name: "heatmapLegend"; label: qsTr("Display legend")	}

		}
	}

	Section
	{
		title: qsTr("Tables")

		CheckBox
		{
			name:			"frequencyTables"
			label:			qsTr("Frequency tables")
			infoLabel:		qsTr("Frequency Tables (nominal and ordinal variables)")
			info:			qsTr("Displays a frequency table for each variable.")
			IntegerField
			{
				name:			"frequencyTablesMaximumDistinctValues"
				label:			qsTr("Maximum distinct values")
				min:			1
				defaultValue:	10
				fieldWidth:		50
				max:			2e2
			}
		}
		CheckBox
		{
			name	: "stemAndLeaf";
			label	: qsTr("Stem and leaf tables")
			info	: qsTr("Displays the spread of a variable.")
					+ "<ul>"
					  +	"<li>" + qsTr("Stem: the first digit(s).") + "</li>"
					  +	"<li>" + qsTr("Leaf: the first digit after the stem.") + "</li>"
					+ "</ul>"

			DoubleField
			{
				name: "stemAndLeafScale";	label: qsTr("scale");	negativeValues: false;	inclusive: JASP.MaxOnly;	max: 200;	defaultValue: 1.0;
				info: qsTr("The scale parameter controls how much the table is expanded. For example, scale = 2 will cause the table to be roughly twice as long as the default (scale = 1).")
			}
		}
	}
}
