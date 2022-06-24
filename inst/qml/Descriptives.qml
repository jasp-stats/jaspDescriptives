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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

// All Analysis forms must be built with the From QML item
Form
{
	columns: 1
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"								}
		AssignedVariablesList	{ name: "variables";		title: qsTr("Variables")	}
		AssignedVariablesList	{ name: "splitby";			title: qsTr("Split");		singleVariable: true; suggestedColumns: ["ordinal", "nominal"];	id: splitBy }
	}

	CheckBox
	{
		name	: "transposeMainTable"
		label	: qsTr("Transpose descriptives table")
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

			CheckBox { name: "percentileValuesQuartiles";	label: qsTr("Quartiles") }
			CheckBox
			{
				name:				"percentileValuesEqualGroups"; label: qsTr("Cut points for: ")
				childrenOnSameRow:	true

				IntegerField
				{
					name:			"percentileValuesEqualGroupsNo"
					min:			2
					max:			1000
					defaultValue:	4
					afterLabel:		qsTr(" equal groups")
				}
			}

			CheckBox
			{
				name:				"percentileValuesPercentiles"
				label:				qsTr("Percentiles:")
				childrenOnSameRow:	true

				TextField
				{
					inputType:	"doubleArray"
					name:		"percentileValuesPercentilesPercentiles"
					fieldWidth: 60
				}
			}
		}

		Group
		{
			title: qsTr("Central tendency")

			CheckBox { name: "mode";			label: qsTr("Mode");					}
			CheckBox { name: "median";			label: qsTr("Median")					}
			CheckBox { name: "mean";			label: qsTr("Mean");	checked: true	}
		}

		Group
		{
			title:	qsTr("Distribution")

			CheckBox { name: "skewness";			label: qsTr("Skewness")						}
			CheckBox { name: "kurtosis";			label: qsTr("Kurtosis")						}
			CheckBox { name: "shapiro";				label: qsTr("Shapiro-Wilk test")			}
			CheckBox { name: "sum";					label: qsTr("Sum");							}
		}

		Group
		{
			title:				qsTr("Dispersion")
			columns:			2
			Layout.columnSpan:	2

			CheckBox { name: "standardErrorMean";	label: qsTr("S.E. mean")							}
			CheckBox { name: "standardDeviation";	label: qsTr("Std. deviation");		checked: true	}
			CheckBox { name: "cOfVariation";		label: qsTr("Coefficient of variation");			}
			CheckBox { name: "mad";					label: qsTr("MAD")									}
			CheckBox { name: "madrobust";			label: qsTr("MAD robust")							}
			CheckBox { name: "iqr";					label: qsTr("IQR")									}
			CheckBox { name: "variance";			label: qsTr("Variance")								}
			CheckBox { name: "range";				label: qsTr("Range")								}
			CheckBox { name: "minimum";				label: qsTr("Minimum");				checked: true	}
			CheckBox { name: "maximum";				label: qsTr("Maximum");				checked: true	}
		}

		CheckBox { name: "statisticsValuesAreGroupMidpoints"; label: qsTr("Values are group midpoints"); debug: true }
	}

	Section
	{
		title: qsTr("Basic plots")
		columns: 2

		Group
		{
			Group
			{
				columns: 2
				CheckBox {				name: "plotVariables";			label: qsTr("Distribution plots");	id:	plotVariables					}
				CheckBox {				name: "plotCorrelationMatrix";	label: qsTr("Correlation plots");	id:	plotCorrelationMatrix			}
			}

			Group
			{
				enabled: plotVariables.checked || plotCorrelationMatrix.checked

				indent:		true
				CheckBox {			name: "distPlotDensity";	label: qsTr("Display density")						}
				CheckBox {			name: "distPlotRug";		label: qsTr("Display rug marks")					}
				DropDown {
					name: "binWidthType"
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
					name:			"numberOfBins"
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
			CheckBox {				name: "descriptivesIntervalPlot";label: qsTr("Interval plots")					}
			CheckBox {				name: "descriptivesQQPlot";		label: qsTr("Q-Q plots")						}
			CheckBox {				name: "descriptivesPiechart";	label: qsTr("Pie charts")						}
			CheckBox {				name: "descriptivesDotPlot";	label: qsTr("Dot plots")						}
			CheckBox 
			{			
				name: 		"descriptivesParetoPlot"
				label: 		qsTr("Pareto plots")
				
				CheckBox 
				{			
					name: 				"optParetoRule"	
					label: 				qsTr("Pareto rule")
					childrenOnSameRow: 	true
					
					CIField { name: 	"paretoRule" }
				}
			}
			CheckBox 
			{	
				name: 		"descriptivesLikertPlot"	
				label: 		qsTr("Likert plots")				
				
				DropDown 
				{
					id: 				fontSizeLikert
					name: 				"fontSizeLikert"
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

		DropDown
		{
			name: "colorPalette"
			label: qsTr("Color palette")
			indexDefaultValue: 0
			values:
			[
				{ label: qsTr("Colorblind"),		value: "colorblind"		},
				{ label: qsTr("Colorblind Alt."),	value: "colorblind3"	},
				{ label: qsTr("Viridis"),			value: "viridis"		},
				{ label: qsTr("ggplot2"),			value: "ggplot2"		},
				{ label: qsTr("Gray"),				value: "gray"			}
			]
		}

		CheckBox
		{
			name: "splitPlots";
			label: qsTr("Boxplots")
			Group {
				columns: 2
				Group {
					CheckBox {	name: "splitPlotBoxplot";		label: qsTr("Boxplot element"); checked: true	}
					CheckBox {	name: "splitPlotViolin";		label: qsTr("Violin element")					}
					CheckBox {	name: "splitPlotJitter";		label: qsTr("Jitter element")					}
				}
				Group {
					CheckBox {  name: "splitPlotColour";		label: qsTr("Use color palette")				}
					CheckBox {	name: "splitPlotOutlierLabel";	label: qsTr("Label outliers")					}
				}
			}
		}


		CheckBox
		{
			name: "scatterPlot";	label: qsTr("Scatter plots")
			columns: 2
			RadioButtonGroup
			{
				name:	"graphTypeAbove";
				title:	qsTr("Graph above scatter plot")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton { value: "histogram";	label: qsTr("Histogram")					}
				RadioButton { value: "none";		label: qsTr("None")							}
			}
			RadioButtonGroup
			{
				name:	"graphTypeRight";
				title:	qsTr("Graph right of scatter plot")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton { value: "histogram";	label: qsTr("Histogram")					}
				RadioButton { value: "none";		label: qsTr("None")							}
			}
			CheckBox
			{
				name: "addSmooth"
				label: qsTr("Add regression line")
				checked: true
				RadioButtonGroup
				{
					name:	"regressionType";
					RadioButton { value: "smooth";	label: qsTr("Smooth");	checked: true	}
					RadioButton { value: "linear";	label: qsTr("Linear")					}
				}

				CheckBox
				{
					name: "addSmoothCI"
					label: qsTr("Show confidence interval")
					checked: true
					childrenOnSameRow: true
					CIField {	name: "addSmoothCIValue" }
				}
			}
			CheckBox
			{
				enabled: splitBy.count > 0
				name: "showLegend"
				label: qsTr("Show legend")
				checked: true
			}
		}
			
		Group
		{
			
			VariablesForm
			{
				preferredHeight: 100 * preferencesModel.uiScale
				
				AvailableVariablesList 
				{ 
					name: 				"densityPlotVariables"
					label: 				qsTr("Density plots")
					source: 			[{ name: "allVariablesList", discard: ["variables", "splitby"], use: "type=ordinal|nominal|nominalText"}]
				}
				
				AssignedVariablesList 
				{ 
					name: 				"densityPlotSeparate"
					singleVariable: 	true
					title: 				qsTr("Separate densities:")
					suggestedColumns: 	["ordinal", "nominal"] 
				}
			}
			
			CheckBox 
			{ 
				name: 		"descriptivesDensityPlot"
				label: 		qsTr("Display density plots") 
			
				DoubleField
				{
					name:			"transparency"
					label:			qsTr("Transparency")
					fieldWidth:		32
					defaultValue:	20
					min:			0
					max:			100
				}
			}
		}

		VariablesForm
		{
			preferredHeight: 100 * preferencesModel.uiScale
			AvailableVariablesList
			{
				name: "heatMapVariables"
				label: qsTr("Tile heatmaps for selected variables")
				source: [{ name: "allVariablesList", discard: ["variables", "splitby"], use: "type=ordinal|nominal|nominalText"}]
			}
			AssignedVariablesList
			{
				name: "heatmapHorizontal"
				label: qsTr("Horizontal axis:")
				singleVariable: true
			}
			AssignedVariablesList
			{
				name: "heatmapVertical"
				label: qsTr("Vertical axis:")
				singleVariable: true
			}
		}

		Group
		{
			indent: true
			CheckBox { name: "heatmapLegend"; label: qsTr("Display legend")	}
			CheckBox
			{
				name: "heatmapPlotValue"; label: qsTr("Display value"); childrenOnSameRow: false;
				DoubleField { name: "heatmapPlotValueSize"; label: qsTr("Relative text size"); negativeValues: false; defaultValue: 1 }
			}
			DoubleField { name: "heatmapRectangleRatio"; label: qsTr("Width to height ratio of tiles"); negativeValues: false; defaultValue: 1}

			Group
			{
				columns: 2
				title: qsTr("Statistic to plot")
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
			}
		}
	}

	Section
	{
		title: qsTr("Tables")

		CheckBox
		{
			name:			"frequencyTables"
			label:			qsTr("Frequency tables")
			IntegerField
			{
				name:			"frequencyTablesMaximumAmount"
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
			DoubleField
			{
				name: "stemAndLeafScale";	label: qsTr("scale");	negativeValues: false;	inclusive: JASP.MaxOnly;	max: 200;	defaultValue: 1.0;
				info: qsTr("The scale parameter controls how much the table is expanded. For example, scale = 2 will cause the table to be roughly twice as long as the default (scale = 1).")
			}
			info	: qsTr("Create a Stem and leaf table.")
		}

	}

	Section
	{
		title: qsTr("Charts")
		debug: true

		RadioButtonGroup
		{
			name:	"chartType";
			title:	qsTr("Chart Type")

			RadioButton { value: "_1noCharts";		label: qsTr("None")			}
			RadioButton { value: "_2barCharts";		label: qsTr("Bar charts")	}
			RadioButton { value: "_3pieCharts";		label: qsTr("Pie charts")	}
			RadioButton { value: "_4histograms";	label: qsTr("Histograms")	}
		}

		RadioButtonGroup
		{
			name:	"chartValues"
			title:	qsTr("Chart Values")

			RadioButton { value: "_1frequencies";	label: qsTr("Frequencies")	}
			RadioButton { value: "_2percentages";	label: qsTr("Percentages")	}
		}
	}
}
