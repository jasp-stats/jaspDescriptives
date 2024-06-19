import QtQuick
import JASP.Controls
import JASP

Form
{
	info: qsTr("Flexplot allows the user to create graphical displays of data, using barcharts and histograms for univariate data, and various different types of scatterplots for bivariate and multivariate data.")
	infoBottom: "## " + qsTr("References") + "\n" +
				"- Fife, D.A., (in press). The Eight Steps of Data Analysis: A Graphical Framework to Promote Sound Statistical Analysis. *Perspectives on Psychological Science.* doi: 10.31234/osf.io/r8g7c\n" +
				"- Fife, D.A., (2020). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3\n" +
				"## " + qsTr("Example") + "\n" +
				qsTr("For more details about flexplot in JASP, watch this %1videa%2").arg("<a href=\"https://www.youtube.com/watch?v=N2vM74rw6-Q&list=PL8F480DgtpW8pF6MmNaEUR95n1RmIgasP&feature=youtu.be\">").arg("</a>")
	Formula
	{
		lhs: "dependent"
		rhs: "variables"
	}

	VariablesForm
	{
		infoLabel: qsTr("Input")
		AvailableVariablesList	{ name: "allVariables" }
		AssignedVariablesList	{
			name: "dependent"		;
			title: qsTr("Dependent Variable");
			info: qsTr("The variable of interest. This is also called the outcome variable. If only an outcome variable is specified, Flexplot will produce a histogram for numeric data and a barchart for categorical data. If independent variable(s) and/or panelled variable(s) are specified, this variable will be displayed on the Y axis.")
			singleVariable: true
			onCountChanged:  nameY.value = count > 0 ? model.data(model.index(0,0)) : ""
		}
		AssignedVariablesList	{
			name: "variables"		;
			title: qsTr("Independent Variable(s)") ;
			id: varlist
			maxRows: 2
			height: 60
			info: qsTr("The variable(s) for which we wish to visually assess relationships with the DV. The first variable chosen shows up on the X axis, either as a scatterplot (for numeric predictors) or as a beeswarm plot (for categorical variables). The second variable chosen will show up as different colors/lines/symbols. If the second varaible chosen is numeric, it will be binned first.")
			onCountChanged: {
				nameLegend.value = count > 1 ? model.data(model.index(1,0)) : "";
				nameX.value = count > 0 ? model.data(model.index(0,0)) : "";
			}

		}
		AssignedVariablesList	{
			name: "paneledVars"	;
			title: qsTr("Paneled Variable(s)");
			info: qsTr("Variables specified in these boxes will be binned (if numeric) then displayed as different subplots. The first variable specified as a panelled variable will form the column plots, while the second will form the row plots.")
			id: paneledVars
			maxRows: 2
			height: 60
			onCountChanged: {
				nameCols.value = count > 0 ? model.data(model.index(0,0)) : "";
				nameRows.value = count > 1 ? model.data(model.index(1,0)) : "";
			}

		}
	}

	Section
	{
		title: qsTr("Options")

		Group
		{
			title: qsTr("Point controls")
			columns: 4
			Slider
			{
				name: "alpha"
				label: qsTr("Point transparency")
				info: qsTr("The degree of transparency of the dots in the graphics")
				value: 0.4
				vertical: true
				enabled: varlist.count > 0
			}
			Slider
			{
				name: "jitx"
				label: qsTr("Jitter in X")
				info: qsTr("The maximal amount of \"jittering\" used on the X axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap.")
				value: .1
				min: 0
				max: .5
				vertical: true
				enabled: varlist.count > 0
			}
			Slider
			{
				name: "jity"
				label: qsTr("Jitter in Y")
				info: qsTr("The maximal amount of \"jittering\" used on the Y axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap.")
				value: 0
				min: 0
				max: .5
				vertical: true
				enabled: varlist.count > 0
			}
		}
		Group
		{
			Group
			{
				title: qsTr("Visual Statistics")
				CheckBox
				{
					name:"confidence";
					label: qsTr("Plot confidence bands")
					info: qsTr("Should 95% confidence intervals be displayed?")
					enabled: varlist.count > 0
				}
				DropDown
				{
					name: "type"
					values:
						[
						{label: qsTr("Loess"),					value: "Loess",			info: qsTr("A non-parametric loess line") },
						{label: qsTr("Regression"),				value: "Regression",	info: qsTr("A straight (regression) line") },
						{label: qsTr("Quadratic"),				value: "Quadratic",		info: qsTr("A line that includes both a linear effect and a quadratic (squared) term") },
						{label: qsTr("Cubic"),					value: "Cubic",			info: qsTr("A line that includes a linear, squared, and cubed term") },
						{label: qsTr("None"),					value: "None",			info: qsTr("No line") }
					]
					label: qsTr("Fitted line (scatterplots)")
					info: qsTr("The type of fitted line displayed when the x axis is a numeric variable. These can be one of the following:")
					enabled: varlist.count > 0
				}
				DropDown
				{
					name: "intervals"
					values:
						[
						{label: qsTr("Quartiles"),				value: "Quartiles",				info: qsTr("Horizontal lines are displayed at the 25/75th percentiles, with a dot for the median") },
						{label: qsTr("Standard errors"),		value: "Standard errors",		info: qsTr("Horizontal lines are displayed at +1/-1 standard errors from the mean, with a dot for the mean") },
						{label: qsTr("Standard deviations"),	value: "Standard deviations",	info: qsTr("Horizontal lines are displayed at +1/-1 standard deviations from the mean, with a dot for the mean") }
					]
					label: qsTr("Intervals (categorical predictors)")
					enabled: varlist.count > 0
				}
			}

			Group
			{
				title: qsTr("Other Plot Controls")
				DropDown
				{
					name: "theme"
					label: qsTr("GGplot theme")
					info: qsTr("the type of GGplot theme to use when displaying the data. Can be one of the following:")
					values:
						[
						{label: qsTr("JASP"),					value: "JASP"},
						{label: qsTr("Black and white"),		value: "Black and white"},
						{label: qsTr("Minimal"),				value: "Minimal"},
						{label: qsTr("Classic"),				value: "Classic"},
						{label: qsTr("Dark"),					value: "Dark"}
					]
				}
				DropDown
				{
					name: "palette"
					label: qsTr("Color Palette")
					values:
						[
						{label: qsTr("GGplot Default"),			value: "GGplot Default"},
						{label: qsTr("Nature"),					value: "Nature"},
						{label: qsTr("AAAS"),					value: "AAAS"},
						{label: qsTr("Lancet"),					value: "Lancet"},
						{label: qsTr("JCO"),					value: "JCO"},
						{label: qsTr("Dark"),					value: "Dark"}
					]
				}
				CheckBox
				{
					name:"bw";
					label: qsTr("Convert to grayscale");
					checked: false
				}

				CheckBox
				{
					name:"ghost";
					label: qsTr("Ghost lines");
					info: qsTr("Ghost lines are a visual aid that can be used when doing panelled plots. Ghost lines simply repeat the fitted line from one panel across the other panels to make it easier to make comparisons across panels.")
					checked: true
					enabled: paneledVars.count > 0
				}
			}
		}
	}

	Section
	{
		title: qsTr("Plot Labels")
		Group
		{
			title: qsTr("Plot Labels")
			TextField
			{
				id: nameX;
				label: qsTr("X Axis Label");
				name: "nameX";
			}
			TextField
			{
				id: nameY
				label: qsTr("Y Axis Label")
				name: "nameY";
			}
			TextField
			{
				id: nameLegend
				label: qsTr("Legend Label")
				name: "nameLegend";
			}
			TextField
			{
				id: nameCols
				label: qsTr("Column Panel Label")
				name: "nameCols";
			}
			TextField
			{
				id: nameRows
				label: qsTr("Row Panel Label")
				name: "nameRows";
			}
		}
	}
}
