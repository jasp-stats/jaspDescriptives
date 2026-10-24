import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns: 1
	info: qsTr("Time Series Descriptives allows the user to obtain descriptive statistics and descriptive plots for univariate time-series.")
	VariablesForm
    {
		preferredHeight: 150 * preferencesModel.uiScale
		AvailableVariablesList { name: "variables" }
        AssignedVariablesList
        {
            name: "dependent"
			label: qsTr("Variable")
			allowedColumns: ["scale"]
            singleVariable: true
            info: qsTr("A variable that is measured repeatedly over time.")
        }
        AssignedVariablesList
        {
			id:		time
			name:	"time"
			label:	qsTr("Time")
			allowedColumns: ["scale", "ordinal", "nominal"]
            singleVariable: true
			info:	qsTr("Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.")
        }
    }

    Group
    {
        CheckBox
        {
            name: "descriptivesTableTransposed"
            label: qsTr("Transpose descriptives table")
        }
    }
    Group
    {
        CheckBox
        {
            name: "filter"
            id: filter
            label: qsTr("Filter by")
            info: qsTr("Selects an inclusive range of observations for analysis. Row number refers to original spreadsheet rows, even when Time changes the analysis order. Time index filters numeric Time values, including negative values and decimals. Date filters interpreted dates and times. Date filters use UTC. A date-only End includes the entire day; an End with a time uses that exact cutoff. Time or date filtering requires a usable Time variable; otherwise, select Row number or disable filtering.")
            RadioButtonGroup
            {
                name: "filterBy"
                RadioButton
                {
                    value: "row"
                    id: filterRow
                    label: qsTr("Row number")
                    checked: true
                    Group
                    {
                        columns: 2
                        IntegerField { name: "rowStart"; label: qsTr("Start"); defaultValue: 1 }
                        IntegerField { name: "rowEnd"; label: qsTr("End"); defaultValue: 100 }
                    }
                }
                RadioButton
                {
                    value: "time"
                    id: filterTime
                    label: qsTr("Time index")
                    enabled: time.count != 0
                    Group
                    {
                        columns: 2
                        DoubleField { name: "timeStart"; label: qsTr("Start"); defaultValue: 1; negativeValues: true; decimals: 10 }
                        DoubleField { name: "timeEnd"; label: qsTr("End"); defaultValue: 100; negativeValues: true; decimals: 10 }
                    }
                }
                RadioButton
                {
                    value: "date"
                    id: filterDate
                    label: qsTr("Date")
                    enabled: time.count != 0
                    Group
                    {
						columns: 2
                        TextField
                        {
                            name: "dateStart"
                            label: qsTr("Start")
                            placeholderText: "YYYY-MM-DD HH:MM:SS"
                            fieldWidth: 150
                        }
                        TextField
                        {
                            name: "dateEnd"
                            label: qsTr("End")
                            placeholderText: "YYYY-MM-DD HH:MM:SS"
                            fieldWidth: 150
                        }
                    }
                }
            }
        }
    }

	Group
    {
        title: qsTr("Plots")
        columns: 2
        CheckBox
        {
            name: "timeSeriesPlot"
            id:    tsPlot
            label: qsTr("Time series plot")
            checked: true
            info: qsTr("Plots the dependent variable (y-axis) over time (x-axis).")
            RadioButtonGroup
            {
                name:	"timeSeriesPlotType"
                radioButtonsOnSameRow: true
                RadioButton { value: "points";	label: qsTr("Points") }
                RadioButton { value: "line";	label: qsTr("Line") }
                RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            }
            RadioButtonGroup
            {
                name:	"timeSeriesPlotDistribution"
                title: qsTr("Distribution")
                RadioButton { value: "density";	label: qsTr("Density") }
                RadioButton { value: "histogram";	label: qsTr("Histogram") }
                RadioButton { value: "none";	label: qsTr("None");	checked: true }
            }
        }
        CheckBox
        {
            name: "lagPlot"
            id: sspPlot
            label: qsTr("Lag plot")
            info: qsTr("Plots the dependent variable (y-axis) against a lagged version of itself (x-axis). The lag stands for the amount of observations in between the dependent variable and the lagged version of itself. The regression line is the autoregression at the specified lag.")
            Group
            {
                IntegerField
                {
                    name: "lagPlotLag"
                    label: qsTr("Lag")
                    defaultValue: 1
                    min: 1
                }
                CheckBox
                {
                    name: "lagPlotRegressionLine"
                    id: sspSmooth
                    label: qsTr("Add regression line")
                    checked: true
                    RadioButtonGroup
                    {
                        name:	"lagPlotRegressionType"
                        radioButtonsOnSameRow: true
                        RadioButton { value: "smooth";	label: qsTr("Smooth");	checked: true }
                        RadioButton { value: "linear";	label: qsTr("Linear")	}
                    }

                    CheckBox
                    {
                        name: "lagPlotRegressionCi"
                        label: qsTr("Confidence interval")
                        checked: true
                        childrenOnSameRow: true
                        CIField { name: "lagPlotRegressionCiLevel" }
                    }
                }
            }
        }
        CheckBox
        {
            name: "acf"
            id: acf
            label: qsTr("Autocorrelation function")
            IntegerField { name: "acfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
            CheckBox { name: "acfZeroLag"; label: qsTr("Zero lag") }
            info: qsTr("Plots the autocorrelation for a specified number of lags. The confidence interval may be given assuming either a white noise process, or assuming for a lag q a moving average process of order q - 1.")
            CheckBox
            {
                name: "acfCi"
                id: acfCi
                label: qsTr("Confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "acfCiLevel" }
                
            }
            RadioButtonGroup
            {
                name: "acfCiType"
                enabled: acfCi.checked
                Layout.leftMargin: 25 * preferencesModel.uiScale
                RadioButton
                {
                    value: "whiteNoise"
                    label: qsTr("Based on white noise")
                    checked: true
                }
                RadioButton
                {
                    value: "movingAverage"
                    label: qsTr("Based on moving average")
                }
            }
        }
        CheckBox
        {
            name: "pacf"
            id: pacf
            label: qsTr("Partial autocorrelation function")
            info: qsTr("Plots the partial autocorrelation for a specified number of lags.")
            IntegerField { name: "pacfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
            CheckBox
            {
                name: "pacfCi"
                label: qsTr("Confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "pacfCiLevel" }
            }
        }
    }
}
