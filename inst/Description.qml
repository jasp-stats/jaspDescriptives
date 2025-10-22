import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Descriptives")
	description	: qsTr("Explore the data with tables and plots")
	icon		: "analysis-descriptives.svg"
	hasWrappers	: true

	Analysis
	{
		title:			qsTr("Descriptive Statistics")
		func:			"Descriptives"
	}

	Analysis
	{
		title:	qsTr("Raincloud Plots")
		func:	"raincloudPlots"
	}

	Analysis
	{
		title:	qsTr("Time Series Descriptives")
		func:	"DescriptivesTimeSeries"
	}

	Analysis
	{
		title:  qsTr("Flexplot")
		qml:   	"Flexplot.qml"
		func:	"flexplot"
		preloadData	: false

	}
	Analysis
	{
		title:  qsTr("Plot Builder (beta)")
		qml:   	"jaspPlotBuilder.qml"
		func:	"jaspPlotBuilder"
		preloadData	: false

	}
}
