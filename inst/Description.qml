import QtQuick
import JASP.Module

Description
{
	name		: "jaspDescriptives"
	title		: qsTr("Descriptives")
	description	: qsTr("Explore the data with tables and plots")
	version			: "0.95.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-descriptives.svg"
	hasWrappers	: true
	preloadData	: true

	Analysis
	{
		title:			qsTr("Descriptive Statistics")
		func:			"Descriptives"
		preloadData	: true
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
