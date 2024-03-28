import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspDescriptives"
	title		: qsTr("Descriptives")
	description	: qsTr("Explore the data with tables and plots")
	version		: "0.19.0"  // Built with the JASP nightly: macOS arm64 from 06:33 21-03-2024
	author		: "JASP Team"
	maintainer	: "JASP Team <jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-descriptives.svg"
	hasWrappers	: true

	Analysis
	{
		title:	qsTr("Descriptive Statistics")
		func:	"Descriptives"
	}

	Analysis
	{
		title:	qsTr("Raincloud Plots")
		func:	"raincloudPlots"
	}
}
