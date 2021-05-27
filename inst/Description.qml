import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspDescriptives"
	title		: qsTr("Descriptives")
	description	: qsTr("Explore the data with tables and plots")
	version		: "0.15"
	author		: "JASP Team"
	maintainer	: "JASP Team <jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-descriptives.svg"

	Analysis
	{
		title:	qsTr("Descriptive Statistics")
		func:	"Descriptives"
	}
}
