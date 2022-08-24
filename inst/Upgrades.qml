
import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:	"Descriptives"
		fromVersion:	"0.16.3"
		toVersion:		"0.16.4"

		ChangeRename { from: "transposeMainTable"; to: "descriptivesTableTransposed" }
        ChangeRename { from: "percentileValuesQuartiles"; to: "quartiles" }
        ChangeRename { from: "percentileValuesEqualGroups"; to: "quantilesForEqualGroups" }
        ChangeRename { from: "percentileValuesEqualGroupsNo"; to: "quantilesForEqualGroupsNumber" }
        ChangeRename { from: "percentileValuesPercentiles"; to: "percentiles" }
	}
}
