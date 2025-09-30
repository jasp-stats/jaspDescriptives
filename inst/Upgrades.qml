
import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName:	"Descriptives"
		fromVersion:	"0.16.4"
		toVersion:		"0.17.0"

		ChangeRename { from: "splitby";										to: "splitBy"								}
		ChangeRename { from: "transposeMainTable";							to: "descriptivesTableTransposed"			}
		ChangeRename { from: "percentileValuesQuartiles";					to: "quartiles"								}
		ChangeRename { from: "percentileValuesEqualGroups";					to: "quantilesForEqualGroups"				}
		ChangeRename { from: "percentileValuesEqualGroupsNo";				to: "quantilesForEqualGroupsNumber"			}
		ChangeRename { from: "percentileValuesPercentiles";					to: "percentiles"							}
		ChangeRename { from: "percentileValuesPercentilesPercentiles";		to: "percentileValues"						}
		ChangeRename { from: "shapiro";										to: "shapiroWilkTest"						}
		ChangeRename { from: "standardErrorMean";							to: "seMean"								}
		ChangeRename { from: "standardDeviation";							to: "sd"									}
		ChangeRename { from: "cOfVariation";								to: "coefficientOfVariation"				}
		ChangeRename { from: "madrobust";									to: "madRobust"								}
		ChangeRename { from: "plotVariables";								to: "distributionPlots"						}
		ChangeRename { from: "plotCorrelationMatrix";						to: "correlationPlots"						}
		ChangeRename { from: "distPlotDensity";								to: "distributionAndCorrelationPlotDensity"	}
		ChangeRename { from: "distPlotRug";									to: "distributionAndCorrelationPlotRugMarks"}
		ChangeRename { from: "binWidthType";								to: "distributionAndCorrelationPlotHistogramBinWidthType"		}
		ChangeRename { from: "numberOfBins";								to: "distributionAndCorrelationPlotHistogramManualNumberOfBins"	}
		ChangeRename { from: "descriptivesIntervalPlot";					to: "intervalPlot"							}
		ChangeRename { from: "descriptivesQQPlot";							to: "qqPlot"								}
		ChangeRename { from: "descriptivesPiechart";						to: "pieChart"								}
		ChangeRename { from: "descriptivesDotPlot";							to: "dotPlot"								}
		ChangeRename { from: "descriptivesParetoPlot";						to: "paretoPlot"							}
		ChangeRename { from: "paretoPlotRuleField";							to: "paretoPlotRuleCi"						}
		ChangeRename { from: "descriptivesLikertPlot";						to: "likertPlot"							}
		ChangeRename { from: "likertPlotEqualLevel";						to: "likertPlotAssumeVariablesSameLevel"	}
		ChangeRename { from: "likertPlotFontSize";							to: "likertPlotAdjustableFontSize"			}
		ChangeRename { from: "splitPlots";									to: "boxPlot"								}
		ChangeRename { from: "splitPlotBoxplot";							to: "boxPlotBoxPlot"						}
		ChangeRename { from: "splitPlotViolin";								to: "boxPlotViolin"							}
		ChangeRename { from: "splitPlotJitter";								to: "boxPlotJitter"							}
		ChangeRename { from: "splitPlotColour";								to: "boxPlotColourPalette"					}
		ChangeRename { from: "splitPlotOutlierLabel";						to: "boxPlotOutlierLabel"					}
		ChangeRename { from: "graphTypeAbove";								to: "scatterPlotGraphTypeAbove"				}
		ChangeRename { from: "graphTypeRight";								to: "scatterPlotGraphTypeRight"				}
		ChangeRename { from: "addSmooth";									to: "scatterPlotRegressionLine"				}
		ChangeRename { from: "regressionType";								to: "scatterPlotRegressionLineType"			}
		ChangeRename { from: "addSmoothCI";									to: "scatterPlotRegressionLineCi"			}
		ChangeRename { from: "addSmoothCIValue";							to: "scatterPlotRegressionLineCiLevel"		}
		ChangeRename { from: "showLegend";									to: "scatterPlotLegend"						}
		ChangeRename { from: "descriptivesDensityPlot";						to: "densityPlot"							}
		ChangeRename { from: "transparency";								to: "densityPlotTransparency"				}
		ChangeRename { from: "heatmapHorizontal";							to: "heatmapHorizontalAxis"					}
		ChangeRename { from: "heatmapVertical";								to: "heatmapVerticalAxis"					}
		ChangeRename { from: "heatmapPlotValue";							to: "heatmapDisplayValue"					}
		ChangeRename { from: "heatmapPlotValueSize";						to: "heatmapDisplayValueRelativeTextSize"	}
		ChangeRename { from: "heatmapRectangleRatio";						to: "heatmapTileWidthHeightRatio"			}
		ChangeRename { from: "frequencyTablesMaximumAmount";				to: "frequencyTablesMaximumDistinctValues"	}
	}

	Upgrade
	{
		functionName:	"Descriptives"
		fromVersion:	"0.17.1"
		toVersion:		"0.17.2"
		// This change was previously errorenously omitted, but should only be added if missing. As 0.17.1 could actually add it itself when making a new analysis
		ChangeRename 
		{ 	
			condition: function(options) { return options["heatmapLegend"]  == undefined }; 
			from: "heatmapDisplayLegend";		to: "heatmapLegend"								
		}
	}

	Upgrade
	{
		functionName:	"Descriptives"
		fromVersion:	"0.95.0"
		toVersion:	"0.95.1"

		// this resulted from adding geometric and harmonic means
		ChangeRename { from: "mean";	to: "meanArithmetic" }
	}
}
