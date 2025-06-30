	//
// Copyright (C) 2013-2024 University of Amsterdam
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



Form {
	columns: 1

	info: qsTr("The <b>Plot Builder (beta)</b> lets you assemble a complete figure in just a few clicks.<br/><br/>"
			+ "1. <b>Decide on design:</b> At the very top choose <i>Repeated measurements → Yes</i> if the same subjects are measured more than once "
			+ "(e.g., across time-points or conditions); otherwise leave <i>No</i> selected. "
			+ "2. <b>Map your variables:</b> Drag variables from the list on the left into the fields on the right (X-Axis, Y-Axis, etc). "
			+ "3. <b>Choose what to display:</b> Expand the <i>Data and geometries</i> accordion to add layers such as raw points, histograms/box-/violin plots, "
			+ "counts, proportions, Mean or Median summaries, etc. "
			+ "4. <b>Polish the figure:</b> Use the remaining accordions to fine-tune axes, titles, annotations, themes, colors, sizing, and legend placement.")



	infoBottom:
		"## " + qsTr("References") + "\n" +
		"- Engler, J. B. (2025). “Tidyplots Empowers Life Scientists With Easy Code-Based Data Visualization.” _iMeta_, 4, e70018. https://doi.org/10.1002/imt2.70018\n" +
		"- Engler, J. B. (2024). _tidyplots: Tidy Plots for Scientific Papers_. R package version 0.2.1. Available at: <https://CRAN.R-project.org/package=tidyplots>.\n" +
		"- Wickham, H., & Seidel, D. (2023). _rlang: Functions for Base Types and Core R and 'Tidyverse' Features_. R package version 1.1.1. Available at: <https://CRAN.R-project.org/package=rlang>.\n" +
		"- Wickham, H., François, R., Henry, L., Müller, K., & Vaughan, D. (2023). _dplyr: A Grammar of Data Manipulation_. R package version 1.1.4. Available at: <https://CRAN.R-project.org/package=dplyr>.\n" +
		"- Wickham, H., & Henry, L. (2023). _tidyr: Tidy Messy Data_. R package version 1.3.0. Available at: <https://CRAN.R-project.org/package=tidyr>.\n" +
		"- Dawson, C. (2024). _ggprism: A 'ggplot2' Extension Inspired by 'GraphPad Prism'_. R package version 1.0.5. Available at: <https://csdaw.github.io/ggprism/>.\n" +
		"- Pedersen, T. L. (2020). _patchwork: The Composer of Plots_. R package version 1.1.1. Available at: <https://CRAN.R-project.org/package=patchwork>.\n" +
		"- Wickham, H., & Grolemund, G. (2017). _R for Data Science: Import, Tidy, Transform, Visualize, and Model Data_. O'Reilly Media.\n" +
		"- Wickham, H. (2016). _ggplot2: Elegant Graphics for Data Analysis_. Springer-Verlag New York.\n" +
		"- Kassambara, A. (2024). _ggpubr: 'ggplot2' Based Publication Ready Plots_. R package version 0.6.0. Available at: <https://rpkgs.datanovia.com/ggpubr/>.\n" +
		"- Dunnington, D. (2023). _ggeasy: Easy Access to 'ggplot2' Commands_. R package version 0.1.4. Available at: <https://CRAN.R-project.org/package=ggeasy>.\n" +
		"- Wickham, H., & Henry, L. (2023). _forcats: Tools for Working with Categorical Variables (Factors)_. R package version 1.0.0. Available at: <https://CRAN.R-project.org/package=forcats>.\n"

	TabView {
		infoLabel: qsTr("The basic setup: decide on the design (repeated-measures or non-repeated-measures) and select the variables")
		name: "PlotBuilderTab"
		newItemName: qsTr("Plot 1")
		rowComponent: Group {

			childControlsArea.anchors.leftMargin: jaspTheme.contentMargin

			Group{
				columns:3
				RadioButtonGroup {
					id:						isRM
					Layout.columnSpan:		1
					name:					"isRM"
					title:					qsTr("Repeated measurements")
					radioButtonsOnSameRow:	true
					columns:				2
					info: qsTr("Select whether you want to create a repeated-measures plot or a non-repeated-measures plot")

					RadioButton {
						label:		qsTr("No")
						info: qsTr("Select this to create a non-repeated-measures plot")
						value:		"noRM"
						id:			noRM
						checked:	true
					}

					RadioButton {
						label:		qsTr("Yes")
						info: qsTr("Select this to create a repeated-measures plot")
						value:		"RM"
						id:          yesRM
					}
				}

				CheckBox{
				name: "deleteNAListwise"
				id: deleteNAlistwise
				checked: true
				visible: yesRM.checked
				enabled: yesRM.checked
				info: qsTr("ON (listwise deletion): if a case is missing even one of the selected variables, the whole case is excluded."
						   + "OFF: keep every case and ignore only the specific values that are missing")
				label: qsTr("Delete missing values listwise")
				}
			}

			Group {
				VariablesForm {

					removeInvisibles:true
					preferredWidth: jaspForm.width - 2 * jaspTheme.contentMargin
					preferredHeight: 300 * jaspTheme.uiScale
					visible: noRM.checked

					infoLabel: qsTr("Input for non-repeated measures plot")

					// NO RM DATASET -------------------------------------------------------------------------------------------------------------------------

					AvailableVariablesList {
						name: "allVariablesList"
					}

					AssignedVariablesList {
						name: "variableXPlotBuilder"
						title: qsTr("X-Axis Variable")
						id: variableXPlotBuilder
						allowedColumns: ["scale", "ordinal", "nominal"]
						minLevels: 2
						singleVariable: true
						info: qsTr("Select the variable for the X-Axis")
					}

					AssignedVariablesList {
						name: "variableYPlotBuilder"
						title: qsTr("Y-Axis Variable")
						allowedColumns: ["scale", "ordinal", "nominal"]
						id: variableYPlotBuilder
						singleVariable: true
						info: qsTr("Select the variable for the Y-Axis")
					}

					AssignedVariablesList {
						name: "variableColorPlotBuilder"
						title: qsTr("Group Variable")
						id: variableColorPlotBuilder
						allowedColumns: ["scale", "ordinal", "nominal"]
						minLevels: 2
						visible: !yesRM.checked
						singleVariable: true
						info: qsTr("Select the variable for data grouping, which will also determine the coloring.")
						onCountChanged: {
							if (count > 0) {
								colorByVariableX.checked = false;
								colorByVariableY.checked = false;
							}
						}
					}

					AssignedVariablesList {
						name: "columnsvariableSplitPlotBuilder"
						title: qsTr("Split (Columns)")
						id: columnsvariableSplitPlotBuilder
						allowedColumns: ["ordinal", "nominal"]
						singleVariable: true
						info: qsTr("You can choose a variable to split the plots into columns")
					}

					AssignedVariablesList {
						name: "rowsvariableSplitPlotBuilder"
						title: qsTr("Split (Rows)")
						id: rowsvariableSplitPlotBuilder
						allowedColumns: ["ordinal", "nominal"]
						singleVariable: true
						info: qsTr("You can choose a variable to split the plots into rows")

					}

					AssignedVariablesList {
						name: "gridVariablePlotBuilder"
						title: qsTr("Grid")
						id: gridVariablePlotBuilder
						allowedColumns: ["ordinal", "nominal"]
						singleVariable: true
						info: qsTr("You can choose a variable to make a grid")
					}
				}

				//  RM DATASET ---------------------------------------------------------------------------------------------------------------------------

				VariablesForm {

					removeInvisibles:	true
					preferredWidth: jaspForm.width - 2 * jaspTheme.contentMargin
					preferredHeight: 600  * jaspTheme.uiScale
					visible: yesRM.checked

					infoLabel: qsTr("Input for repeated measures plot")

					AvailableVariablesList {
						name: "allVariablesListRM"
					}


					FactorLevelList	{
						name: "repeatedMeasuresFactors";
						id: repeatedMeasuresFactors
						title: qsTr("Repeated Measures Factors")
						info:qsTr("Here you can specify the factor for the repeated measures. For example, it could be time (e.g., 24 h, 48 h, 72 h) or conditions like before and after")
						height: 180 * preferencesModel.uiScale;	factorName: qsTr("RM Factor")

					}

					AssignedRepeatedMeasuresCells {
						id: rmCells
						name: "repeatedMeasuresCells"
						info: qsTr("Assign the variables that correspond to each level of the repeated measures factor. For example, if the factor is time, assign variables like '24h', '48h', and '72h'")
						title: qsTr("Repeated Measures Cells")
						source: "repeatedMeasuresFactors"
					}

					AssignedVariablesList {
						name: "betweenSubjectFactors"
						title: qsTr("Between Subject Factors")
						info: qsTr("Select the between-subject factors that distinguish different groups of subjects, such as treatment group, gender, or genotype. These variables must be categorical (nominal) with at least two levels")
						allowedColumns: ["nominal"]
						minLevels: 2
					}

					AssignedVariablesList {
						name: "covariates"
						title: qsTr("Covariates")
						info: qsTr("Select continuous variables that may influence the outcome and should be included as covariates in the analysis")
						allowedColumns: ["scale"]
						minNumericLevels: 2
					}


				}

			} //

			/* -------------------------------------------------------------------
			   Point annotations – repelled data-labels
			   ------------------------------------------------------------------- */


			Section{
				// infoLabel: qsTr("Here you can define the role of the repeated measures variables in the plot.")
				visible: yesRM.checked
				title: qsTr("Assign repeated measures components")
				VariablesForm {
					removeInvisibles: true
					id: varsForm
					preferredHeight: 300 * preferencesModel.uiScale

					AvailableVariablesList {
						id: withinComponents
						name: "withinComponents"
						title: qsTr("Repeated Measures Components")
						source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"]
						enabled: yesRM.checked
						onEnabledChanged: {
							if (enabled) {
								while (count > 0) {
									itemDoubleClicked(0)
								}
							}
						}
					}

					AssignedVariablesList {
						id: xVarRM
						allowedColumns: ["scale", "ordinal", "nominal"]
						name: "xVarRM"
						info: qsTr("Select the variable for the X-Axis")
						title: qsTr("X-Axis Variable")
						singleVariable: true
					}

					AssignedVariablesList {
						id: groupVarRM
						name: "groupVarRM"
						title: qsTr("Group Variable")
						singleVariable: true
						info: qsTr("Select the variable for data grouping, which will also determine the coloring")
						allowedColumns: ["scale", "ordinal", "nominal"]
					}

					AssignedVariablesList {
						name: "colSplitRM"
						title: qsTr("Split (Columns)")
						id: colSplitRM
						allowedColumns: ["ordinal", "nominal"]
						info: qsTr("You can choose a variable to split the plots into columns")
						singleVariable: true
					}

					AssignedVariablesList {
						name: "rowSplitRM"
						title: qsTr("Split (Rows)")
						id: rowSplitRM
						allowedColumns: ["ordinal", "nominal"]
						singleVariable: true
						info: qsTr("You can choose a variable to split the plots into rows")
					}

					AssignedVariablesList {
						name: "gridVarRM"
						title: qsTr("Grid")
						id: gridVarRM
						allowedColumns: ["ordinal", "nominal"]
						singleVariable: true
						info: qsTr("You can choose a variable to make a grid")
					}
				}
			}

			Label {
				text: qsTr("Data and geometries")
				wrapMode: Text.Wrap
				color: "black"
				font.bold: true
			}

			// -------------------------------------------------------------------
			// Data points
			// -------------------------------------------------------------------


		Section {
				title: qsTr("Individual data points")

				Label {
					text: qsTr("Required: X AND Y-Axis Variables")
					wrapMode: Text.Wrap
					color: "black"
				}

				CheckBox {
					name: "addDataPoint"
					label: qsTr("Individual data points")
					id: addDataPoint
					info: qsTr("Check this option to show individual data points on the plot")
					columns: 4
					enabled: (
								 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
								 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

								 ||

								 // 2. If repeated measures (yesRM) and at least one of the following is true:
								 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

									  xVarRM.count > 0

									  ))
								 )

					onEnabledChanged: {
						if (!enabled) {
							checked = false;
						}
					}

					Group {
						DoubleField {
							info: qsTr("Set the point size")
							name: "pointsizePlotBuilder"
							id: pointsizePlotBuilder
							label: qsTr("Point size")
							enabled: !sizeByVariable.checked
							value: 3
							min: 0
							max: 10
						}

						CheckBox{
						name:"sizeByVariable"
						id: sizeByVariable
						label: qsTr("Size by variable")
						info: qsTr("Choose this if you want the point size to be determined by a variable rather than a fixed constant")
						}

						CheckBox{
						name:"shapebyVariable"
						id: shapebyVariable
						label: qsTr("Shape by variable")
						info: qsTr("Choose this if you want the point shape to be determined by a variable")
						}
					}

					Group {
						DoubleField {
							info: "A small “jitter” is added to the individual data points displayed to prevent overlaps. Here you can adjust the height of the jitter."
							name: "jitterhPlotBuilder"
							id: jitterhPlotBuilder
							label: qsTr("Jitter height")
							value: 0.3
							min: 0
							max: 10
						}

						DoubleField {
							info: "A small “jitter” is added to the individual data points displayed to prevent overlaps. Here you can adjust the width of the jitter."
							name: "jitterwPlotBuilder"
							id: jitterwPlotBuilder
							label: qsTr("Jitter width")
							value: 0.3
							min: 0
							max: 10
						}
					}

					Group {
						DoubleField {
							name: "alphaPlotBuilder"
							id: alphaPlotBuilder
							label: qsTr("Transparency")
							info: qsTr("Set the transparency")
							value: 0.5
							min: 0
							max: 1
						}

						DoubleField {
							name: "pointDodgePlotBuilder"
							label: qsTr("Dodge")
							info: qsTr("Adjusts the horizontal “dodge” spacing between subgroup geometric elements. "
									  + "Only takes effect when a grouping variable is mapped (e.g., male vs. female within each species). "
									  + "A value of 0 overlays the elements; higher values push the subgroups farther apart")
							id: pointDodgePlotBuilder
							defaultValue: 0.8
						}
					}

					Group{
						CheckBox {
							name: "blackOutlineDataPoint"
							label: qsTr("Black outline")
							checked: false
							enabled: !shapebyVariable.checked
							info: qsTr("Add a black outline/border to the elements")
							onEnabledChanged: {
								if (!enabled) {
									checked = false;
								}
							}
						}

						CheckBox {
							name: "whiteBorder"
							label: qsTr("White outline")
							checked: false
							enabled: !shapebyVariable.checked
							info: qsTr("Add a white outline/border to the elements")
							onEnabledChanged: {
								if (!enabled) {
									checked = false;
								}
							}
						}

						CheckBox {
							name: "emptyCircles"
							label: qsTr("Empty circles")
							checked: false
							enabled: !shapebyVariable.checked
							info: qsTr("Remove the fill")
							onEnabledChanged: {
								if (!enabled) {
									checked = false;
								}
							}
						}
					}

				}

				VariablesForm{
					visible: sizeByVariable.checked
					enabled: sizeByVariable.checked
					preferredWidth: jaspForm.width - 2 * jaspTheme.contentMargin
					preferredHeight: 100 * preferencesModel.uiScale

					AvailableVariablesList {
						name: "sizeVars"
						source: [
							{ name: "allVariablesList", use: "type=scale|ordinal" },
							{ name: "allVariablesListRM", use: "type=scale|ordinal" }
						]
					}

					AssignedVariablesList {
						id: sizeVariablePlotBuilder
						allowedColumns: ["scale", "ordinal"]
						enabled: sizeByVariable.checked
						name: "sizeVariablePlotBuilder"
						title: qsTr("Point Size Variable")
						singleVariable: true
						onEnabledChanged: {
							if (!enabled) {
								while (count > 0) {
									itemDoubleClicked(0)
								}
							}
						}
					}

					Group{
					columns:2
					DoubleField {
						name: "pointSizeMin"
						label: qsTr("Min point size")
						value: 1

					}
					DoubleField {
						name: "pointSizeMax"
						label: qsTr("Max point size")
						value: 4

					}
					}
				}

				VariablesForm{
					preferredWidth: jaspForm.width - 2 * jaspTheme.contentMargin
					preferredHeight: 100 * preferencesModel.uiScale
					visible: shapebyVariable.checked
					enabled: shapebyVariable.checke

					AvailableVariablesList {
						name: "shapeVars"
						source: [
							{ name: "allVariablesList",   use: "type=nominal|ordinal" },
							{ name: "allVariablesListRM", use: "type=nominal|ordinal" }
						]
					}



					AssignedVariablesList {
						id: pointShapeVariable
						allowedColumns: ["nominal", "ordinal"]
						name: "pointShapeVariable"
						title: qsTr("Point Shape Variable")
						singleVariable: true
						enabled: shapebyVariable.checked
						onEnabledChanged: {
							if (!enabled) {
								while (count > 0) {
									itemDoubleClicked(0)
								}
							}
						}
					}

				}


				CheckBox { ///////////////////////
					name: "addStatEllipse"
					label: qsTr("Add ellipse")
					checked: false
					enabled: addDataPoint.checked
					columns: 3
					info: qsTr("Add a statistical ellipse around the points."
					+ "'Ellipse type' controls the method:"
					+ "\"t\" assumes a multivariate t-distribution,"
					+ "\"Normal\" assumes a multivariate normal distribution,"
					+ "and \"Euclidean\" draws a circle whose radius equals the value of 'Level'")
					onEnabledChanged: {
						if (!enabled) {
							checked = false;
						}
					}

					DropDown {
						name: "ellipseType"
						label: qsTr("Ellipse type")
						values: [
							{ label: qsTr("t"), value: "t" },
							{ label: qsTr("Normal"), value: "norm" },
							{ label: qsTr("Euclidean"), value: "euclid"}
						]
						indexDefaultValue: 0
					}

					DoubleField {
						name: "fillEllipse"
						label: qsTr("Transparency")
						value: 0.20
						min: 0
						max: 1
					}

					DoubleField {
						name: "levelEllipse"
						label: qsTr("Level")
						value: 0.95
						min: 0
						max: 1
						info: qsTr("Confidence level (for t and normal ellipse type) or radius (euclidean ellipse type)")
					}

				} /////////////////////////////////////////


				CheckBox {
					name: "connectRMPlotBuilder"
					label: qsTr("Connect data points (requires repeated measures)")
					id: connectRMPlotBuilder
					checked: false
					enabled: isRM.value === "RM" & addDataPoint.checked
					columns: 2
					onEnabledChanged: {
						if (!enabled) {
							checked = false;
						}
					}

					DoubleField {
						name: "lineRMtransparency"
						label: qsTr("Transparency")
						value: 0.5
					}

					DoubleField {
						name: "lineRMsize"
						label: qsTr("Line width")
						value: 0.5
					}


				}

			}

			// -------------------------------------------------------------------
			// Distributions (histogram, boxplot, violin)
			// -------------------------------------------------------------------
			Section {
				title: qsTr("Distributions (histogram, boxplot, violin)")

				Label {
					text: qsTr("Required: X OR Y-Axis Variable (histogram); X AND Y-Axis Variables (boxplot, violin)")
					wrapMode: Text.Wrap
					color: "black"
				}

				GridLayout {
					columns: 3
					rows: 1
					rowSpacing: 40
					columnSpacing: 40


					Group{

						// Histogram
						CheckBox {
							name: "addHistogram"
							id: addHistogram
							label: qsTr("Histogram")
							info: qsTr("Check this option to create histogram for x or y variables. Histogram requires either X-Axis or X-Axis variable, but not both.")
							enabled: (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)
									 && !(variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

							onEnabledChanged: {
								if (!enabled) {
									checked = false;
								}
							}

							DoubleField {
								name: "binsPlotBuilder"
								label: qsTr("Number of bins")
								info: qsTr("Sets the number of bins used to divide the data range in histograms."
										 + "More bins give finer detail, fewer bins give a more general overview")
								id: binsPlotBuilder
								defaultValue: 30
							}

							DoubleField {
								name: "alphaHistogramPlotBuilder"
								label: qsTr("Transparency")
								id: alphaHistogramPlotBuilder
								defaultValue: 0.8
								info: qsTr("Set the transparency")
								min: 0
								max: 1
							}

							CheckBox{
							name: "blackHistogramOutline"
							label: qsTr("Black outline")
							}
						}

						// Density

						CheckBox {
							name: "addDensity"
							id: addDensity
							label: qsTr("Density")
							info: qsTr("Check this option to overlay a density estimate on the histogram. Density can be shown as scaled (0–1) or on the histogram count scale.")
							enabled: (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)
									 && !(variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

							// Density overlay mode radiobutton
							RadioButtonGroup {
								id: densityOverlayMode
								name: "densityOverlayMode"
								title: qsTr("Density overlay mode")
								radioButtonsOnSameRow: false
								columns: 1
								info: qsTr("Choose whether the density curve is scaled to the histogram count ('Count'), or normalized to a maximum of 1 ('Scaled')")

								RadioButton {
									label: qsTr("Count")
									info: qsTr("Show density on the histogram count scale (area under the curve equals total count)")
									value: "count"
									id: densityCount
									checked: true
								}

								RadioButton {
									label: qsTr("Scaled (0–1)")
									info: qsTr("Show density scaled so that the maximum of the curve is 1")
									value: "scaled"
									id: densityScaled
								}
							}

							DoubleField {
								name: "alphaDensityPlotBuilder"
								label: qsTr("Transparency")
								id: alphaDensityPlotBuilder
								defaultValue: 0.8
								info: qsTr("Set the transparency for the density overlay")
								min: 0
								max: 1
							}

							DoubleField {
								name: "lineWidthDensity"
								label: qsTr("Line width")
								id: lineWidthDensity
								defaultValue: 0.8
								info: qsTr("Set the line width for the density overlay")

							}

							CheckBox{
							name: "blackDensityOutline"
							label: qsTr("Black outline")
							}
						}

					}



					// Boxplot


					CheckBox {
						name: "addBoxplot"
						id: addBoxplot
						label: qsTr("Boxplot")
						info: qsTr("Check this option to create boxplot. Boxplot and violin plot require both X-Axis and Y-Axis variables.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeBoxplotPlotBuilder"
							label: qsTr("Dodge")
							id: dodgeBoxplotPlotBuilder
							info: qsTr("Adjusts the horizontal “dodge” spacing between subgroup geometric elements. "
									  + "Only takes effect when a grouping variable is mapped (e.g., male vs. female within each species). "
									  + "A value of 0 overlays the elements; higher values push the subgroups farther apart")
							defaultValue: 0.8}


						DoubleField {
							name: "alphaBoxplotPlotBuilder"
							label: qsTr("Transparency")
							id: alphaBoxplotPlotBuilder
							info: qsTr("Set the transparency")
							defaultValue: 0.8
							min: 0
							max: 1
						}

						DoubleField {
							name: "widthLineBoxplotPlotBuilder"
							label: qsTr("Line width")
							id: widthLineBoxplotPlotBuilder
							info: qsTr("Sets the line width of the geometric element (e.g., boxplot, violin, error bar). "
									 + "Increase for thicker outlines, decrease for thinner ones")
							defaultValue: 0.8
						}

						DoubleField {
							name: "widthBoxplotPlotBuilder"
							label: qsTr("Boxplot width")
							info: qsTr("Set the width of the boxplots")
							id: widthBoxplotPlotBuilder
							defaultValue: 0.6
						}

						DoubleField {
							name: "widthWhiskersPlotBuilder"
							label: qsTr("Whiskers width")
							info: qsTr("Sets the horizontal width of the whiskers in a boxplot")
							id: widthWhiskersPlotBuilder
							defaultValue: 0.3
						}

						CheckBox {
							name: "outlierBoxplotPlotBuilder"
							label: qsTr("Show outliers")
							id: outlierBoxplotPlotBuilder
							info: qsTr("Displays individual outlier points beyond the whiskers in a boxplot. "
										 + "Outliers are defined as values that fall outside the range of Q1 − coef × IQR and Q3 + coef × IQR")
							checked: false
						}

						DoubleField {
							name: "outlierCoefBoxplotPlotBuilder"
							label: qsTr("Outlier coef")
							info: qsTr("Sets the multiplier (coef) used to define outliers in a boxplot. "
										+ "Outliers are values beyond Q1 − coef × IQR or Q3 + coef × IQR")
							id: outlierCoefoxplotPlotBuilder
							defaultValue: 1.5
						}

						DoubleField {
							name: "outlierSizeBoxplotPlotBuilder"
							label: qsTr("Outlier size")
							info: qsTr("Set the size of the outliers")
							id: outlierSizeBoxplotPlotBuilder
							defaultValue: 1
						}

						CheckBox {
							name: "blackOutlineBoxplot"
							label: qsTr("Black outline")
							checked: true
							info: qsTr("Add a black outline/border to the elements")
						}

						CheckBox {
							name: "whiteOutlineBoxplot"
							info: qsTr("Add a white outline/border to the elements")
							label: qsTr("White outline")
						}


					}

					// Violin
					CheckBox {
						name: "addViolin"
						id: addViolin
						label: qsTr("Violin plot")
						info: qsTr("Check this option to add a violin plot to your visualization. Boxplot and violin plot require both x-axis and y-axis variables.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeViolinPlotBuilder"
							label: qsTr("Dodge")
							info: qsTr("Adjusts the horizontal “dodge” spacing between subgroup geometric elements. "
									  + "Only takes effect when a grouping variable is mapped (e.g., male vs. female within each species). "
									  + "A value of 0 overlays the elements; higher values push the subgroups farther apart")
							defaultValue: 0.8
						}

						DoubleField {
							name: "alphaViolinPlotBuilder"
							label: qsTr("Transparency")
							id: alphaViolinPlotBuilder
							defaultValue: 0.8
							info: qsTr("Set the transparency")
							min: 0
							max: 1
						}

						DoubleField {
							name: "linewidthViolinPlotBuilder"
							label: qsTr("Line width")
							info: qsTr("Sets the line width of the geometric element (e.g., boxplot, violin, error bar). "
									 + "Increase for thicker outlines, decrease for thinner ones")
							id: linewidthViolinPlotBuilder
							defaultValue: 0.8
						}

						DropDown {
							name: "scaleViolinPlotBuilder"
							label: qsTr("Scale method")
							info: qsTr("Controls how the width of each violin is scaled:"
									 + "<b>Area</b>: all violins have the same area (default in ggplot2)."
									 + "<b>Count</b>: violins are scaled by the number of observations in each group."
									 + "<b>Width</b>: all violins have the same maximum width, regardless of group size.")
							id: scaleViolinPlotBuilder
							values: [
								{ label: qsTr("Area"), value: "area" },
								{ label: qsTr("Count"), value: "count" },
								{ label: qsTr("Width"), value: "width" }
							]
							indexDefaultValue: 2
						}

						FormulaField {
							name: "drawQuantilesViolinPlotBuilder"
							label: qsTr("Draw quantiles")
							info: qsTr("Draws horizontal lines at the specified quantiles inside each violin. "
									 + "Use comma-separated values between 0 and 1 (e.g., 0.25, 0.5, 0.75) to show quartiles or other custom quantiles.")
							defaultValue: "0.25, 0.5, 0.75"
						}

						CheckBox {
							name: "trimViolinPlotBuilder"
							label: qsTr("Trim violins")
							id: trimViolinPlotBuilder
							info: qsTr("If checked, the violins are trimmed to the range of the data. "
									 + "If unchecked, the density tails may extend beyond the observed values.")
							checked: false
						}

						CheckBox {
							name: "blackOutlineViolin"
							label: qsTr("Black outline")
							info: qsTr("Add a black outline/border to the elements")

						}

						CheckBox {
							name: "whiteOutlineViolin"
							label: qsTr("White outline")
							info: qsTr("Add a white outline/border to the elements")

						}


					}
				}

			}

			// -------------------------------------------------------------------
			// Amounts (count and sum)
			// -------------------------------------------------------------------



			Section {
				title: qsTr("Amounts (count and sum)")
				info: qsTr("This section adds layers that visualise amounts. "
						 + "<b>Dodge</b> shifts subgroup geoms sideways so they don’t overlap. "
						 + "<b>Size / Width</b> sets the thickness of bars and lines or the radius of dots. "
						 + "<b>Transparency (alpha)</b> controls opacity: 0 = invisible, 1 = solid. "
						 + "<b>Outline (black / white)</b> toggles a border around the shape. ")



				Label {
					text: qsTr("Required: X OR Y-Axis Variable")
					wrapMode: Text.Wrap
					color: "black"
				}

				GridLayout {
					columns: 3
					rows: 2
					rowSpacing: 40
					columnSpacing: 40

					// Count Bar

					CheckBox {
						name: "addCountBar"
						label: qsTr("Count bar")
						info: qsTr("Enable to add a count bar to the plot.")
						enabled: isRM.value === "noRM" && // can't use if RM
								 ((variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0) ||
								  (variableXPlotBuilder.count === 0 && variableYPlotBuilder.count > 0))
						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeCountBar"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}

						DoubleField {
							name: "barwidthCountBar"
							label: qsTr("Bar width")
							defaultValue: 0.8
						}
						DoubleField {
							name: "alphaCountBar"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}
					}

					// Count Dash
					CheckBox {
						name: "addCountDash"
						label: qsTr("Count dash")
						info: qsTr("Enable to add dashed lines to the plot")
						enabled: isRM.value === "noRM" && // can't use if RM
								 ((variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0) ||
								  (variableXPlotBuilder.count === 0 && variableYPlotBuilder.count > 0))
						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeCountDash"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "dashwidthCountDash"
							label: qsTr("Dash width")
							defaultValue: 0.8
						}

						DoubleField {
							name: "linewidthCountDash"
							label: qsTr("Line width")
							defaultValue: 1
						}
						DoubleField {
							name: "alphaCountDash"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}

						CheckBox {
							name: "blackOutlineCountDash"
							label: qsTr("Black dash")
						}
					}

					// Count Dot
					CheckBox {
						name: "addCountDot"
						label: qsTr("Count dot")
						info: qsTr("Enable to add count dots to the plot")
						enabled: isRM.value === "noRM" && // can't use if RM
								 ((variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0) ||
								  (variableXPlotBuilder.count === 0 && variableYPlotBuilder.count > 0))
						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeCountDot"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "sizeCountDot"
							label: qsTr("Dot size")
							defaultValue: 5
						}


						Group {
							DoubleField {
								name: "alphaCountDot"
								label: qsTr("Transparency")
								defaultValue: 1
								min: 0
								max: 1
							}

							CheckBox {
								name: "blackOutlineCountDot"
								label: qsTr("Black outline")
							}

							CheckBox {
								name: "whiteOutlineCountDot"
								label: qsTr("White outline")
							}
						}
					}

					// Count Line
					CheckBox {
						name: "addCountLine"
						label: qsTr("Count line")
						info: qsTr("Enable to add count lines to the plot")
						enabled: isRM.value === "noRM" && // can't use if RM
								 ((variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0) ||
								  (variableXPlotBuilder.count === 0 && variableYPlotBuilder.count > 0))
						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeCountLine"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}

						DoubleField {
							name: "linewidthCountLine"
							label: qsTr("Line width")
							defaultValue: 1
						}

						DoubleField {
							name: "alphaCountLine"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}

						CheckBox {
							name: "blackOutlineCountLine"
							label: qsTr("Black line")
						}

					}

					CheckBox {
						name: "addCountArea"
						label: qsTr("Count area")
						info: qsTr("Enable to add a count area to the plot")
						enabled: isRM.value === "noRM" && // can't use if RM
								 ((variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0) ||
								  (variableXPlotBuilder.count === 0 && variableYPlotBuilder.count > 0))
						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeCountArea"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "alphaCountArea"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}
					}

					// Count Value
					CheckBox {
						name: "addCountValue"
						label: qsTr("Count value")
						info: qsTr("Enable to add count values to the plot")
						enabled: isRM.value === "noRM" &&
								 (
									 (variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0) ||
									 (variableXPlotBuilder.count === 0 && variableYPlotBuilder.count > 0)
									 )

						DoubleField {
							name: "fontsizeCountValue"
							label: qsTr("Font size")
							defaultValue: 14
						}

						FormulaField {
							name: "accuracyCountValue"
							label: qsTr("Accuracy")
							defaultValue: "0.1"
						}

						DoubleField {
							name: "alphaCountValue"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}

						DoubleField {
							name: "hjustCountValue"
							label: qsTr("Horizontal justification")
							defaultValue: 0.5
							min: -Infinity
						}

						DoubleField {
							name: "vjustCountValue"
							label: qsTr("Vertical justification")
							defaultValue: -0.5
							min: -Infinity
						}

						CheckBox {
							name: "blackOutlineCountValue"
							label: qsTr("Black text")
						}
					}
				}


				Label {
					text: qsTr("Required: X AND Y-Axis Variables")
					wrapMode: Text.Wrap
					color: "black"
				}

				GridLayout {
					columns: 3
					rows: 2
					rowSpacing: 40
					columnSpacing: 40

					// Sum Bar
					CheckBox {
						name: "addSumBar"
						label: qsTr("Sum bar")
						info: qsTr("Enable to add a sum bar to the plot.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeSumBar"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "widthSumBar"
							label: qsTr("Bar width")
							defaultValue: 0.8
						}
						DoubleField {
							name: "alphaSumBar"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}
					}

					// Sum Dash
					CheckBox {
						name: "addSumDash"
						label: qsTr("Sum dash")
						info: qsTr("Enable to add dashed lines to the plot.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeSumDash"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "widthSumDash"
							label: qsTr("Dash width")
							defaultValue: 0.8
						}
						DoubleField {
							name: "linewidthSumDash"
							label: qsTr("Line width")
							defaultValue: 1
						}
						DoubleField {
							name: "alphaSumDash"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}

						CheckBox {
							name: "blackOutlineSumDash"
							label: qsTr("Black dash")
						}
					}

					// Sum Dot
					CheckBox {
						name: "addSumDot"
						label: qsTr("Sum dot")
						info: qsTr("Enable to add sum dots to the plot.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeSumDot"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "sizeSumDot"
							label: qsTr("Dot size")
							defaultValue: 5
						}
						DoubleField {
							name: "alphaSumDot"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}

						CheckBox {
							name: "blackOutlineSumDot"
							label: qsTr("Black outline")
						}
						CheckBox {
							name: "whiteOutlineSumDot"
							label: qsTr("White outline")
						}
					}

					// Sum Line
					CheckBox {
						name: "addSumLine"
						label: qsTr("Sum line")
						info: qsTr("Enable to add a sum line to the plot.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeSumLine"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "linewidthSumLine"
							label: qsTr("Line width")
							defaultValue: 0.5
						}
						DoubleField {
							name: "alphaSumLine"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}

						CheckBox {
							name: "blackOutlineSumLine"
							label: qsTr("Black line")
						}
					}

					// Sum Area
					CheckBox {
						name: "addSumArea"
						label: qsTr("Sum area")
						info: qsTr("Enable to add a sum area to the plot.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "dodgeSumArea"
							label: qsTr("Dodge")
							defaultValue: 0.8
						}
						DoubleField {
							name: "alphaSumArea"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}

					}

					// Sum Value
					CheckBox {
						name: "addSumValue"
						label: qsTr("Sum value")
						info: qsTr("Enable to add sum values to the plot.")
						enabled: (
									 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

									 ||

									 // 2. If repeated measures (yesRM) and at least one of the following is true:
									 (yesRM.checked && repeatedMeasuresFactors.count > 0 && (

										  xVarRM.count > 0

										  ))
									 )

						onEnabledChanged: {
							if (!enabled) {
								checked = false;
							}
						}

						DoubleField {
							name: "fontsizeSumValue"
							label: qsTr("Font size")
							defaultValue: 14
						}
						FormulaField {
							name: "accuracySumValue"
							label: qsTr("Accuracy")
							defaultValue: "0.1"
						}
						DoubleField {
							name: "alphaSumValue"
							label: qsTr("Transparency")
							defaultValue: 1
							min: 0
							max: 1
						}
						DoubleField {
							name: "hjustSumValue"
							label: qsTr("Horizontal justification")
							defaultValue: 0.5
							min: -Infinity
						}
						DoubleField {
							name: "vjustSumValue"
							label: qsTr("Vertical justification")
							defaultValue: -0.5
							min: -Infinity
						}

						CheckBox {
							name: "blackOutlineSumValue"
							label: qsTr("Black text")
						}
					}
				}
			}

			// -------------------------------------------------------------------
			// Proportions (bar/area stack)
			// -------------------------------------------------------------------

			Section {
				title: qsTr("Proportions")

				info: qsTr("Visualise how each subgroup contributes to the whole "
						 + "Requires one Group variable plus either an X- or Y-axis variable "
						 + "Absolute mode stacks raw counts; Relative mode rescales each stack to 0-1 or 0-100 % "
						 + "Common controls: Transparency sets opacity, Reverse order flips the stack sequence, "
						 + "Line width outlines area stacks, Replace N/A turns missing groups into a separate slice")
				columns: 2

				Label {
					text: qsTr("Required: Group Variable AND X- or Y-Axis Variable")
					wrapMode: Text.Wrap
					color: "black"
					Layout.columnSpan: 2
				}

				RadioButtonGroup {
					id: propModeGroup
					name: "propMode"
					title: qsTr("Proportion mode")
					radioButtonsOnSameRow: true
					columns: 2
					Layout.columnSpan: 2

					RadioButton {
						value: "absolute"
						label: qsTr("Absolute")
						checked: true
						info: qsTr("Stack heights show raw counts")
					}
					RadioButton {
						id: relative
						value: "relative"
						label: qsTr("Relative")
						info: qsTr("Each stack is rescaled so its total height equals 100 %")
					}
				}

				GridLayout {
					columns: 3
					columnSpacing: 40

					CheckBox {
						name: "addBarStack"
						label: qsTr("Bar stack")
						info: qsTr("Add stacked bars for each category using the selected proportion mode")
						enabled: ( noRM.checked &&
								   (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0) &&
								   variableColorPlotBuilder.count > 0 )
							  || ( yesRM.checked &&
								   repeatedMeasuresFactors.count > 0 &&
								   groupVarRM.count > 0 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "alphaBarStack";   label: qsTr("Transparency"); defaultValue: 0.8; min: 0; max: 1 }
						CheckBox    { name: "reverseBarStack"; label: qsTr("Reverse order") }
					}

					CheckBox {
						name: "addAreaStack"
						label: qsTr("Area stack")
						info: qsTr("Add stacked areas connected across categories")
						enabled: ( noRM.checked &&
								   (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0) &&
								   variableColorPlotBuilder.count > 0 )
							  || ( yesRM.checked &&
								   repeatedMeasuresFactors.count > 0 &&
								   groupVarRM.count > 0 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "alphaAreaStack";     label: qsTr("Transparency"); defaultValue: 0.4; min: 0; max: 1 }
						DoubleField { name: "linewidthAreaStack"; label: qsTr("Line width");  defaultValue: 0.25; min: 0 }
						CheckBox    { name: "reverseAreaStack";   label: qsTr("Reverse order") }
						CheckBox    { name: "replaceNaAreaStack"; label: qsTr("Replace N/A"); info: qsTr("Treat missing groups as a separate slice") }
					}

					CheckBox {
						name: "asPercentage"
						label: qsTr("Show as percentages")
						enabled: relative.checked
						info: qsTr("Format the relative scale from 0 % to 100 % instead of 0 to 1")
						onEnabledChanged: { if (!enabled) checked = false }
					}
				}
			}



			// -------------------------------------------------------------------
			// Mean
			// -------------------------------------------------------------------
			Section {
				title: qsTr("Mean")

				info: qsTr("Summary layers that display the arithmetic mean of the Y variable for every X (or vice versa). "
						 + "Requires both X and Y axes mapped. "
						 + "Mean can be rendered as bar, line, dot, dash, area, or value. "
						 + "Common controls: <b>Dodge</b> separates sub-groups, <b>Transparency</b> sets opacity, <b>Size/width</b> changes thickness, <b>Outline</b> toggles borders")

				Label {
					text: qsTr("Required: X AND Y-Axis Variables")
					wrapMode: Text.Wrap
					color: "black"
				}

				GridLayout {
					columns: 3
					rowSpacing: 40
					columnSpacing: 40

					/* ── Mean bar ── */
					CheckBox {
						name: "addMeanBar"
						label: qsTr("Mean bar")
						info: qsTr("Add mean bars")
						enabled: (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMeanBar";  label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "alphaMeanBar";  label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						DoubleField { name: "widthMeanBar";  label: qsTr("Bar width");    defaultValue: 0.8 }
					}

					/* ── Mean dash ── */
					CheckBox {
						name: "addMeanDash"
						label: qsTr("Mean dash")
						info: qsTr("Add dashed mean lines")
						enabled: (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMeanDash";     label: qsTr("Dodge");       defaultValue: 0.8 }
						DoubleField { name: "dashwidthMeanDash"; label: qsTr("Dash width");  defaultValue: 0.8 }
						DoubleField { name: "linewidthMeanDash"; label: qsTr("Line width");  defaultValue: 1 }
						DoubleField { name: "alphaMeanDash";     label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineMeanDash"; label: qsTr("Black dash") }
					}

					/* ── Mean dot ── */
					CheckBox {
						name: "addMeanDot"
						label: qsTr("Mean dot")
						info: qsTr("Add mean dots")
						enabled: (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMeanDot"; label: qsTr("Dodge");    defaultValue: 0.8 }
						DoubleField { name: "sizeMeanDot";  label: qsTr("Dot size"); defaultValue: 5 }
						DoubleField { name: "alphaMeanDot"; label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineMeanDot"; label: qsTr("Black outline") }
						CheckBox    { name: "whiteOutlineMeanDot"; label: qsTr("White outline") }
					}

					/* ── Mean line ── */
					CheckBox {
						name: "addMeanLine"
						label: qsTr("Mean line")
						info: qsTr("Add mean profile lines")
						enabled: (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMeanLine";   label: qsTr("Dodge");      defaultValue: 0.8 }
						DoubleField { name: "linewidthMeanLine"; label: qsTr("Line width"); defaultValue: 1 }
						DoubleField { name: "alphaMeanLine";   label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineMeanLine"; label: qsTr("Black line") }
					}

					/* ── Mean area ── */
					CheckBox {
						name: "addMeanArea"
						label: qsTr("Mean area")
						info: qsTr("Add filled mean areas")
						enabled: (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMeanArea"; label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "alphaMeanArea"; label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
					}

					/* ── Mean value ── */
					CheckBox {
						name: "addMeanValue"
						label: qsTr("Mean value")
						info: qsTr("Add numeric mean labels")
						enabled: (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField  { name: "fontsizeMeanValue";  label: qsTr("Font size"); defaultValue: 14 }
						FormulaField {
							name: "accuracyMeanValue"
							label: qsTr("Accuracy")
							info: qsTr("Sets the rounding increment for the mean value; 0.1 gives one decimal, 0.01 gives two")
							defaultValue: "0.1"
						}
						DoubleField  { name: "alphaMeanValue";     label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						DoubleField  { name: "hjustMeanValue";     label: qsTr("Horizontal justification"); defaultValue: 0.5; min: -Infinity }
						DoubleField  { name: "vjustMeanValue";     label: qsTr("Vertical justification");   defaultValue: -0.5; min: -Infinity }
						CheckBox     { name: "blackOutlineMeanValue"; label: qsTr("Black text") }
					}
				}
			}



			// -------------------------------------------------------------------
			// Median
			// -------------------------------------------------------------------
			Section {
				title: qsTr("Median")

				info: qsTr("Summary layers that display the median of the Y variable for every X (or vice versa). "
						 + "Requires both X and Y axes mapped. "
						 + "Median can be rendered as bar, line, dot, dash, area, or value. "
						 + "Common controls: <b>Dodge</b> separates sub-groups, <b>Transparency</b> sets opacity, <b>Size/width</b> changes thickness, <b>Outline</b> toggles borders")

				Label {
					text: qsTr("Required: X AND Y-Axis Variables")
					wrapMode: Text.Wrap
					color: "black"
				}

				GridLayout {
					columns: 3
					rowSpacing: 40
					columnSpacing: 40

					/* Median bar ---------------------------------------------------- */
					CheckBox {
						name: "addMedianBar"
						label: qsTr("Median bar")
						info: qsTr("Add median bars")
						enabled: (
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMedianBar";  label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "alphaMedianBar";  label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						DoubleField { name: "widthMedianBar";  label: qsTr("Bar width");    defaultValue: 0.8 }
					}

					/* Median dash --------------------------------------------------- */
					CheckBox {
						id: addMedianDash
						name: "addMedianDash"
						label: qsTr("Median dash")
						info: qsTr("Add dashed median lines")
						enabled: (
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMedianDash";     label: qsTr("Dodge");       defaultValue: 0.8 }
						DoubleField { name: "dashwidthMedianDash"; label: qsTr("Dash width");  defaultValue: 0.8 }
						DoubleField { name: "linewidthMedianDash"; label: qsTr("Line width");  defaultValue: 1 }
						DoubleField { name: "alphaMedianDash";     label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineMedianDash"; label: qsTr("Black dash"); enabled: addMedianDash.checked }
					}

					/* Median dot ---------------------------------------------------- */
					CheckBox {
						id: addMedianDot
						name: "addMedianDot"
						label: qsTr("Median dot")
						info: qsTr("Add median dots")
						enabled: (
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMedianDot"; label: qsTr("Dodge");    defaultValue: 0.8 }
						DoubleField { name: "sizeMedianDot";  label: qsTr("Dot size"); defaultValue: 5 }
						DoubleField { name: "alphaMedianDot"; label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineMedianDot"; label: qsTr("Black outline"); enabled: addMedianDot.checked }
						CheckBox    { name: "whiteOutlineMedianDot"; label: qsTr("White outline") }
					}

					/* Median line --------------------------------------------------- */
					CheckBox {
						id: addMedianLine
						name: "addMedianLine"
						label: qsTr("Median line")
						info: qsTr("Add median profile lines")
						enabled: (
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMedianLine";   label: qsTr("Dodge");      defaultValue: 0.8 }
						DoubleField { name: "linewidthMedianLine"; label: qsTr("Line width"); defaultValue: 1 }
						DoubleField { name: "alphaMedianLine";   label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineMedianLine"; label: qsTr("Black line"); enabled: addMedianLine.checked }
					}

					/* Median area --------------------------------------------------- */
					CheckBox {
						name: "addMedianArea"
						label: qsTr("Median area")
						info: qsTr("Add filled median areas")
						enabled: (
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeMedianArea"; label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "alphaMedianArea"; label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
					}

					/* Median value -------------------------------------------------- */
					CheckBox {
						name: "addMedianValue"
						label: qsTr("Median value")
						info: qsTr("Add numeric median labels")
						enabled: (
									 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField  { name: "fontsizeMedianValue";  label: qsTr("Font size"); defaultValue: 14 }
						FormulaField {
							name: "accuracyMedianValue"
							label: qsTr("Accuracy")
							info: qsTr("Sets the rounding increment for the median value; 0.1 gives one decimal, 0.01 gives two")
							defaultValue: "0.1"
						}
						DoubleField  { name: "alphaMedianValue";     label: qsTr("Transparency"); defaultValue: 1; min: 0; max: 1 }
						DoubleField  { name: "hjustMedianValue";     label: qsTr("Horizontal justification"); defaultValue: 0.5; min: -Infinity }
						DoubleField  { name: "vjustMedianValue";     label: qsTr("Vertical justification");   defaultValue: -0.5; min: -Infinity }
						CheckBox     { name: "blackOutlineMedianValue"; label: qsTr("Black text") }
					}
				}
			}


			// -------------------------------------------------------------------
			// Error bars and ribbons (range, sd, sem, 95% CI)
			// -------------------------------------------------------------------

			Section {
				title: qsTr("Error bars and ribbons (range, SD, SEM, 95% CI)")

				info: qsTr("Shows variability with vertical bars or shaded ribbons. "
						 + "Choose <b>Range</b> (min–max), <b>SD</b>, <b>SEM</b>, or <b>95 % CI</b>. "
						 + "Common controls: <b>Dodge</b> offsets overlapping groups, <b>Width/Line width</b> adjusts thickness, "
						 + "<b>Transparency</b> sets opacity, <b>Black lines</b> toggles outlines")

				Label {
					text: qsTr("Required: X AND Y-Axis Variables")
					wrapMode: Text.Wrap
					color: "black"
				}

				Label {
					text: qsTr("The optimal dodge value is 0 if both X and Y-Axis Variables are continuous")
					wrapMode: Text.Wrap
					color: "black"
				}

				GridLayout {
					columns: 2
					rows: 2
					rowSpacing: 40
					columnSpacing: 70

					/* Range Error Bar */
					CheckBox {
						name: "addRangeErrorBar"
						label: qsTr("Range error bar")
						info: qsTr("Add range error bars")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeRangeErrorBar";   label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "widthRangeErrorBar";   label: qsTr("Width");        defaultValue: 0.3 }
						DoubleField { name: "linewidthRangeErrorBar"; label: qsTr("Line width"); defaultValue: 1 }
						DoubleField { name: "transparencyRangeErrorBar"; label: qsTr("Transparency"); defaultValue: 1 }
						CheckBox    { name: "blackOutlineRangeErrorBar"; label: qsTr("Black lines") }
					}

					/* SD Error Bar */
					CheckBox {
						id: addSDErrorBar
						name: "addSDErrorBar"
						label: qsTr("SD error bar")
						info: qsTr("Add SD error bars")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeSDErrorBar";   label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "widthSDErrorBar";   label: qsTr("Width");        defaultValue: 0.3 }
						DoubleField { name: "linewidthSDErrorBar"; label: qsTr("Line width"); defaultValue: 1 }
						DoubleField { name: "transparencySDErrorBar"; label: qsTr("Transparency"); defaultValue: 1 }
						CheckBox    { name: "blackOutlineSDErrorBar"; label: qsTr("Black lines"); enabled: addSDErrorBar.checked }
					}

					/* SEM Error Bar */
					CheckBox {
						id: addSEMErrorBar
						name: "addSEMErrorBar"
						label: qsTr("SEM error bar")
						info: qsTr("Add SEM error bars")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeSEMErrorBar";   label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "widthSEMErrorBar";   label: qsTr("Width");        defaultValue: 0.3 }
						DoubleField { name: "linewidthSEMErrorBar"; label: qsTr("Line width"); defaultValue: 1 }
						DoubleField { name: "transparencySEMErrorBar"; label: qsTr("Transparency"); defaultValue: 1 }
						CheckBox    { name: "blackOutlineSEMErrorBar"; label: qsTr("Black lines"); enabled: addSEMErrorBar.checked }
					}

					/* 95 % CI Error Bar */
					CheckBox {
						name: "addCI95ErrorBar"
						label: qsTr("95% CI error bar")
						info: qsTr("Add 95 % CI error bars")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "dodgeCI95ErrorBar";   label: qsTr("Dodge");        defaultValue: 0.8 }
						DoubleField { name: "widthCI95ErrorBar";   label: qsTr("Width");        defaultValue: 0.3 }
						DoubleField { name: "linewidthCI95ErrorBar"; label: qsTr("Line width"); defaultValue: 1 }
						DoubleField { name: "transparencyCI95ErrorBar"; label: qsTr("Transparency"); defaultValue: 1 }
						CheckBox    { name: "blackOutlineCI95ErrorBar"; label: qsTr("Black lines") }
					}
				}

				GridLayout {
					columns: 2
					rowSpacing: 40
					columnSpacing: 70

					/* Range Ribbon */
					CheckBox {
						name: "addRangeRibbon"
						label: qsTr("Range ribbon")
						info: qsTr("Add range ribbons")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "alphaRangeRibbon"; label: qsTr("Transparency"); defaultValue: 0.4; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineRangeRibbon"; label: qsTr("Black lines") }
					}

					/* SD Ribbon */
					CheckBox {
						id: addSdRibbon
						name: "addSdRibbon"
						label: qsTr("SD ribbon")
						info: qsTr("Add SD ribbons")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "alphaSdRibbon"; label: qsTr("Transparency"); defaultValue: 0.4; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineSdRibbon"; label: qsTr("Black lines"); enabled: addSdRibbon.checked }
					}

					/* SEM Ribbon */
					CheckBox {
						name: "addSemRibbon"
						label: qsTr("SEM ribbon")
						info: qsTr("Add SEM ribbons")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "alphaSemRibbon"; label: qsTr("Transparency"); defaultValue: 0.4; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineSemRibbon"; label: qsTr("Black lines") }
					}

					/* 95 % CI Ribbon */
					CheckBox {
						name: "addCi95Ribbon"
						label: qsTr("95% CI ribbon")
						info: qsTr("Add 95 % CI ribbons")
						enabled: (
									  (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
								   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
								 )
						onEnabledChanged: { if (!enabled) checked = false }

						DoubleField { name: "alphaCi95Ribbon"; label: qsTr("Transparency"); defaultValue: 0.4; min: 0; max: 1 }
						CheckBox    { name: "blackOutlineCi95Ribbon"; label: qsTr("Black lines") }
					}
				}
			}


			// -------------------------------------------------------------------
			// Curve fit and reference lines
			// -------------------------------------------------------------------
			Section {
				title: qsTr("Curve fit and reference lines")

				info: qsTr("Fit trend lines or statistical models and add reference or identity lines for comparison. "
						 + "Curve fitting requires individual data points. "
						 + "Common controls: <b>Method</b> chooses the model, <b>Line width</b> adjusts thickness, "
						 + "<b>Dodge</b> separates groups, <b>Transparency</b> sets opacity, and <b>Color</b> selects the line colour")

				Label {
					text: qsTr("Required: individual data points (Curve fit)")
					wrapMode: Text.Wrap
					color: "black"
				}

				CheckBox {
					name: "addCurveFitPlotBuilder"
					label: qsTr("Add curve fit")
					info: qsTr("Fit a model to the data points")
					enabled: addDataPoint.checked
					onEnabledChanged: { if (!enabled) checked = false }
					columns: 3

					Group {
						DropDown {
							name: "curvaFitMethod"
							label: qsTr("Method")
							id: curvaFitMethod
							indexDefaultValue: 0
							values: [
								{ label: qsTr("Linear"), value: "lm" },
								{ label: qsTr("LOESS"),  value: "loess" }
							]
						}
						DoubleField {
							name: "linewidthCurveFit"
							label: qsTr("Line width")
							defaultValue: 1
						}
					}

					Group {
						DoubleField { name: "dodgeCurveFit";        label: qsTr("Dodge");        defaultValue: 0.8; min: 0; max: 1 }
						DoubleField { name: "transparencyCurveFit"; label: qsTr("Transparency"); defaultValue: 0.2; min: 0; max: 1 }
					}

					Group {
						CheckBox { name: "seCurveFit";           label: qsTr("SE");          checked: true }
						CheckBox { name: "blackOutlineCurveFit"; label: qsTr("Black lines") }
					}
				}

				CheckBox {
					name: "addReferenceLinePlotBuilder"
					label: qsTr("Add reference line")
					info: qsTr("Add reference lines")
					columns: 2

					Group {
						TextField { name: "xReferenceLine"; label: qsTr("X-axis intercept"); placeholderText: qsTr("e.g. 0.5, or c(1,2)"); fieldWidth: 100 }
						TextField { name: "yReferenceLine"; label: qsTr("Y-axis intercept"); placeholderText: qsTr("e.g. 0.5, or c(1,2)"); fieldWidth: 100 }
					}

					Group {
						DoubleField { name: "linewidhtReferenceLines"; label: qsTr("Line width"); defaultValue: 1 }
						TextField   { name: "colorReferenceLine";      label: qsTr("Line color"); placeholderText: qsTr("e.g. black, #ff5733"); defaultValue: "lightgray"; fieldWidth: 100 }

						Label {
							text: qsTr("Note: For available colors, see %1this page%2")
								  .arg("<a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>")
								  .arg("</a>")
							wrapMode: Text.Wrap
							textFormat: Text.RichText
							MouseArea { anchors.fill: parent; onClicked: { Qt.openUrlExternally("https://r-charts.com/colors/") } }
						}
					}
				}

				CheckBox {
					name: "addIdentityLinePlotBuilder"
					label: qsTr("Add identity line")
					info: qsTr("Add identity line (y = x)")
					CheckBox { name: "reversedirectionIdentityLine"; label: qsTr("Reverse direction") }
					TextField {
						name: "colorIdentityLine"; label: qsTr("Line color");
						placeholderText: qsTr("e.g. black, #ff5733"); defaultValue: "lightgray"; fieldWidth: 100
					}
				}

				Label {
					text: qsTr("Note: For available colors, see %1this page%2")
						  .arg("<a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>")
						  .arg("</a>")
					wrapMode: Text.Wrap
					textFormat: Text.RichText
					MouseArea { anchors.fill: parent; onClicked: { Qt.openUrlExternally("https://r-charts.com/colors/") } }
				}
			}


			Label {
				text: qsTr("Axis settings and annotations")
				wrapMode: Text.Wrap
				color: "black"
				font.bold: true
			}

			// -------------------------------------------------------------------
			// Adjust axis
			// -------------------------------------------------------------------
			Section {
				columns: 3
				title: qsTr("X-Axis")

				/* concise, section-level help */
				info: qsTr("Fine-tune the x-axis: <b>Title</b> adds a heading, "
						 + "<b>Limits</b> sets the range, <b>Breaks</b> positions tick marks, "
						 + "<b>Labels</b> handle rotation, shortening and sorting, and "
						 + "<b>Rename labels</b> lets you edit them manually")

				TextField {
					label: qsTr("Title")
					name: "titleXPlotBuilder"
					placeholderText: qsTr("Enter x-axis title")
					fieldWidth: 300          /* per earlier code */
					/* per request, no per-field info here */
					Layout.columnSpan: 3
				}

				Group {
					title: qsTr("Limits")
					columns: 1
					TextField { name: "limitFromX"; label: qsTr("From"); fieldWidth: 40 }
					TextField { name: "limitToX";   label: qsTr("To");   fieldWidth: 40 }
				}

				Group {
					title: qsTr("Breaks")
					columns: 1
					TextField { name: "breakFromX"; label: qsTr("From"); fieldWidth: 40 }
					TextField { name: "breakToX";   label: qsTr("To");   fieldWidth: 40 }
					TextField { name: "breakByX";   label: qsTr("By");   fieldWidth: 40 }
				}

				Group {
					columns: 1
					title: qsTr("Labels")

					CheckBox { name: "rotateXLa	bel"; label: qsTr("Rotate") }

					CheckBox {
						name: "cutShortScale"
						label: qsTr("Shorten")
						info: qsTr("Shorten axis labels using K for thousand, M for million, and so on")
					}

					CheckBox {
						name: "enableSort"
						label: qsTr("Sort")
						checked: false

						DropDown {
							name: "sortXLabelsOrder"
							label: qsTr("Order")
							values: ["Increasing", "Decreasing"]
							startValue: "Increasing"
						}

						DropDown {
							name: "aggregationFun"
							label: qsTr("By")
							values: ["mean", "median"]
							startValue: "Mean"
						}
					}
				}

				ComponentsList {
					name: "xAxisLabelRenamer"
					title: qsTr("Rename labels")
					addItemManually: true
					minimumItems: 0
					rowComponent: Row {
						TextField { name: "originalXLabel"; label: qsTr("Original label"); fieldWidth: 100 }
						TextField { name: "newXLabel";      label: qsTr("New label");      fieldWidth: 150 }
					}
				}
			}


			Section {
				columns: 3
				title: qsTr("Y-Axis")

				/* concise, section-level help */
				info: qsTr("Fine-tune the y-axis: <b>Title</b> adds a heading, "
						 + "<b>Limits</b> sets the range, <b>Breaks</b> positions tick marks, "
						 + "<b>Labels</b> handle rotation, shortening and sorting, and "
						 + "<b>Rename labels</b> lets you edit them manually")

				TextField {
					label: qsTr("Title")
					name: "titleYPlotBuilder"
					placeholderText: qsTr("Enter y-axis title")
					fieldWidth: 300
					Layout.columnSpan: 3
				}

				Group {
					title: qsTr("Limits")
					columns: 1
					TextField { name: "limitFromY"; label: qsTr("From"); fieldWidth: 40 }
					TextField { name: "limitToY";   label: qsTr("To");   fieldWidth: 40 }
				}

				Group {
					title: qsTr("Breaks")
					columns: 1
					TextField { name: "breakFromY"; label: qsTr("From"); fieldWidth: 40 }
					TextField { name: "breakToY";   label: qsTr("To");   fieldWidth: 40 }
					TextField { name: "breakByY";   label: qsTr("By");   fieldWidth: 40 }
				}

				Group {
					title: qsTr("Labels")
					columns: 1

					CheckBox { name: "rotateYLabel"; label: qsTr("Rotate") }

					CheckBox {
						name: "cutShortScaleY"
						label: qsTr("Shorten")
						info: qsTr("Shorten axis labels using K for thousand, M for million, and so on")
					}

					CheckBox {
						name: "enableSortY"
						label: qsTr("Sort")
						checked: false

						DropDown {
							name: "sortYLabelsOrder"
							label: qsTr("Order")
							values: ["Increasing", "Decreasing"]
							startValue: "Increasing"
						}

						DropDown {
							name: "aggregationFunY"
							label: qsTr("By")
							values: ["mean", "median"]
							startValue: "mean"
						}
					}
				}

				ComponentsList {
					name: "yAxisLabelRenamer"
					title: qsTr("Rename labels")
					addItemManually: true
					minimumItems: 0

					rowComponent: Row {
						TextField { name: "originalYLabel"; label: qsTr("Original label"); fieldWidth: 100 }
						TextField { name: "newYLabel";      label: qsTr("New label");      fieldWidth: 150 }
					}
				}
			}



			// -------------------------------------------------------------------
			// Title, caption, annotation
			// -------------------------------------------------------------------
			Section {
				title: qsTr("Title, caption")
				columns: 1

				info: qsTr("Use these fields to add text to your figure. "
						 + "<b>Title</b> appears above the plot, while <b>Caption</b> appears below")

				Group {
					TextField {
						name: "titlePlotBuilder"
						label: qsTr("Title")
						placeholderText: qsTr("Enter the plot title here")
						fieldWidth: 300
					}
				}

				TextArea {
					title: qsTr("Caption")
					name: "captionPlotBuilder"
					height: 100
				}
			}


			Section {
				text: qsTr("Annotation and data label")

				info: qsTr("Label points automatically or place custom annotations anywhere. "
						 + "<b>Label variable</b> shows each point’s value; <b>Custom annotations</b> add free-form text with position controls. "
						 + "Common controls: <b>Size</b> adjusts text height, <b>White background</b> improves legibility, colour fields accept named or hex colours")

				Label {
					text: qsTr("Label data by variable")
					wrapMode: Text.Wrap
					color: "black"
				}

				VariablesForm {
					enabled: addDataPoint.checked
					preferredWidth: jaspForm.width - 2 * jaspTheme.contentMargin
					preferredHeight: 100 * preferencesModel.uiScale

					AvailableVariablesList {
						name: "labelVars"
						source: [
							{ name: "allVariablesList",   use: ["type=scale","type=ordinal","type=nominal"] },
							{ name: "allVariablesListRM", use: ["type=scale","type=ordinal","type=nominal"] }
						]
					}

					AssignedVariablesList {
						name:           "labelVariablePlotBuilder"
						title:          qsTr("Label variable")
						allowedColumns: ["scale","ordinal","nominal"]
						singleVariable: true
						enabled: addDataPoint.checked
						onEnabledChanged: {
							if (!enabled) {
								while (count > 0) {
									itemDoubleClicked(0)
								}
							}
						}
						info: qsTr("Variable whose values will be shown next to each point")
					}

					Group {
						columns: 2
						DoubleField {
							name:  "fontsizeDataLabels"
							label: qsTr("Size")
							value: 4
							fieldWidth: 50
						}
						CheckBox {
							name:  "backgroundDataLabels"
							label: qsTr("White background")
							checked: false
							Layout.fillWidth: false
						}
					}
				}

				ComponentsList {
					name: "annotationPlotBuilder"
					id: annotationPlotBuilder
					title: qsTr("Custom annotations")
					addItemManually: true
					minimumItems: 0
					enabled: (
								  (noRM.checked && (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0))
							   || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
							 )
					onEnabledChanged: {
						if (!enabled) {
							for (var i = annotationPlotBuilder.count - 1; i >= 0; --i) {
								annotationPlotBuilder.removeItem(i)
							}
						}
					}

					rowComponent: Row {
						Group {
							title: qsTr("Annotation ") + (rowIndex + 1)
							columns: 4

							Group {
								title: qsTr("Label")
								TextField {
									name: "annotationText"
									label: qsTr("Text")
									placeholderText: qsTr("e.g. p = 0.1, $italic(p)==0.1$")
									fieldWidth: 100
								}
							}

							Group {
								columns: 2
								title: qsTr("Position")

								Group {
									DoubleField {
										name: "annotationX"
										label: qsTr("X-Axis")
										defaultValue: 3
										fieldWidth: 60
									}
									DoubleField {
										name: "annotationY"
										label: qsTr("Y-Axis")
										defaultValue: 3
										fieldWidth: 60
									}
								}

								Group {
									DropDown {
										visible: noRM.checked
										name: "ColumnAnnotation"
										label: qsTr("Column")
										values: columnsvariableSplitPlotBuilder.levels
										enabled: noRM.checked
										onEnabledChanged: { if (!enabled) currentIndex = -1 }
									}

									DropDown {
										name: "RMColumnAnnotation"
										title: qsTr("Column")
										visible: yesRM.checked
										enabled: yesRM.checked
										values: colSplitRM.columnsNames.length > 0
												? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(colSplitRM.columnsNames[0])
												   ? repeatedMeasuresFactors.factorLevelMap[colSplitRM.columnsNames[0]]
												   : colSplitRM.levels)
												: []
										addEmptyValue: groupValue.columnsNames.length === 0
										placeholderText: qsTr("<No Value>")
										onEnabledChanged: { if (!enabled) currentIndex = -1 }
									}

									DropDown {
										name: "RowAnnotation"
										label: qsTr("Row")
										values: rowsvariableSplitPlotBuilder.levels
										enabled: noRM.checked
										visible: noRM.checked
										onEnabledChanged: { if (!enabled) currentIndex = -1 }
									}

									DropDown {
										name: "RMRowAnnotation"
										title: qsTr("Row")
										visible: yesRM.checked
										enabled: yesRM.checked
										values: rowSplitRM.columnsNames.length > 0
												? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(rowSplitRM.columnsNames[0])
												   ? repeatedMeasuresFactors.factorLevelMap[rowSplitRM.columnsNames[0]]
												   : rowSplitRM.levels)
												: []
										addEmptyValue: groupValue.columnsNames.length === 0
										placeholderText: qsTr("<No Value>")
										onEnabledChanged: { if (!enabled) currentIndex = -1 }
									}

									DropDown {
										name: "GridAnnotation"
										label: qsTr("Grid")
										values: gridVariablePlotBuilder.levels
										enabled: noRM.checked
										visible: noRM.checked
										onEnabledChanged: { if (!enabled) currentIndex = -1 }
									}

									DropDown {
										name: "RMGridAnnotation"
										title: qsTr("Grid")
										visible: yesRM.checked
										enabled: yesRM.checked
										values: gridVarRM.columnsNames.length > 0
												? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(gridVarRM.columnsNames[0])
												   ? repeatedMeasuresFactors.factorLevelMap[gridVarRM.columnsNames[0]]
												   : gridVarRM.levels)
												: []
										addEmptyValue: groupValue.columnsNames.length === 0
										placeholderText: qsTr("<No Value>")
										onEnabledChanged: { if (!enabled) currentIndex = -1 }
									}
								}
							}

							Group {
								title: qsTr("Appearance")
								DoubleField {
									name: "annotationSize"
									label: qsTr("Size")
									defaultValue: 5.5
								}
								TextField {
									name: "colorText"
									label: qsTr("Color")
									placeholderText: qsTr("e.g. black, #ff5733")
									defaultValue: "black"
									fieldWidth: 60
								}
							}
						}
					}
				}
			}



			Label {
				text: qsTr("Theme, colors, size, and legend")
				wrapMode: Text.Wrap
				color: "black"
				font.bold: true
			}


			Section {
				title: qsTr("Theme and color")

				/* section-level help */
				info: qsTr("Choose an overall visual style and how colours are assigned. "
						 + "<b>Theme</b> applies a preset look, while <b>Font base size</b> scales all text. "
						 + "<b>Color settings</b> lets you map colour to a variable, pick a palette, or enter custom colours")

				/* ────────────────── Theme & font size ────────────────── */
				Group {
					columns: 2

					DropDown {
						name: "plotStyle"
						label: qsTr("Theme")
						values: ["JASP", "ggplotgray", "ggpubr", "PlotBuilder"]
						indexDefaultValue: 0
					}

					DoubleField {
						name: "baseFontSize"
						label: qsTr("Font base size")
						value: 18
					}
				}

				/* ────────────────── Colour mapping and palettes ────────────────── */
				Group {
					title: qsTr("Color settings")
					info: qsTr("Decide which variable drives colours and select a palette or provide custom hex/named colours")
					columns: 1

					RadioButtonGroup {
						name: "colorByGroup"
						title: qsTr("Color by")
						info: qsTr("Choose what determines element colours")
						radioButtonsOnSameRow: true
						columns: 4

						RadioButton {
							value: "none"
							label: qsTr("None")
							info: qsTr("Uniform colors")
							enabled: true
							checked: !colorXRadio.checked && !colorYRadio.checked
							onEnabledChanged: { if (!enabled && checked) checked = false }
						}
						RadioButton {
							value: "grouping"
							label: qsTr("Group variable")
							info: qsTr("Coloring by Group variable")
							enabled: variableColorPlotBuilder.count > 0 || groupVarRM.count > 0
							checked: variableColorPlotBuilder.count > 0 || groupVarRM.count > 0
							onEnabledChanged: { if (!enabled && checked) checked = false }
						}
						RadioButton {
							id: colorXRadio
							value: "x"
							label: qsTr("X variable")
							info: qsTr("Coloring by X variable")
							enabled: (variableXPlotBuilder.count > 0 && variableColorPlotBuilder.count === 0) || (xVarRM.count > 0 && groupVarRM.count === 0)
							checked: false
							onEnabledChanged: { if (!enabled && checked) checked = false }
						}
						RadioButton {
							id: colorYRadio
							value: "y"
							label: qsTr("Y variable")
							info: qsTr("Coloring by Y variable")
							enabled: (variableYPlotBuilder.count > 0 && variableColorPlotBuilder.count === 0) || (groupVarRM.count === 0)
							checked: false
							onEnabledChanged: { if (!enabled && checked) checked = false }
						}
						RadioButton {
							value: "splitColumn"
							info: qsTr("Apply separate colours in different splits")
							label: qsTr("Split (column)")
							enabled: columnsvariableSplitPlotBuilder.count > 0 || colSplitRM.count > 0
							checked: false
							onEnabledChanged: { if (!enabled && checked) checked = false }
						}
						RadioButton {
							value: "splitRow"
							label: qsTr("Split (rows)")
							info: qsTr("Apply separate colours in different splits")
							enabled: rowsvariableSplitPlotBuilder.count > 0 || rowSplitRM.count > 0
							checked: false
							onEnabledChanged: { if (!enabled && checked) checked = false }
						}
					}

					DropDown {
						name: "colorsAll"
						label: qsTr("Color schemes")
						indexDefaultValue: 0
						values: [
							{ label: qsTr("JASP colors: jaspPalette"), value: "jaspPalette" },
							{ label: qsTr("JASP colors: colorblind"), value: "colorblind" },
							{ label: qsTr("JASP colors: colorblind2"), value: "colorblind2" },
							{ label: qsTr("JASP colors: colorblind3"), value: "colorblind3" },
							{ label: qsTr("JASP colors: sportsTeamsNBA"), value: "sportsTeamsNBA" },
							{ label: qsTr("JASP colors: ggplot2"), value: "ggplot2" },
							{ label: qsTr("JASP colors: grandBudapest"), value: "grandBudapest" },
							{ label: qsTr("JASP colors: gray"), value: "gray" },
							{ label: qsTr("JASP colors: blue"), value: "blue" },
							{ label: qsTr("Discrete: Friendly"), value: "colors_discrete_friendly" },
							{ label: qsTr("Discrete: Seaside"), value: "colors_discrete_seaside" },
							{ label: qsTr("Discrete: Apple"), value: "colors_discrete_apple" },
							{ label: qsTr("Discrete: Friendly Long"), value: "colors_discrete_friendly_long" },
							{ label: qsTr("Discrete: Okabe Ito"), value: "colors_discrete_okabeito" },
							{ label: qsTr("Discrete: IBM"), value: "colors_discrete_ibm" },
							{ label: qsTr("Discrete: Metro"), value: "colors_discrete_metro" },
							{ label: qsTr("Discrete: Candy"), value: "colors_discrete_candy" },
							{ label: qsTr("Discrete: Alger"), value: "colors_discrete_alger" },
							{ label: qsTr("Discrete: Rainbow"), value: "colors_discrete_rainbow" },
							{ label: qsTr("Continuous: Viridis"), value: "colors_continuous_viridis" },
							{ label: qsTr("Continuous: Magma"), value: "colors_continuous_magma" },
							{ label: qsTr("Continuous: Inferno"), value: "colors_continuous_inferno" },
							{ label: qsTr("Continuous: Plasma"), value: "colors_continuous_plasma" },
							{ label: qsTr("Continuous: Cividis"), value: "colors_continuous_cividis" },
							{ label: qsTr("Continuous: Rocket"), value: "colors_continuous_rocket" },
							{ label: qsTr("Continuous: Mako"), value: "colors_continuous_mako" },
							{ label: qsTr("Continuous: Turbo"), value: "colors_continuous_turbo" },
							{ label: qsTr("Continuous: Blue Pink Yellow"), value: "colors_continuous_bluepinkyellow" },
							{ label: qsTr("Diverging: Blue to Red"), value: "colors_diverging_blue2red" },
							{ label: qsTr("Diverging: Blue to Brown"), value: "colors_diverging_blue2brown" },
							{ label: qsTr("Diverging: BuRd"), value: "colors_diverging_BuRd" },
							{ label: qsTr("Diverging: BuYlRd"), value: "colors_diverging_BuYlRd" },
							{ label: qsTr("Diverging: Spectral"), value: "colors_diverging_spectral" },
							{ label: qsTr("Diverging: Icefire"), value: "colors_diverging_icefire" }
						]
					}

					TextField {
						name: "customColors"
						label: qsTr("Custom colors")
						info: qsTr("Here you can specify custom colors separated by commas")
						placeholderText: qsTr("e.g. red, blue, #ff5733")
						fieldWidth: 150
					}

					Label {
						text: qsTr("Note: For available colors, see %1this page%2")
								.arg("<a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>")
								.arg("</a>")
						wrapMode: Text.Wrap
						textFormat: Text.RichText
						MouseArea {
							anchors.fill: parent
							onClicked: { Qt.openUrlExternally("https://r-charts.com/colors/") }
						}
					}
				}
			}


			Section {
				title: qsTr("Size, margins and plot orientation")
				columns: 1

				/* section-level help */
				info: qsTr("Control the canvas dimensions and whitespace. "
						 + "<b>Plot size</b> sets the pixel width × height, "
						 + "<b>Axis padding</b> adds proportional space inside the axes, "
						 + "<b>Margins</b> add outer whitespace, and "
						 + "<b>Plot orientation</b> can flip X and Y")

				Group {
					title: qsTr("Plot size")
					info: qsTr("Width and height of the exported figure in pixels")
					columns: 2

					DoubleField {
						id: plotWidth
						name: "widthPlotBuilder"
						label: qsTr("Width (px)")
						defaultValue: 380
						fieldWidth: 50
					}

					DoubleField {
						id: plotHeight
						name: "heightPlotBuilder"
						label: qsTr("Height (px)")
						defaultValue: 300
						fieldWidth: 50
					}
				}

				Group {
					title: qsTr("Axis padding")
					info: qsTr("Fraction of the data range to leave empty inside each axis (0 – 1)")
					columns: 4

					DoubleField { name: "YPaddingSecond"; label: qsTr("Top");    value: 0.05 }
					DoubleField { name: "YPaddingFirst";  label: qsTr("Bottom"); value: 0.04 }
					DoubleField { name: "XPaddingFirst";  label: qsTr("Left");   value: 0.05 }
					DoubleField { name: "XPaddingSecond"; label: qsTr("Right");  value: 0.05 }
				}

				Group {
					title: qsTr("Margins")
					info: qsTr("Outer whitespace around the plot (pts)")
					columns: 4

					DoubleField { name: "topMargin";    label: qsTr("Top");    value: 10 }
					DoubleField { name: "bottomMargin"; label: qsTr("Bottom"); value: 10 }
					DoubleField { name: "leftMargin";   label: qsTr("Left");   value: 10 }
					DoubleField { name: "rightMargin";  label: qsTr("Right");  value: 10 }
				}

				Group {
					title: qsTr("Plot orientation")
					info: qsTr("Flip swaps the X and Y axes")
					CheckBox { name: "flipPlot"; label: qsTr("Flip plot") }
				}
			}


			// -------------------------------------------------------------------
			// Edit style and colors
			// -------------------------------------------------------------------
			Section {
				title: qsTr("Legend")

				info: qsTr("Control where the legend appears and what it shows. "
						 + "Use <b>Position</b> to dock the legend or hide it. "
						 + "<b>Remove title</b> hides the legend heading")

				Group {
					columns: 2

					DropDown {
						name: "legendPosistionPlotBuilder"
						label: qsTr("Position")
						id: legendPosistionPlotBuilder
						indexDefaultValue: 0
						fieldWidth: 150
						values: [
							{ label: qsTr("Right"),      value: "right" },
							{ label: qsTr("Left"),       value: "left" },
							{ label: qsTr("Bottom"),     value: "bottom" },
							{ label: qsTr("Top"),        value: "top" },
							{ label: qsTr("No legend"),  value: "none" }
						]
					}

					CheckBox {
						name: "removeLegendTitle"
						label: qsTr("Remove title")
					}
				}

				ComponentsList {
					name: "colorLabelRenamer"
					title: qsTr("Rename labels")
					info: qsTr("Edit the text of individual legend entries")
					addItemManually: true
					minimumItems: 0

					rowComponent: Row {
						TextField {
							name: "originalColorLabel"
							label: qsTr("Original label")
							fieldWidth: 100
						}
						TextField {
							name: "newColorLabel"
							label: qsTr("New label")
							fieldWidth: 100
						}
					}
				}
			}

			Section{
				title: qsTr("Order of geometries")

				/* key ↔︎ readable name list (one per line) */
				info: qsTr(
					"point: Point<br>"
				  + "histogram: Histogram<br>"
				  + "boxplot: Boxplot<br>"
				  + "violin: Violin plot<br>"
				  + "count_bar: Count bar<br>"
				  + "count_dash: Count dash<br>"
				  + "count_line: Count line<br>"
				  + "count_area: Count area<br>"
				  + "count_dot: Count dot<br>"
				  + "count_value: Count value<br>"
				  + "sum_bar: Sum bar<br>"
				  + "sum_dash: Sum dash<br>"
				  + "sum_line: Sum line<br>"
				  + "sum_area: Sum area<br>"
				  + "sum_dot: Sum dot<br>"
				  + "sum_value: Sum value<br>"
				  + "barstack: Bar stack<br>"
				  + "areastack: Area stack<br>"
				  + "mean_bar: Mean bar<br>"
				  + "mean_dash: Mean dash<br>"
				  + "mean_line: Mean line<br>"
				  + "mean_area: Mean area<br>"
				  + "mean_dot: Mean dot<br>"
				  + "mean_value: Mean value<br>"
				  + "median_bar: Median bar<br>"
				  + "median_dash: Median dash<br>"
				  + "median_line: Median line<br>"
				  + "median_area: Median area<br>"
				  + "median_dot: Median dot<br>"
				  + "median_value: Median value<br>"
				  + "range_errorbar: Range error bar<br>"
				  + "sd_errorbar: SD error bar<br>"
				  + "sem_errorbar: SEM error bar<br>"
				  + "ci95_errorbar: 95% CI error bar<br>"
				  + "range_ribbon: Range ribbon<br>"
				  + "sd_ribbon: SD ribbon<br>"
				  + "sem_ribbon: SEM ribbon<br>"
				  + "ci95_ribbon: 95% CI ribbon<br>"
				  + "curve_fit: Curve fit<br>"
				  + "stat_ellipse: Stat ellipse<br>"
				  + "rm_lines: RM lines<br>"
				  + "reference_lines: Reference lines<br>"
				  + "identity_line: Identity line<br>"
				  + "point_labels: Point labels"
				)

				Label {
					text: qsTr(
						"You can set the order of the added geometric/data layers here, similar to the example shown in the text field.\n"
					  + "The order of the layers is defined from left to right—meaning the rightmost layer will be drawn on top."
					)
					wrapMode: Text.Wrap
					color: "black"
				}

				TextField {
					label: qsTr("Layer order")
					name: "layerOrder"
					placeholderText: qsTr("point, boxplot")
					fieldWidth: 300
				}
			}


			Label {
				text: qsTr("Split and grid control")
				wrapMode: Text.Wrap
				color: "black"
				font.bold: true
			}

			Section {
				title: qsTr("Split control")

				/* section-level help */
				info: qsTr("Adjust how faceted panels share scales, axes and space. "
						 + "<b>Scale and axis settings</b> decide whether panels use the same range, which axis lines are drawn and where labels appear. "
						 + "<b>Layout settings</b> control panel sizes, the position of the highest-value table, optional marginal plots and custom axis titles")

				GridLayout {
					columns: 2
					rowSpacing: 10
					columnSpacing: 20
					enabled: columnsvariableSplitPlotBuilder.count > 0 || colSplitRM.count > 0
						  || rowsvariableSplitPlotBuilder.count > 0    || rowSplitRM.count > 0

					/* ────────── Scale and axis settings ────────── */
					Label { text: qsTr("Scale and axis settings") }

					Column {
						spacing: 10

						DropDown {
							name: "scales"
							label: qsTr("Scale range")
							info: qsTr("Choose whether facet panels share a common scale (<i>fixed</i>) or get individual ranges")
							values: [
								{ label: qsTr("shared across all facets"), value: "fixed" },
								{ label: qsTr("vary across X"),            value: "free_x" },
								{ label: qsTr("vary across Y"),            value: "free_y" },
								{ label: qsTr("vary across both axes"),    value: "free" }
							]
							startValue: "fixed"
						}

						DropDown {
							name: "axes"
							label: qsTr("Show axis lines")
							info: qsTr("Decide which facet panels display axis lines")
							values: [
								{ label: qsTr("only on outer panels"), value: "margins" },
								{ label: qsTr("on all X axes"),        value: "all_x" },
								{ label: qsTr("on all Y axes"),        value: "all_y" },
								{ label: qsTr("on all axes"),          value: "all" }
							]
							startValue: "margins"
						}

						DropDown {
							name: "axisLabels"
							label: qsTr("Axis label visibility")
							info: qsTr("Control which tick-label texts are shown")
							values: [
								{ label: qsTr("show on all axes"),        value: "all" },
								{ label: qsTr("only on outer axes"),      value: "margins" },
								{ label: qsTr("only on interior X axes"), value: "all_x" },
								{ label: qsTr("only on interior Y axes"), value: "all_y" }
							]
							startValue: "all"
						}
					}

					/* ────────── Layout settings ────────── */
					Label { text: qsTr("Layout settings") }

					Column {
						spacing: 10

						RadioButtonGroup {
							id: asTableGroup
							name: "asTable"
							title: qsTr("Highest value at")
							info: qsTr("Controls where the summary table of highest values is placed inside each facet")
							radioButtonsOnSameRow: true

							RadioButton {
								value: "bottom-right"
								label: qsTr("bottom right")
								checked: true
							}
							RadioButton {
								value: "top-right"
								label: qsTr("top right")
							}
						}

						DropDown {
							name: "space"
							label: qsTr("Panel size adjustment")
							info: qsTr("Allow facet panels to stretch freely along X, Y or both axes")
							values: [
								{ label: qsTr("same size"),               value: "fixed" },
								{ label: qsTr("free width (X)"),          value: "free_x" },
								{ label: qsTr("free height (Y)"),         value: "free_y" },
								{ label: qsTr("free width and height"),   value: "free" }
							]
							startValue: "fixed"
						}

						CheckBox {
							id: marginsCheckBox
							name: "margins"
							label: qsTr("Include marginal plots")
							info: qsTr("Adds aggregated rows/columns at the edges of the facet grid")
							checked: false
						}

						/* custom axis titles for split layouts */
						TextField {
							name: "xAxisTitleSplit"
							label: qsTr("X axis title")
							placeholderText: qsTr("Enter a label for the X axis")
						}
						TextField {
							name: "yAxisTitleSplit"
							label: qsTr("Y axis title")
							placeholderText: qsTr("Enter a label for the Y axis")
						}
					}
				}
			}


			Section {
				title: qsTr("Grid control")

				/* section-level help */
				info: qsTr("Arrange panels when a grid variable is mapped. "
						 + "<b>Layout settings</b> set the facet grid’s rows, columns and where the highest-values table appears, "
						 + "<b>Scales and strip settings</b> decide whether panels share axes and where the facet label strip is placed")

				GridLayout {
					enabled: gridVariablePlotBuilder.count > 0 || gridVarRM.count > 0
					columns: 2
					rowSpacing: 10
					columnSpacing: 20

					Label { text: qsTr("Layout settings") }

					Column {
						spacing: 10

						DoubleField {
							name: "ncolFacetWrap"
							label: qsTr("Number of columns")
							info: qsTr("Fixed number of facet columns; 0 lets the layout pick automatically")
							defaultValue: gridVariablePlotBuilder.levels ? Math.floor(gridVariablePlotBuilder.levels / 2) + 1 : 0
						}

						DoubleField {
							name: "nrowFacetWrap"
							label: qsTr("Number of rows")
							info: qsTr("Fixed number of facet rows; 0 lets the layout pick automatically")
							defaultValue: gridVariablePlotBuilder.levels ? Math.floor(gridVariablePlotBuilder.levels / 2) + 1 : 0
						}

						RadioButtonGroup {
							name: "asTableFacetWrap"
							title: qsTr("Highest value at")
							info: qsTr("Choose the corner where the summary table of highest values is drawn")
							radioButtonsOnSameRow: true

							RadioButton {
								value: "bottom-rightFacetWrap"
								label: qsTr("bottom right")
								checked: true
							}
							RadioButton {
								value: "top-rightFacetWrap"
								label: qsTr("top right")
							}
						}
					}

					Label { text: qsTr("Scales and strip settings") }

					Column {
						spacing: 10

						DropDown {
							name: "scalesFacetWrap"
							label: qsTr("Scale range")
							info: qsTr("Fixed panels share axes; free panels get independent X and/or Y scales")
							values: [
								{ label: qsTr("shared across all facets"), value: "fixed" },
								{ label: qsTr("vary across X"),            value: "free_x" },
								{ label: qsTr("vary across Y"),            value: "free_y" },
								{ label: qsTr("vary across both axes"),    value: "free" }
							]
							startValue: "fixed"
						}

						DropDown {
							name: "stripPosition"
							label: qsTr("Strip position")
							info: qsTr("Location of the facet label strip")
							values: [
								{ label: qsTr("top"),    value: "top" },
								{ label: qsTr("bottom"), value: "bottom" },
								{ label: qsTr("left"),   value: "left" },
								{ label: qsTr("right"),  value: "right" }
							]
							startValue: "top"
						}
					}
				}
			}


			Label {
				text: qsTr("P value and comparison lines")
				wrapMode: Text.Wrap
				color: "black"
				font.bold: true
			}

			Section {

				title: qsTr("P value brackets")
				columns: 3

				/* section-level help */
				info: qsTr("Add brackets with P values to show pairwise significance.\n"
						 + "Set colour, text size and vertical spacing, then list each comparison in the table below")

				Label {
					text: qsTr("Required: X AND Y-Axis Variables")
					wrapMode: Text.Wrap
					color: "black"
				}

				Group {
					columns: 2

					/* ── Text appearance ── */
					Group {
						columns: 1
						info: qsTr("Colour and font size of the P-value text")

						TextField {
							name: "labelcolor"
							label: qsTr("P value color")
							info: qsTr("Named or hex colour for the P value text")
							fieldWidth: 70
							defaultValue: "black"
						}

						DoubleField {
							name: "labelSizePValue"
							label: qsTr("P value size")
							info: qsTr("Font size of the P value text")
							defaultValue: 4.5
						}
					}

					/* ── Vertical placement ── */
					Group {
						columns: 1
						info: qsTr("Starting height and step between stacked brackets (data units)")

						DoubleField {
							name: "yPositionPValue"
							label: qsTr("Y-Axis position of the first P value")
							info: qsTr("Initial vertical position for the first bracket")
							decimals: 2
							fieldWidth: 70
							value: 70
						}

						DoubleField {
							name: "stepDistance"
							label: qsTr("Step distance")
							info: qsTr("Vertical gap between successive brackets")
							decimals: 2
							fieldWidth: 70
							value: 0.15
						}

						Label {
							text: qsTr("Note: If custom Y-Axis limits are set, the starting\n"
									   + "position for the P value must fall within the defined interval")
							wrapMode: Text.Wrap
							color: "black"
						}
					}
				}

				ComponentsList {
					name: "pairwiseComparisons"
					title: qsTr("Pairwise comparisons table")
					id: pairwiseComparisons
					addItemManually: true
					minimumItems: 0
					maximumItems: -1

					enabled: (
								 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
							 )

					onEnabledChanged: {
						if (!enabled) {
							for (var i = pairwiseComparisons.count - 1; i >= 0; --i)
								pairwiseComparisons.removeItem(i)
						}
					}

					rowComponent: Row {

						Group {
							title: qsTr("Bracket ") + (rowIndex + 1)
							columns: 3

							/* Compared groups ------------------------------------ */
							Group {
								title: qsTr("Compared groups")
								info: qsTr("Labels of the two groups being compared")

								TextField {
									name: "group1"
									label: qsTr("Group 1")
									fieldWidth: 60
								}

								TextField {
									name: "group2"
									label: qsTr("Group 2")
									fieldWidth: 60
								}
							}

							/* P value & bracket style ---------------------------- */
							Group {
								title: qsTr("P value and brackets")
								info: qsTr("Enter the P value (e.g. 0.03) or a significance symbol (*, **, ***). "
										 + "<b>Tip length</b> is the size of the bracket tips; "
										 + "<b>Bracket size</b> is the horizontal width")

								TextField {
									name: "pAdj"
									label: qsTr("P value")
									fieldWidth: 60
									value: "* or 0.001"
								}

								DoubleField {
									name: "tipLengthPValue"
									label: qsTr("Tip length")
									decimals: 2
									fieldWidth: 70
									value: 0.03
								}

								DoubleField {
									name: "bracketSizePValue"
									label: qsTr("Bracket size")
									decimals: 2
									fieldWidth: 70
									value: 0.3
								}
							}

							/* Facet position ------------------------------------- */
							Group {
								title: qsTr("Position")
								columns: 2
								info: qsTr("Select the facet (group/column/row/grid) where the bracket should appear")

								/* group selectors */
								DropDown { name: "GroupPValue";  label: qsTr("Group");  values: variableColorPlotBuilder.levels; visible: noRM.checked; enabled: noRM.checked; onEnabledChanged: { if (!enabled) currentIndex = -1 } }
								DropDown { name: "RMGroupPValue"; label: qsTr("Group"); visible: yesRM.checked; enabled: yesRM.checked;
										   values: groupVarRM.columnsNames.length > 0
												   ? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(groupVarRM.columnsNames[0])
													  ? repeatedMeasuresFactors.factorLevelMap[groupVarRM.columnsNames[0]]
													  : groupVarRM.levels)
												   : [];
										   addEmptyValue: groupValue.columnsNames.length === 0; placeholderText: qsTr("<No Value>");
										   onEnabledChanged: { if (!enabled) currentIndex = -1 } }

								/* column selectors */
								DropDown { name: "ColumnPValue";  label: qsTr("Column"); values: columnsvariableSplitPlotBuilder.levels; visible: noRM.checked; enabled: noRM.checked; onEnabledChanged: { if (!enabled) currentIndex = -1 } }
								DropDown { name: "RMColumnPValue"; label: qsTr("Column"); visible: yesRM.checked; enabled: yesRM.checked;
										   values: colSplitRM.columnsNames.length > 0
												   ? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(colSplitRM.columnsNames[0])
													  ? repeatedMeasuresFactors.factorLevelMap[colSplitRM.columnsNames[0]]
													  : colSplitRM.levels)
												   : [];
										   addEmptyValue: groupValue.columnsNames.length === 0; placeholderText: qsTr("<No Value>");
										   onEnabledChanged: { if (!enabled) currentIndex = -1 } }

								/* row selectors */
								DropDown { name: "RowPValue";  label: qsTr("Row"); values: rowsvariableSplitPlotBuilder.levels; visible: noRM.checked; enabled: noRM.checked; onEnabledChanged: { if (!enabled) currentIndex = -1 } }
								DropDown { name: "RMRowPValue"; label: qsTr("Row"); visible: yesRM.checked; enabled: yesRM.checked;
										   values: rowSplitRM.columnsNames.length > 0
												   ? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(rowSplitRM.columnsNames[0])
													  ? repeatedMeasuresFactors.factorLevelMap[rowSplitRM.columnsNames[0]]
													  : rowSplitRM.levels)
												   : [];
										   addEmptyValue: groupValue.columnsNames.length === 0; placeholderText: qsTr("<No Value>");
										   onEnabledChanged: { if (!enabled) currentIndex = -1 } }

								/* grid selectors */
								DropDown { name: "GridPValue";  label: qsTr("Grid"); values: gridVariablePlotBuilder.levels; visible: noRM.checked; enabled: noRM.checked; onEnabledChanged: { if (!enabled) currentIndex = -1 } }
								DropDown { name: "RMGridPValue"; label: qsTr("Grid"); visible: yesRM.checked; enabled: yesRM.checked;
										   values: gridVarRM.columnsNames.length > 0
												   ? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(gridVarRM.columnsNames[0])
													  ? repeatedMeasuresFactors.factorLevelMap[gridVarRM.columnsNames[0]]
													  : gridVarRM.levels)
												   : [];
										   addEmptyValue: groupValue.columnsNames.length === 0; placeholderText: qsTr("<No Value>");
										   onEnabledChanged: { if (!enabled) currentIndex = -1 } }
							}
						}
					}
				}
			}


			Section {
				title: qsTr("Custom comparison lines")
				columns: 4
				/* section-level help */
				info: qsTr("Draw manual comparison lines (e.g., **min–max bars** or custom contrasts) with optional text labels")

				Label {
					text: qsTr("Required: X AND Y-Axis Variables")
					wrapMode: Text.Wrap
					color: "black"
				}

				Label {
					text: qsTr("Note: If custom Y-Axis limits are set, the starting\n"
							   + "position for the Y-Axis start and end values must fall within the defined interval")
					wrapMode: Text.Wrap
					color: "black"
				}

				ComponentsList {
					name: "annotationLineList"
					id: annotationLineList
					title: qsTr("Add annotation lines")
					addItemManually: true
					minimumItems: 0

					enabled: (
								 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)
							  || (yesRM.checked && repeatedMeasuresFactors.count > 0 && xVarRM.count > 0)
							 )

					onEnabledChanged: {
						if (!enabled) {
							for (var i = annotationLineList.count - 1; i >= 0; --i)
								annotationLineList.removeItem(i)
						}
					}

					rowComponent: Row {

						Group {
							columns: 4
							title: qsTr("Line ") + (rowIndex + 1)
							info: qsTr("Define one comparison line with optional label, coordinates and appearance")

							/* ── Label text ── */
							Group {
								title: qsTr("Label")
								TextField {
									name: "textAnnotationline"
									label: qsTr("Label")
									info: qsTr("Text shown near the line (leave empty for no label)")
									fieldWidth: 60
								}
							}

							/* ── Position ── */
							Group {
								title: qsTr("Position")
								columns: 4
								info: qsTr("Start/end coordinates and facet location")

								Group {
									DoubleField { name: "xAnnotation";  label: qsTr("X-Axis start"); info: qsTr("Line start on X axis") }
									DoubleField { name: "xendAnnotation"; label: qsTr("X-Axis end");  info: qsTr("Line end on X axis") }
								}
								Group {
									DoubleField { name: "yAnnotation";  label: qsTr("Y-Axis start"); info: qsTr("Line start on Y axis") }
									DoubleField { name: "yendAnnotation"; label: qsTr("Y-Axis end"); info: qsTr("Line end on Y axis") }
								}

								Group {columns: 1

									DropDown { name: "ColumnAnnotationCompLine"; label: qsTr("Column"); values: columnsvariableSplitPlotBuilder.levels;
											   visible: noRM.checked; enabled: noRM.checked;
											   info: qsTr("Facet column (for split layouts)"); onEnabledChanged: { if (!enabled) currentIndex = -1 } }
									DropDown { name: "RowAnnotationCompLine";    label: qsTr("Row");    values: rowsvariableSplitPlotBuilder.levels;
											   visible: noRM.checked; enabled: noRM.checked;
											   info: qsTr("Facet row (for split layouts)");    onEnabledChanged: { if (!enabled) currentIndex = -1 } }
									DropDown { name: "GridAnnotationCompLine";   label: qsTr("Grid");   values: gridVariablePlotBuilder.levels;
											   visible: noRM.checked; enabled: noRM.checked;
											   info: qsTr("Facet grid cell");                   onEnabledChanged: { if (!enabled) currentIndex = -1 } }

									/* facet selectors – RM */
									DropDown { name: "RMColumnCompLine"; label: qsTr("Column"); visible: yesRM.checked; enabled: yesRM.checked;
											   info: qsTr("Facet column (RM)"); values: colSplitRM.columnsNames.length > 0
														  ? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(colSplitRM.columnsNames[0])
															 ? repeatedMeasuresFactors.factorLevelMap[colSplitRM.columnsNames[0]]
															 : colSplitRM.levels)
														  : [];
											   addEmptyValue: groupValue.columnsNames.length === 0; placeholderText: qsTr("<No Value>");
											   onEnabledChanged: { if (!enabled) currentIndex = -1 } }

									DropDown { name: "RMRowCompLine"; label: qsTr("Row"); visible: yesRM.checked; enabled: yesRM.checked;
											   info: qsTr("Facet row (RM)"); values: rowSplitRM.columnsNames.length > 0
														  ? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(rowSplitRM.columnsNames[0])
															 ? repeatedMeasuresFactors.factorLevelMap[rowSplitRM.columnsNames[0]]
															 : rowSplitRM.levels)
														  : [];
											   addEmptyValue: groupValue.columnsNames.length === 0; placeholderText: qsTr("<No Value>");
											   onEnabledChanged: { if (!enabled) currentIndex = -1 } }

									DropDown { name: "RMGridCompLine"; label: qsTr("Grid"); visible: yesRM.checked; enabled: yesRM.checked;
											   info: qsTr("Facet grid cell (RM)"); values: gridVarRM.columnsNames.length > 0
														  ? (repeatedMeasuresFactors.factorLevelMap.hasOwnProperty(gridVarRM.columnsNames[0])
															 ? repeatedMeasuresFactors.factorLevelMap[gridVarRM.columnsNames[0]]
															 : gridVarRM.levels)
														  : [];
											   addEmptyValue: groupValue.columnsNames.length === 0; placeholderText: qsTr("<No Value>");
											   onEnabledChanged: { if (!enabled) currentIndex = -1 } }}
								/* facet selectors – noRM */

							}

							/* ── Appearance ── */
							Group {
								title: qsTr("Appearance")
								info: qsTr("Line colour and label styling")

								TextField {
									name: "colorAnnotationLine"
									label: qsTr("Line color")
									info: qsTr("Named or hex colour for the line")
									placeholderText: qsTr("e.g. black, #ff5733")
									defaultValue: "black"
									fieldWidth: 60
								}

								DoubleField {
									name: "textSizeAnnotationLine"
									label: qsTr("Label size")
									info: qsTr("Font size of the label text")
									defaultValue: 5.5
									fieldWidth: 60
								}

								DoubleField {
									name: "textDistanceAnnotationLine"
									label: qsTr("Label distance")
									info: qsTr("Vertical offset between the line and its label (data units)")
									defaultValue: 0.5
									fieldWidth: 60
								}
							}
						}
					}
				}
			}

		}
	}

	Section {
		title: qsTr("Plot layout")
		columns: 1

		/* section-level help */
		info: qsTr("Combine multiple plots in custom grids. "
				 + "<b>Arrange plots by column</b> builds the top part of the layout, "
				 + "<b>Arrange plots by row</b> builds the bottom. "
				 + "Specify plot IDs, relative sizes and optional common legends")

		Group {
			title: qsTr("Arrange plots by column")

			/* relative width string */
			TextField {
				name: "columnWidthInput"
				label: qsTr("Relative column widths")
				info: qsTr("Comma-separated numbers that set the width of each column")
				placeholderText: qsTr("1,1")
			}

			ComponentsList {
				id: rowSpecifications
				name: "rowSpecifications"
				title: qsTr("Specify layout")
				addItemManually: true
				Layout.preferredWidth: form.width - 2 * jaspTheme.generalAnchorMargin

				rowComponent: Row {
					Group {
						title: qsTr("Column ") + (rowIndex + 1)

						TextField {
							name: "plotIDs"
							label: qsTr("Plot IDs")
							info: qsTr("Comma-separated IDs referencing earlier plots")
							placeholderText: qsTr("Plot 1, Plot 2, ...")
						}

						TextField {
							name: "rowHeightsColumn"
							label: qsTr("Plot heights")
							info: qsTr("Relative heights of plots within this column")
							placeholderText: qsTr("1,1")
						}
					}

					Group {
						title: " "

						TextField {
							name: "labelsColumn"
							label: qsTr("Labels")
							info: qsTr("Optional plot labels (A, B, …)")
							placeholderText: qsTr("A, B, C, ...")
							fieldWidth: 150
						}

						CheckBox {
							name: "getCommonLegendColumn"
							label: qsTr("Collect legend")
							info: qsTr("Merge legends of all plots in this column")
						}
					}
				}
			}
		}

		Group {
			title: qsTr("Arrange plots by row")

			TextField {
				name: "relHeightWithinRowLayout"
				label: qsTr("Relative row heights")
				info: qsTr("Comma-separated numbers that set the height of each row")
				placeholderText: "1,1"
			}

			ComponentsList {
				id: fullRowSpecifications
				name: "fullRowSpecifications"
				title: qsTr("Specify layout")
				addItemManually: true
				Layout.preferredWidth: form.width - 2 * jaspTheme.generalAnchorMargin

				rowComponent: Row {
					Group {
						title: qsTr("Row ") + (rowIndex + 1)

						TextField {
							name: "plotIDsFullRow"
							label: qsTr("Plot IDs")
							info: qsTr("Comma-separated IDs referencing earlier plots")
							placeholderText: qsTr("Plot 1, Plot 2, ...")
						}

						TextField {
							name: "relWidthsFullRow"
							label: qsTr("Plot widths")
							info: qsTr("Relative widths of plots within this row")
							placeholderText: "1,1"
						}
					}

					Group {
						title: " "

						TextField {
							name: "labelsFullRow"
							label: qsTr("Labels")
							info: qsTr("Optional plot labels (A, B, …)")
							placeholderText: qsTr("A, B, C, ...")
							fieldWidth: 150
						}

						CheckBox {
							name: "getCommonLegendRows"
							label: qsTr("Collect legend")
							info: qsTr("Merge legends of all plots in this row")
						}
					}
				}
			}
		}

		Label {
			text: qsTr("Note: If you have column and row arrangement,\n"
					   + "the column will be the top part of the layout\n"
					   + "and the row will be the bottom part of the layout.")
			wrapMode: Text.Wrap
			color: "black"
		}

		/* label settings -------------------------------------------------- */
		Group {
			columns: 3
			title: qsTr("Label settings")

			DoubleField {
				name: "labelSize"
				label: qsTr("Label size")
				info: qsTr("Font size of subplot labels")
				value: 18
			}

			DoubleField {
				name: "labelDistance1"
				label: qsTr("Horizontal position")
				info: qsTr("Horizontal offset of labels (0–1, relative to plot width)")
				value: 0.05
				min: 0
				max: 1
			}

			DoubleField {
				name: "labelDistance2"
				label: qsTr("Vertical position")
				info: qsTr("Vertical offset of labels (0–1, relative to plot height)")
				value: 0.95
				min: 0
				max: 1
			}
		}

		/* additional layout sizing --------------------------------------- */
		Group {
			title: qsTr("Additional settings")

			Row {
				spacing: 20

				Group {
					TextField {
						name: "relativeHeight"
						label: qsTr("Column heights/row widths")
						info: qsTr("Relative heights of columns or widths of rows when combining column and row layouts")
						placeholderText: "1,1"
						fieldWidth: 150
					}

					DoubleField {
						name: "layoutWidth"
						label: qsTr("Width")
						info: qsTr("Overall width of the assembled layout (pixels)")
						value: 500
					}

					DoubleField {
						name: "layoutHeight"
						label: qsTr("Height")
						info: qsTr("Overall height of the assembled layout (pixels)")
						value: 500
					}

					DoubleField {
						name: "plotSpacing"
						label: qsTr("Spacing")
						info: qsTr("Gap between plots inside the layout (pixels)")
						value: 10
					}

					CheckBox {
						name: "getCommonLegend"
						label: qsTr("Collect legend across layout")
						info: qsTr("Place a single shared legend for all plots in the layout")
					}
				}
			}
		}
	}

}

// End Form
