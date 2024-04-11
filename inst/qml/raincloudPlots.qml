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
import JASP.Widgets

Form
{
	columns: 1

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesListOne" }
		AssignedVariablesList
		{ 
			name: 					"dependentVariables"
			title: 					qsTr("Dependent Variables")
			allowedColumns: 		["scale"]
			info:					qsTr("Select all the variables that you want to plot.")
		}
		AssignedVariablesList
		{
			name: 					"primaryFactor"
			title:					qsTr("Primary Factor")
			id: 					primaryFactor;
			suggestedColumns: 		["nominal", "ordinal"]
			singleVariable: 		true
			info:					qsTr("Its levels are shown on the x-axis (y-axis for horizontal plot).")
		}
		AssignedVariablesList
		{
			name: 					"secondaryFactor"
			title:					qsTr("Secondary Factor")
			id: 					secondaryFactor
			suggestedColumns: 		["nominal", "ordinal"]
			singleVariable: 		true
			info:					qsTr("Its levels are color coded.")
		}

		AssignedVariablesList
		{
			name: 					"covariate"
			title:					qsTr("Covariate")
			id: 					covariate
			suggestedColumns: 		["nominal", "ordinal", "scale"]
			singleVariable: 		true
			info:					qsTr("Points are color coded according to this.")
		}

		AssignedVariablesList
		{
			name: 					"observationId"
			title:					qsTr("ID")
			id: 					observationId
			suggestedColumns: 		["nominal", "ordinal", "scale"]
			singleVariable: 		true
			enabled:				primaryFactor.count === 1
			info:					qsTr(
										"Select a participant/observation ID in your dataset, " +
										"to connect individual observations (points) accross the levels of the primary factor. " +
										"Otherwise, you do not need this."
									)
		}

	}  // End variables form

	Label{ text: qsTr("Note: Data must be in long format.") }

	Section
	{
		title:   qsTr("General")
		columns: 3

		// Start top 2 rows

		Group
		{
			columns: 2

			Label        { text: qsTr("Color palette") 	   }
			ColorPalette
			{
				name:  		"colorPalette"
				label: 		""
				infoLabel: 	qsTr("Color palette")
				info:  		qsTr("How to color code the levels of the secondary factor.")
			}

			Label{ text: qsTr("Covariate palette"); enabled: covariate.count === 1 }
			ColorPalette
			{
				name:       		"covariatePalette"
				label:      		""
				enabled: 			covariate.count === 1
				indexDefaultValue:  3
				infoLabel:			qsTr("Covariate palette")
				info:  				qsTr("How to color code the covariate. 'Viridis' works good for both discrete and continuous covariates.")
			}
		}

		CheckBox
		{
			name: 					"colorAnyway"
			label:					qsTr("Apply color palette to primary factor")
			id: 					colorAnyway
			enabled: 				secondaryFactor.count === 0
			checked:				secondaryFactor.count === 0
			Layout.columnSpan: 		2
			info:					qsTr(
										"Applies the color palette to the levels of the primary factor. " +
										"Otherwise, the plot stays black and white.<br>" +
										"This option is superseeded by a secondary factor; then color coding is according to that."
									)
		}

		// End top 2 rows
		
		CheckBox
		{
			name: 				"horizontal"
			label: 				qsTr("Horizontal plot")
			Layout.columnSpan:  3
			info:				qsTr(
									"Plots the dependent variable axis horizontally, at the bottom of the plot.<br>" +
									"This is where the raincloud plot gets its name from: It will look like the points are raining from the violin and box (cloud)."
								)
		}

		CheckBox
		{
			name:   			"table"
			label:  			qsTr("Table with statistics")
			info:				qsTr(
									"Shows a table under the plot with statistics per cloud, like number of observations or median.<br>" +
									"If you select 'Mean' or 'Interval around mean' (see Advanced Section), they will also be shown here."
								)
			CheckBox
			{
				name:  			"tableBoxStatistics"
				label: 			qsTr("Box Statistics")
				checked: 		true
				info:			qsTr(
									"Shows the box statistics in the table: lower whisker, 25th percentile, median, 75th percentile, upper whisker.<br>" +
									"It can be helpful to un-check this if you are also working with 'Interval around mean' (see Advanced Section)."
								)
			}
		}

	}  // End section General



	Section
	{
		title:   qsTr("Cloud Elements")
		columns: 3

		CheckBox
		{
			name: 		"showVio"
			id: 		showVio
			text: 		qsTr("Show violin")
			checked: 	true
			info: 		qsTr(
							"Whether or not the violin should be shown. If un-checked, opacity is set to 0 and outline to 'none'."
						)
		}
		CheckBox
		{
			name: 		"showBox"
			id:   		showBox
			text: 		qsTr("Show box")
			checked: 	!(meanInterval.checked || meanIntervalCustom.checked)
			info: 		qsTr(
							"Whether or not the box should be shown. If un-checked, opacity is set to 0 and outline to 'none'."
						)
		}
		CheckBox
		{
			name: 		"showPoint"
			id: 		showPoint
			text: 		qsTr("Show point")
			checked: 	true
			info: 		qsTr(
							"Whether or not the points of a cloud should be shown. If un-checked, opacity is set to 0.<br>" +
							"If you have many, many points, it can be helpful to hide them." +

							"<br><br>" +

							"<h3>Element Settings</h3>" +  // All of this here, because infoLabel{} for DoubleField{} does not work.

							"<h4>Nudge</h4>" +
							"The nudge for violin, box, and point determines how far the elements are nudged from the center (axis tick).<br>" +
							"By default, the box is in the center (nudge = 0) and the violin is nudged to the right. " +
							"The points are nudged to the left of the box.<br>" +
							"With a custom orientation (see Advanced section), the points get centered (nudge fixed to 0). Violin and box are nudged depending on orientation: left or right." +

							"<h4>Height, Width, and Spread</h4>" +
							"These settings determine the respective properties of the elements:<br><br>" +
							"How high should the violin peak? (Does not change the proportions of the probability density.)<br>" +
							"With a custom orientation to the left (see Advanced Section), it can be " +
							"helpful to decrease the height if the violin overlaps with axis ticks.<br><br>" +
							"How wide should the box be?<br><br>" +
							"How wide should the points spread? I.e., how jittered should they be along the x-axis (y-axis if horizontal plot?.<br>" +
							"Note, that this still correctly shows the values of the points on the dependent variable axis.<br>" +
							"(If you would also like y-jitter see the 'Jitter' option for points.)" +

							"<h4>Box Padding</h4>" +
							"With a secondary factor, there will be a box for each factor level. With the padding, you can change the spacing between theses boxes.<br>" +
							"Increasing the padding will - visually - decrease the box width. If you want to keep box width constant, also increase the box width." +

							"<h4>Point Size</h4>" +
							"We recommend a greater size, the fewer points you have." +

							"<h4>Opacity</h4>" +
							"Increase this to _decrease_ the transparency of the respective element." +

							"<h4>Outline and Outline Width</h4>" +
							"Would you like the outline to match the 'Color palette', be 'black', or have 'none' at all?<br>" +
							"The width determines how thick the outline is." +

							"<h4>Violin Smoothing</h4>" +
							"This percentage determines the smoothness of the probability density.<br>" +
							"The lower, the stronger it is influenced by the presence/absence of individual points." +

							"<h4>Point Jitter</h4>" +
							"Use this with care!<br>" +
							"It slightly jitters the points along the dependent variable axis. This means that the position of the points no longer matches the values of the observations.<br>" +
							"If you want to have less overlap between the points while keeping an accurate visualization, increases the point spread.<br>" +
							"Using point jitter can be useful in some cases. For example, if you are working with likert data." +

							"<h4>ID Lines Width</h4>" +
							"Determines how thick the lines are."
						)
		}


		Group  // Start group Violin
		{
			title:   qsTr("Violin")
			columns: 2
			enabled: showVio.checked

				Label{ text: qsTr("Nudge") }
				DoubleField
				{
					name:				"vioNudge"
					defaultValue:		(!customSides.checked) ? 0.09 : 0.24
					negativeValues:		true
				}

				Label{ text: qsTr("Height") }
				DoubleField
				{
					name: 				"vioHeight"
					defaultValue: 		0.7
				}

				// Placeholder Start
				Label       { text: "empty"; 		opacity: 0 }
				DoubleField { name: "placeholder1"; opacity: 0 }  // For clean alignment, it does not work to omit this DoubleField and Layout.columnSpan: 2 the Label
				Label		{ text: "empty";    	opacity: 0 }
				DoubleField { name: "placeholder2"; opacity: 0 }
				// Placeholder End

				Label		 { text: qsTr("Opacity") 														}
				PercentField
				{
					name: "vioOpacity"
					fieldWidth: 40
					defaultValue: (showVio.checked) ? 50 : 0

				}

				Label{ text: qsTr("Outline") }
				DropDown
				{
					name: 				"vioOutline"
					indexDefaultValue: (showVio.checked) ? 0 : 2
					values:	[
							{ label: qsTr("Color palette"), value: "colorPalette" },
							{ label: qsTr("black"),         value: "black"        },
							{ label: qsTr("none"),          value: "none"         },
							]
				}

				Label 		{ text: qsTr("Outline width") 			   }
				DoubleField { name: "vioOutlineWidth"; defaultValue: 1 }

				// Placeholder Start
				Label		{ text: "empty"; 		opacity: 0 }
				DoubleField { name: "placeholder3"; opacity: 0 }
				// Placeholder End

				Label		 { text: qsTr("Smoothing") 									 }
				PercentField { name: "vioSmoothing";   fieldWidth: 40; defaultValue: 100 }
		}  // End group Violin


		Group  // Start group Box
		{
			title:   qsTr("Box")
			columns: 2
			enabled: showBox.checked

			Label{ text: qsTr("Nudge") }
			DoubleField
			{
				name:				"boxNudge"
				id:					boxNudge
				defaultValue:		(!customSides.checked) ? 0 : 0.15
				onValueChanged:		if (initialized) meanNudge.value = boxNudge.value
				negativeValues:		true
			}

			Label{ text: qsTr("Width") }
			
			DoubleField
			{
    			name: "boxWidth"
    			id: boxWidth
    			defaultValue: (secondaryFactor.count === 0) ? 0.1 : 0.2
    			onValueChanged: if (initialized) meanDistance.value = boxWidth.value
			}

			Label       { text: qsTr("Padding") 													  }
			DoubleField { name: "boxPadding"; defaultValue: (secondaryFactor.count === 0) ? 0.1 : 0.2 }

			// Placeholder Start
			Label		{ text: "empty"; 		opacity: 0 }
			DoubleField { name: "placeholder4"; opacity: 0 }
			// Placeholder End

			Label		 { text: qsTr("Opacity") 														}
			PercentField { name: "boxOpacity"; fieldWidth: 40; defaultValue: (showBox.checked) ? 50 : 0
			}

			Label{ text: qsTr("Outline") }
			DropDown
			{
				name: 	"boxOutline"
				indexDefaultValue: (showBox.checked) ? 0 : 2
				values:	[
						{ label: qsTr("Color palette"), value: "colorPalette" },
					   	{ label: qsTr("black"),         value: "black"		  },
					   	{ label: qsTr("none"),          value: "none"		  },
					   	]
			}

			Label{ text: qsTr("Outline width") }
			DoubleField
			{
				name: "boxOutlineWidth"
				id: boxOutlineWidth
				defaultValue: 1
				onValueChanged: intervalOutlineWidth.value = boxOutlineWidth.value
			}
		}  // End group Box


		Group  // Start group Point
		{
			title:   qsTr("Point")
			columns: 2
			enabled: showPoint.checked

			Label{ text: qsTr("Nudge"); enabled: (!customSides.checked) ? true : false }
			DoubleField
			{
				name:				"pointNudge"
				defaultValue:		(!customSides.checked) ? 0.15 : 0 // Is multiplied by -1 in the R script
				enabled:			(!customSides.checked) ? true : false
				negativeValues:		true
			}

			Label		{ text: qsTr("Spread") 					   }
			DoubleField { name: "pointSpread"; defaultValue: 0.065 }

			Label		{ text: qsTr("Size")				   }
			DoubleField { name: "pointSize"; defaultValue: 2.5 }

			// Placeholder Start
			Label		{ text: "empty"; 		opacity: 0 }
			DoubleField { name: "placeholder5"; opacity: 0 }
			// Placeholder End

			Label 		 {text: qsTr("Opacity")																}
			PercentField { name: "pointOpacity"; fieldWidth: 40; defaultValue: (showPoint.checked) ? 50 : 0 }

			// Placeholder Start
			Label       { text: "empty"; 		opacity: 0 }
			DoubleField { name: "placeholder6"; opacity: 0 }
			Label       { text: "empty"; 		opacity: 0 }
			DoubleField { name: "placeholder7"; opacity: 0 }
			Label       { text: "empty"; 		opacity: 0 }
			DoubleField { name: "placeholder8"; opacity: 0 }
			// Placeholder End

			Label	 { text: qsTr("Jitter")		  	   }
			CheckBox { name: "jitter"; 		id: jitter }
		}  // End group Point

		Label{ text: ""; Layout.columnSpan: 3 }  // Placeholder

		Group
		{
			title: qsTr("ID lines")
			enabled: observationId.count === 1
			columns: 2

			PercentField
			{
				name:					"observationIdLineOpacity"
				label:					qsTr("Opacity")
				fieldWidth: 			40
				defaultValue:			25
			}
		DoubleField{ name: "observationIdLineWidth";   label: qsTr("Width");   defaultValue: 1 }
		}
	}  // End section Cloud Elements



	Section
	{
		title:   qsTr("Axes, Caption, and Plot Size")
		columns: 3

		CheckBox
		{
			name:  				"customAxisLimits"
			label: 				qsTr("Custom limits for dependent variable axis:")
			Layout.columnSpan:  2
			childrenOnSameRow:  true
			info:				qsTr(
									"Use this with care!<br>" +
									"If your dataset has observations that lie outside of the specified, the plot will be truncated and it will not show these observations.<br>" +
									"The limits you specify may only be applied approximately. " +
									"For further fine-tuning of the axis, click the title of the plot " +
									"where it says the name of dependent variable.<br>" +
									"Then select 'Edit Image' in the drop down menu and " +
									"then go to the headers 'x-axis' or 'y-axis'."
								)
			
			DoubleField{ name: "lowerAxisLimit"; label: qsTr("from"); negativeValues: true; defaultValue: 0	}
			DoubleField{ name: "upperAxisLimit"; label: qsTr("to");   negativeValues: true; defaultValue: 1000 }
		}  // End CheckBox customAxisLimits
		HelpButton
		{
			toolTip:	qsTr(
							"Limits may only be applied approximately.\n" +
							"For further fine-tuning of the axis, click the title of the plot\n" +
							"where it says the name of dependent variable.\n" +
							"Then select 'Edit Image' in the drop down menu and\n" +
							"then go to the headers 'x-axis' or 'y-axis'."
						)
		}

		CheckBox{
			name: "showCaption"
			id: showCaption
			label: qsTr("Show caption")
			checked: true
			Layout.columnSpan: 3
			info:		qsTr(
							"The caption provides important information about the plot such as warnings, if there are any.<br>" +
							"We strongly recommend to leave this checkbox checked and to only un-check it if you are otherwise happy with the plot and want to export it." +

							"<h3>Plot Size: Width and Height</h3>" +  // Here and not below to combine the documentation for widthPlot and heightPlot
							"This also includes the legend (if there is color coding) and the caption.<br>" +
							"While you can also change the size of a JASP plot with your mouse, the precise specification of width and height allows you to exactly reproduce a plot."
						)
		}

		Group
		{
			title:   qsTr("Plot Size")
			columns: 2

			IntegerField
			{
				name: "widthPlot"
				label: qsTr("Width")
				defaultValue: if (
					secondaryFactor.count === 1 ||
					covariate.count       === 1 ||
					(colorAnyway.checked && primaryFactor.count === 1) ||
					showCaption.checked
					) {
						675
					} else {
						550
					}
			}
			IntegerField{ name: "heightPlot"; label: qsTr("Height"); defaultValue: (showCaption.checked) ? 550 : 450 }
		}
	}  // End section Axes, Caption, and Plot Size



	Section
	{
		title:   qsTr("Advanced")
		columns: 3

		CheckBox
		{
			name: 				"mean"
			id:   				mean
			label:				qsTr("Mean")
			Layout.columnSpan:  2
			info:				qsTr(
									"Whether to also show a mean for each cloud." +

									"<h4>Position</h4>" +

									"Use a 'Custom' position to have the mean placed inside of the box - even if you do not show the box.<br>" +
									"In fact, the mean nudge and distance fields are synchronized with the box nudge and box fields, respectively.<br>" +
									"Use the 'On axis ticks' position to have all means for each primary factor level on top of each other.<br>" +
									"This can be useful if you want to show the change of secondary factor levels (e.g. groups) across the primary factor levels (e.g. time)." +

									"<h4>Size</h4>" +
									"Try different values to make your plot especially pretty." +

									"<h4>Mean Lines: Width</h4>" +
									"See outline width of box and violin as well as ID lines width."
			)

			RadioButtonGroup
			{
				name:       "meanPosition"
			  	title:       qsTr("Position")
			  	RadioButton
			  	{
					value:   "likeBox"
					label:   qsTr("Custom")
					checked: true

					Group
					{
						columns: 2

						DoubleField
						{
							label: 	        qsTr("Nudge")
							isBound:        false  // Is not passed on to options, does not need a name
							id:		        meanNudge
							defaultValue:   boxNudge.value
							onValueChanged: if (initialized) boxNudge.value = meanNudge.value
							negativeValues:	true
						}

						DoubleField
						{
							label: 	        qsTr("Distance")
							isBound:        false  // Is not passed on to options, does not need a name
							id:		        meanDistance
							defaultValue:   boxWidth.value
							onValueChanged: if (initialized) boxWidth.value = meanDistance.value
						}

					}
				}

				RadioButton { value: "onAxisTicks"; label: qsTr("On axis ticks")               }
			}

			DoubleField{ name: "meanSize"; label: qsTr("Size"); defaultValue: 6 }

			CheckBox
			{
				name: 				"meanLines"
				label:  			qsTr("Connect means with lines")
				enabled: 			mean.checked
				childrenOnSameRow:  true

				PercentField { name: "meanLinesOpacity"; label: qsTr("Opacity"); defaultValue: 50; fieldWidth: 40 }
				DoubleField  { name: "meanLinesWidth";   label: qsTr("Width");   defaultValue: 1 				  }
			}
		}  // End CheckBox Means

		CheckBox
		{
			name: 				"meanInterval"
			id:   				meanInterval
			label:  			qsTr("Interval around mean")
			enabled:			mean.checked && !meanIntervalCustom.checked
			Layout.columnSpan: 	3
			info:				qsTr(
									"Whether to also show an interval around the mean.<br>" +
									"This hides the box, but if you really want, you can go up and check 'Show box' again." +

									"<br><br>" +

									"You can choose between ±1 standard deviation or a confidence interval.<br>" +
									"The confidence interval is computed independently for each group. For this, the corresponding checkbox needs to be checked, to acknowledge that.<br>" +
									"This computation means that any factors are treated as between-factors.<br>" +
									"If your data does not meet this assumption, but you still want to show intervals around the means, you can specify custom intervals (see below).<br>" +
									"One example where this assumption does not hold is with ID input. This is why ID input disables the computation of confidence intervals." +

									"<br>" +

									"<h4>Method</h4>" +
									"How should the confidence interval be computed?<br>" +

									"'Normal model' uses a standard error defined as: " +
									"_SD_<sub>group</sub> / square root of _N_<sub>group</sub><br>" +

									"'T model' yields results identical to a one-sample t-test.<br>" +

									"For 'Bootstrap' you can further specify the number of samples and set a seed for reproducible results."
			)

			RadioButtonGroup
			{
				name:    "meanIntervalOption"
			  	enabled: meanInterval.checked

				RadioButton { label: qsTr("± 1 standard deviation"); value: "sd"; checked: true }

				RadioButton  // Start RadioButtion Confidence interval
				{
					label: qsTr("Confidence interval")
					value: "ci"
					enabled: observationId.count === 0

					CheckBox
					{
						name: "meanCiAssumption"
						label: qsTr("Compute confidence interval independently for each group.")

						Group    // Start group ci settings
						{
							columns: 2

							Label   { text: qsTr("Width") }
							CIField { name: "meanCiWidth" }

							Label{ text: qsTr("Method") }
							DropDown
							{
								name: 	"meanCiMethod"
								id: ciMethod
								values:	[
										{ label: qsTr("Normal model"), value: "normalModel"    },
									   	{ label: qsTr("T model"),      value: "oneSampleTTest" },
									   	{ label: qsTr("Bootstrap"),    value: "bootstrap"      },
									   	]
							}
						}  // End group ci settings

						Group  // Start group bootstrap settings
						{
							columns: 2

							Label { text: qsTr("Bootstrap samples"); enabled: ciMethod.value == "bootstrap" }
							IntegerField
							{
								name: "meanCiBootstrapSamples"
								enabled: ciMethod.value == "bootstrap"
								defaultValue: 1000
								min: 1
								max: 50000
							}

							CheckBox
							{
								name: "setSeed"
								id: setSeed
								enabled: ciMethod.value == "bootstrap"
								label: qsTr("Seed for reproducibility")
								info: qsTr("Hi Joris, I think this works.")
							}
							IntegerField
							{
								name: "seed"
								enabled: setSeed.checked
								defaultValue: 1
								negativeValues: true
							}

						}    // End group bootstrap settings


					}  // End ciAssumption CheckBox
				}  // End RadioButton Confidence interval

			}  // End RadioButtonGroup meanIntervalOption
		}  // End CheckBox meanInterval
		
		DoubleField
		{
			label: 				qsTr("Interval line width")
			isBound:			false  // Is not passed on to options, does not need a name
			id:					intervalOutlineWidth
			defaultValue: 		boxOutlineWidth.value
			onValueChanged: 	boxOutlineWidth.value = intervalOutlineWidth.value
			enabled: 			meanInterval.checked || meanIntervalCustom.checked
			Layout.columnSpan:  2
			infoLabel:  		qsTr("Interval line width")
			info: 				qsTr("Determines the thickness of the interval lines.")
		}
		HelpButton
		{
			toolTip:	qsTr("Width of interval whiskers depends on mean distance.")
		}


		Group
		{
			title:				qsTr("Custom Cloud Orientation and Mean Interval Limits")
			Layout.columnSpan:  2

			IntegerField
			{
				id: numberOfClouds
				name: "numberOfClouds"
				label: qsTr("How many clouds are currently shown?")
				min: 1
				defaultValue: 1
				info:				qsTr(
										"To apply custom cloud orientation or custom mean intervals, you first must specify the number of clouds that are currently shown in the plot.<br>" +
										"If the number you entered does not match the number of clouds in the plot, the plot caption will show a warning and you can correct your entry.<br>" +
										"The number you specify determines the number of rows in the table." +

										"<br><br>" +

										"In the table, you can then specify the custom orientation ('L' vs. 'R') for each cloud as well as custom lower and upper interval limits for each mean.<br>" +
										"Make sure to also check the corresponding checkboxes 'Apply custom orientation' or 'Apply custom mean interval limits'.<br>" +
										"Otherwise, your input into the table will not be applied." +

										"<br><br>" +

										"The order in which the rows of the table are mapped to the clouds in the plot is as follows:<br>" +
										"Suppose, there are three times of measurement (primary factor): pre, post, and follow-up.<br>" +
										"Further, there are two species (secondary factor): alpha and beta.<br>" +
										"This means that there are 6 clouds, in the following order:<br>" +
										"pre-alpha<br>pre-beta<br>" +
										"post-alpha<br>post-beta<br>" +
										"follow-up-alpha<br>follow-up-beta<br><br>" +
										"In this example you could now specify the odd rows as 'L' and the even rows as 'R'.<br>" +
										"That way, at each time point, the left cloud would be alpha and the right cloud would be beta." +

										"<br>" +

										"If you have a 2x2 design (e.g. no follow-up in as primary factor), then a useful custom orientation could be LLRR.<br>" +
										"That way, both the alpha and the beta cloud at time point pre would be left and at time point they would be right.<br>" +
										"Combine this with ID input to connect the individual observations over time.<br>" +
										"They will then run between the two clouds on the left to the two clouds on the right." +

										"<br><br>" +

										"Any custom orientation or mean intervals will be applied to the plot of every dependent variable.<br>" +
										"How can you specify different custom mean intervals for two dependent variables?<br>" +
										"For this, you can duplicate the analysis (see symbol: white plus on green background),<br>" +
										"only select one dependent variable in each version, and specify the custom intervals separately."
				)
			}

			CheckBox{ id: customSides; name: "customSides"; label: qsTr("Apply custom orientation") }

			CheckBox{ id: meanIntervalCustom; name: "meanIntervalCustom"; enabled: mean.checked; label: qsTr("Apply custom mean interval limits") }

			TableView
			{

				id: customizationTable
				modelType			: JASP.Simple

				implicitWidth		: 350 //  form.implicitWidth
				implicitHeight		: 240 * preferencesModel.uiScale // about 6 rows

				initialRowCount		: numberOfClouds.value
				rowCount			: numberOfClouds.value
				initialColumnCount	: 3
				columnCount			: 3

				name				: "customizationTable"
				cornerText			: qsTr("Cloud")
				columnNames			: [qsTr("Orientation"), qsTr("Lower Limit"), qsTr("Upper Limit")]

				// isFirstColEditable	: false

				itemType			: JASP.Double
				itemTypePerColumn	: [JASP.String, JASP.Double, JASP.Double] // first column is string, all others are double

				function getRowHeaderText(headerText, rowIndex)	 { return String.fromCharCode(65 + rowIndex);	}
				// function getDefaultValue(columnIndex, rowIndex)	 { return columnIndex === 0 ? String.fromCharCode(65 + rowIndex) : 2 * columnIndex - 3;	}

				function getDefaultValue(columnIndex, rowIndex)	 { return columnIndex === 0 ? "R" : 0	}
				// function getDefaultValue(columnIndex, rowIndex)	 { return columnIndex === 0 ? String.fromCharCode(65 + rowIndex) : 0	}

				JASPDoubleValidator			{ id: doubleValidator; decimals: 3	}
				RegularExpressionValidator  { id: stringValidator; regularExpression: /^[LR]$/ }

				function getValidator(columnIndex, rowIndex) 	 { return columnIndex === 0 ? stringValidator : doubleValidator	}
			}

		}  // End group custom orientation and table

		HelpButton
		{
			toolTip:	qsTr(
							"For customization, the cloud order is as follows:\n" +
							"For each level of the Primary Factor,\n" +
							"go through all levels of the Secondary Factor.\n" +
							"For example, with a 2 (Primary: X, Y) x 2 (Secondary: i, ii) design,\n" +
							"there are four clouds, A to D, with:\n" +
							"A = Xi, B = Xii, C = Yi, D = Yii.\n\n" +
							
							"Custom orientation:\n" +
							"Per default, all violins are right of the boxes.\n" +
							"To customize this, specify 'L' or 'R' for each cloud.\n" +
							"Applying the custom orientation sets Point Nudge to 0."
						)
		}

	}  // End section Advanced

	infoBottom: qsTr(

		"The output is a raincloud plot for each dependent variable. Optionally, there is a table with statistics for each plot.<br><br><br>" +

		"<h1>How can I re-use my elaborate, customized plot settings for a different dataset?</h1><br>" +

		"Suppose that you have two datasets A and B (in separate JASP files) for which you want to create the same raincloud plot.<br>" +
		"Further, suppose that you already have a raincloud plot for dataset A and now you want the same one for dataset B.<br>" +
		"There are two ways in which you can achieve this without having to specify the custom plot settings all over again.<br><br>" +

		"1) If dataset B only has a different dependent variable but is otherwise identical,<br>" +
		"then you can copy and paste this variable from the JASP file for dataset B into the JASP file for dataset A (see Edit Data header menu in JASP).<br>" +
		"Next, select the newly created dependent variable in dataset A as an additional dependent variable for the raincloud plot." +

		"<br><br>" +

		"2) If dataset B is rather different from dataset A<br>(e.g. primary factor has different name and additional levels or there are fewer observations with different IDs),<br>" +
		"then you have to take a different approach.<br>" +
		"First, make a copy of the JASP file for dataset A (called target file in the following).<br>" +
		"Second, open this target file. It should show the raincloud plot as specified for dataset A.<br>" +
		"Third, go to the Edit Data header menu and _delete the entire dataset A_.<br>" +
		"Fourth, go to the JASP file for dataset B and copy-paste its data to the target file.<br>" +
		"Fifth, choose the dependent variables, primary and secondary factor, covariate, and ID for the raincloud plot.<br>" +
		"Importantly, though, all the other specifications like point spread or custom interval limits should still be present from the raincloud plot for dataset A." +

		"<br><br><br>" +

		"<h1>References</h1><br>" +

		"Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., van Langen, J., & Kievit, R. A. (2021). " +
		"Raincloud plots: a multi-platform tool for robust data visualization. [version 2; peer review: 2 approved]. " +
		"Wellcome Open Res 2021, 4:63. https://doi.org/10.12688/wellcomeopenres.15191.2." +

		"<br><br>" +

		"Aphalo, P. (2024). _ggpp: Grammar Extensions to 'ggplot2'_. R package version 0.5.6, <https://CRAN.R-project.org/package=ggpp>." +

		"<br><br>" +

		 "JASP Team (2024). For this module especially: Ott, V. L., van den Bergh, D., Boutin, B., Goosen, J., Judd, N., Bartoš, F., & Wagenmakers, E. J." +

		"<br><br>" +

		"Judd, N., van Langen, J., Allen, M., & Kievit, R. A. (2024). _ggrain: A Rainclouds Geom for 'ggplot2'_. R package version 0.0.4, <https://CRAN.R-project.org/package=ggrain>." +

		"<br><br>" +

		"Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York." +

		"<br><br>" +

		"Wickham, H., François, R., Henry, L., Müller, K., & Vaughan, D. (2023). _dplyr: A Grammar of Data Manipulation_. R package version 1.1.4, <https://CRAN.R-project.org/package=dplyr>." +

		"<br><br>" +

		"Wilke C. & Wiernik, B. (2022). _ggtext: Improved Text Rendering Support for 'ggplot2'_. R package version 0.1.2, <https://wilkelab.org/ggtext/>."
	)

}  // End Form