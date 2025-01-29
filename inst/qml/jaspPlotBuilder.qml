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

    infoBottom:
        "## " + qsTr("References") + "\n" +
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

    /// Here begins the main plot control
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    TabView {


        name: "scatterPlots"
        // name: "PlotBuilderTab"

        rowComponent: Group {

            childControlsArea.anchors.leftMargin: jaspTheme.contentMargin

            Group{
                columns:2
                TextField {
                    name: "plotId"
                    label: qsTr("Plot ID")
                    fieldWidth: 200
                    placeholderText: qsTr("Enter Plot ID")
                }

                RadioButtonGroup {
                    id:						isRM
                    Layout.columnSpan:		1
                    name:					"isRM"
                    title:					qsTr("Repeated measurements?")
                    radioButtonsOnSameRow:	true
                    columns:				2
                    info: qsTr("Choose whether you want to make plots of repeated measurements from repeated measurements.")

                    RadioButton {
                        label:		qsTr("No")
                        value:		"noRM"
                        id:			noRM
                        checked:	true
                    }

                    RadioButton {
                        label:		qsTr("Yes")
                        value:		"RM"
                        id:          yesRM
                    }
                }
            }

            Group {
                VariablesForm {

                    removeInvisibles:	true
                    preferredWidth: jaspForm.width - 2 * jaspTheme.contentMargin
                    preferredHeight: 500

                    infoLabel: qsTr("Input")
                    AvailableVariablesList { name: "allVariablesList"}


                    AssignedVariablesList {
                        name: "variableRepeatedMeasures"
                        title: "Repeated measures variables"
                        visible: yesRM.checked
                        property bool active: yesRM.checked
                        onActiveChanged: if (!active && count > 0) itemDoubleClicked(0)
                    }

                    TextField {
                        name: "rmFactorText"
                        id: rmFactorText
                        placeholderText: "Repeated-measures factor (e.g., Time, Dose, Concentration)"
                        visible: yesRM.checked
                        property bool active: yesRM.checked
                        onActiveChanged: if (!active && count > 0) itemDoubleClicked(0)
                        info: qsTr("Repeated-measures factor (e.g., Time, Dose, Concentration)")
                        fieldWidth: 239
                    }

                    TextField {
                        name: "dimensionText"
                        id: dimensionText
                        placeholderText: qsTr("Measurement dimension (e.g., cm, mg, minutes)")
                        visible: yesRM.checked
                        property bool active: yesRM.checked
                        onActiveChanged: if (!active && count > 0) itemDoubleClicked(0)
                        info: qsTr("Measurement dimension (e.g., cm, mg, minutes)")
                        fieldWidth: 239
                    }

                    CheckBox {
                    name:"useRMFactorAsFill"
                    label: "Use RM factor as a group/color variable"
                    visible: yesRM.checked
                    }

                    AssignedVariablesList {
                        name: "variableYPlotBuilder"
                        title: "Y axis"
                        allowedColumns: ["scale", "ordinal", "nominal"]
                        id: variableYPlotBuilder
                        singleVariable: true
                        visible: noRM.checked
                        property bool active: noRM.checked
                        onActiveChanged: if (!active && count > 0) itemDoubleClicked(0)
                        info: qsTr("Select the variable for the Y-axis.")
                        onCountChanged: {
                            if (count === 0 && addBoxplot.checked) {
                                addBoxplot.checked = false;
                            }
                        }
                    }

                    AssignedVariablesList {
                        name: "variableXPlotBuilder"
                        title: "X axis"
                        id: variableXPlotBuilder
                        allowedColumns: ["scale", "ordinal", "nominal"]
                        minLevels: 2
                        singleVariable: true
                        info: qsTr("Select the variable for the X-axis.")
                        // visible: noRM.checked
                        property bool active: noRM.checked
                        onActiveChanged: if (!active && count > 0) itemDoubleClicked(0)
                        onCountChanged: {
                            if (count === 0 && addBoxplot.checked) {
                                addBoxplot.checked = false;
                            }
                            // if (count === 0 && addHistogram.checked) {
                            //     addHistogram.checked = false;
                            // }
                        }
                    }

                    AssignedVariablesList {
                        name: "variableColorPlotBuilder"
                        title: qsTr("Group/Color by")
                        id: variableColorPlotBuilder
                        allowedColumns: ["scale", "ordinal", "nominal"]
                        minLevels: 2
                        singleVariable: true
                        info: qsTr("Select the variable for data grouping, which will also determine the coloring..")
                        onCountChanged: {
                            if (count > 0) {
                                colorByVariableX.checked = false;
                                colorByVariableY.checked = false;
                            }
                        }
                    }
                    Group {
                        title: qsTr("Or color by")
                        columns: 2
                        CheckBox {
                            name: "colorByVariableX"
                            label: qsTr("X variable")
                            id: colorByVariableX
                            enabled: variableXPlotBuilder.count > 0 && variableColorPlotBuilder.count === 0
                            onCheckedChanged: {
                                if (checked && variableColorPlotBuilder.count > 0) {
                                    checked = false;
                                }
                                if (checked) {
                                    colorByVariableY.checked = false;
                                }
                            }
                        }
                        CheckBox {
                            name: "colorByVariableY"
                            label: qsTr("Y variable")
                            id: colorByVariableY
                            enabled: variableYPlotBuilder.count > 0 && variableColorPlotBuilder.count === 0
                            onCheckedChanged: {
                                if (checked && variableColorPlotBuilder.count > 0) {
                                    checked = false;
                                }
                                if (checked) {
                                    colorByVariableX.checked = false;
                                }
                            }
                        }
                    }
                    AssignedVariablesList {
                        name: "columnsvariableSplitPlotBuilder"
                        title: qsTr("Split by (columns)")
                        id: columnsvariableSplitPlotBuilder
                        allowedColumns: ["ordinal", "nominal"]
                        singleVariable: true
                        info: qsTr("You can choose a variable to split the plots into columns.")
                    }

                    AssignedVariablesList {
                        name: "rowsvariableSplitPlotBuilder"
                        title: qsTr("Split by (rows)")
                        id: rowsvariableSplitPlotBuilder
                        allowedColumns: ["ordinal", "nominal"]
                        singleVariable: true
                        info: qsTr("You can choose a variable to split the plots into rows.")
                    }

                    Group {
                        title: qsTr("Plot size")
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

                }
            } // End variables form

            Label {
                text: qsTr("Data and geometries")
                wrapMode: Text.Wrap
                color: "black"
            }

            // -------------------------------------------------------------------
            // Data points
            // -------------------------------------------------------------------


            Section {
                title: qsTr("Individual data points")
                CheckBox {
                    name: "addDataPoint"
                    label: qsTr("Add individual data points")
                    id: addDataPoint
                    info: qsTr("Check this option to show individual data points on the plot.")
                    columns: 4

                    Group {
                        DoubleField {
                            name: "pointsizePlotBuilder"
                            id: pointsizePlotBuilder
                            label: "Point size"
                            value: 5
                            min: 0
                            max: 10
                        }

                    }

                    Group {
                        DoubleField {
                            name: "jitterhPlotBuilder"
                            id: jitterhPlotBuilder
                            label: "Jitter height"
                            value: 0.3
                            min: 0
                            max: 10
                        }
                        DoubleField {
                            name: "jitterwPlotBuilder"
                            id: jitterwPlotBuilder
                            label: "Jitter width"
                            value: 0.3
                            min: 0
                            max: 10
                        }
                    }

                    Group {
                        DoubleField {
                            name: "alphaPlotBuilder"
                            id: alphaPlotBuilder
                            label: "Transparency"
                            value: 1
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "pointDodgePlotBuilder"
                            label: qsTr("Dodge")
                            id: pointDodgePlotBuilder
                            defaultValue: 0.8
                        }
                    }

                    CheckBox {
                        name: "blackOutlineDataPoint"
                        label: qsTr("Black outline")
                        checked: true
                    }

                    CheckBox {
                        name: "connectRMPlotBuilder"
                        label: qsTr("Connect data points")
                        id: connectRMPlotBuilder
                        checked: false
                        enabled: isRM.value === "RM"

                        DoubleField {
                            name: "lineRMtransparency"
                            label: "Transparency"
                            value: 0.5
                        }

                        DoubleField {
                            name: "lineRMsize"
                            label: "Line width"
                            value: 0.8
                        }


                    }
                }
            }

            // -------------------------------------------------------------------
            // Distributions (histogram, boxplot, violin)
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Distributions (histogram, boxplot, violin)")

                Label {
                    text: qsTr("To create a histogram, please select only one variable (X or Y)")
                    wrapMode: Text.Wrap
                    color: "black"
                }

                GridLayout {
                    columns: 3
                    rows: 1
                    rowSpacing: 40
                    columnSpacing: 40

                    // Histogram
                    CheckBox {
                        name: "addHistogram"
                        id: addHistogram
                        label: qsTr("Add histogram")
                        info: qsTr("Check this option to create histogram for x or y variables.")
                        enabled: (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)
                                 && !(variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                        DoubleField {
                            name: "binsPlotBuilder"
                            label: qsTr("Number of bins")
                            id: binsPlotBuilder
                            defaultValue: 30
                        }

                        DoubleField {
                            name: "alphaHistogramPlotBuilder"
                            label: qsTr("Transparency")
                            id: alphaHistogramPlotBuilder
                            defaultValue: 1
                            min: 0
                            max: 1
                        }
                    }

                    // Boxplot
                    CheckBox {
                        name: "addBoxplot"
                        id: addBoxplot
                        label: qsTr("Add boxplot")
                        info: qsTr("Check this option to create boxplot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeBoxplotPlotBuilder"
                            label: qsTr("Dodge")
                            id: dodgeBoxplotPlotBuilder
                            defaultValue: 0.8}


                        DoubleField {
                            name: "alphaBoxplotPlotBuilder"
                            label: qsTr("Transparency")
                            id: alphaBoxplotPlotBuilder
                            defaultValue: 0.3
                            min: 0
                            max: 1
                        }

                        DoubleField {
                            name: "widthLineBoxplotPlotBuilder"
                            label: qsTr("Line width")
                            id: widthLineBoxplotPlotBuilder
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "widthBoxplotPlotBuilder"
                            label: qsTr("Boxplot width")
                            id: widthBoxplotPlotBuilder
                            defaultValue: 0.6
                        }
                        DoubleField {
                            name: "widthWhiskersPlotBuilder"
                            label: qsTr("Whiskers width")
                            id: widthWhiskersPlotBuilder
                            defaultValue: 0.3
                        }


                        CheckBox {
                            name: "outlierBoxplotPlotBuilder"
                            label: qsTr("Show outliers")
                            id: outlierBoxplotPlotBuilder
                            checked: true
                        }

                        DoubleField {
                            name: "outlierCoefBoxplotPlotBuilder"
                            label: qsTr("Outlier coef")
                            id: outlierCoefoxplotPlotBuilder
                            defaultValue: 1.5
                        }

                        DoubleField {
                            name: "outlierSizeBoxplotPlotBuilder"
                            label: qsTr("Outlier size")
                            id: outlierSizeBoxplotPlotBuilder
                            defaultValue: 1
                        }

                        CheckBox {
                            name: "blackOutlineBoxplot"
                            label: qsTr("Black outline")
                            checked: true
                            info: qsTr("Enable black outline/fill for the boxplot geom.")
                        }


                    }

                    // Violin
                    CheckBox {
                        name: "addViolin"
                        id: addViolin
                        label: qsTr("Add violin plot")
                        info: qsTr("Check this option to add a violin plot to your visualization.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))


                        DoubleField {
                            name: "dodgeViolinPlotBuilder"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "alphaViolinPlotBuilder"
                            label: qsTr("Transparency")
                            id: alphaViolinPlotBuilder
                            defaultValue: 0.3
                            min: 0
                            max: 1
                        }



                        DoubleField {
                            name: "linewidthViolinPlotBuilder"
                            label: qsTr("Line width")
                            id: linewidthViolinPlotBuilder
                            defaultValue: 0.5
                        }
                        DropDown {
                            name: "scaleViolinPlotBuilder"
                            label: qsTr("Scale method")
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
                            defaultValue: "c(0.25, 0.5, 0.75)"
                            multiple: true
                        }

                        CheckBox {
                            name: "trimViolinPlotBuilder"
                            label: qsTr("Trim violins")
                            id: trimViolinPlotBuilder
                            checked: false
                        }



                        CheckBox {
                            name: "blackOutlineViolin"
                            label: qsTr("Black outline")
                        }


                    }
                }

            }

            // -------------------------------------------------------------------
            // Amounts (count and sum)
            // -------------------------------------------------------------------



            Section {
                title: qsTr("Amounts (count and sum)")

                Label {
                    text: qsTr("Represent counts (requires only x variables)")
                    wrapMode: Text.Wrap
                    color: "black"
                }


                GridLayout {
                    columns: 3
                    rows: 2
                    rowSpacing: 40     // Sorok közötti távolság növelése
                    columnSpacing: 40    // Oszlopok közötti távolság növelése

                    // Count Bar

                    CheckBox {
                        name: "addCountBar"
                        label: qsTr("Count bar")
                        info: qsTr("Enable to add a count bar to the plot.")
                        enabled: variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0


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
                        info: qsTr("Enable to add dashed lines to the plot.")
                        enabled: variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0

                        DoubleField {
                            name: "dodgeCountDash"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "dashwidthCountDash"
                            label: qsTr("Dash width")
                            defaultValue: 0.6
                        }

                        DoubleField {
                            name: "linewidthCountDash"
                            label: qsTr("Line width")
                            defaultValue: 0.5
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
                        info: qsTr("Enable to add count dots to the plot.")
                        enabled: variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0

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
                        }
                    }

                    // Count Line
                    CheckBox {
                        name: "addCountLine"
                        label: qsTr("Count line")
                        info: qsTr("Enable to add count lines to the plot.")
                        enabled: variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0


                        DoubleField {
                            name: "dodgeCountLine"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthCountLine"
                            label: qsTr("Line width")
                            defaultValue: 0.5
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
                        info: qsTr("Enable to add a count area to the plot.")
                        enabled: variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0

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
                        info: qsTr("Enable to add count values to the plot.")
                        enabled: variableXPlotBuilder.count > 0 && variableYPlotBuilder.count === 0

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
                            defaultValue: 0
                            min: -Infinity
                        }
                        DoubleField {
                            name: "vjustCountValue"
                            label: qsTr("Vertical justification")
                            defaultValue: 0
                            min: -Infinity
                        }

                        CheckBox {
                            name: "blackOutlineCountValue"
                            label: qsTr("Black text")
                        }
                    }
                }


                Label {
                    text: qsTr("Represent sums (requires X and Y variables)")
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
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

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
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeSumDash"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "widthSumDash"
                            label: qsTr("Dash width")
                            defaultValue: 0.6
                        }
                        DoubleField {
                            name: "linewidthSumDash"
                            label: qsTr("Line width")
                            defaultValue: 0.5
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
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

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
                    }

                    // Sum Line
                    CheckBox {
                        name: "addSumLine"
                        label: qsTr("Sum line")
                        info: qsTr("Enable to add a sum line to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

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
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

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
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

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
                            defaultValue: 0
                            min: -Infinity
                        }
                        DoubleField {
                            name: "vjustSumValue"
                            label: qsTr("Vertical justification")
                            defaultValue: 0
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

                Label {
                    text: qsTr("Represent absolute or relative proportions (requires X or Y and Group/Color variables)")
                    wrapMode: Text.Wrap
                    color: "black"
                }

                GridLayout {
                    columns: 2
                    rowSpacing: 40
                    columnSpacing: 70

                    // Bar Stack Absolute
                    CheckBox {
                        name: "addBarStackAbsolute"
                        label: qsTr("Bar stack (absolute)")
                        info: qsTr("Add an absolute bar stack to the plot.")
                        enabled: (isRM.value === "RM") || (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)
                                 && variableColorPlotBuilder.count > 0

                        DoubleField {
                            name: "alphaBarStackAbsolute"
                            label: qsTr("Transparency")
                            defaultValue: 0.8
                            min: 0
                            max: 1
                        }
                        CheckBox {
                            name: "reverseBarStackAbsolute"
                            label: qsTr("Reverse order")
                            checked: false
                        }

                    }

                    // Bar Stack Relative
                    CheckBox {
                        name: "addBarStackRelative"
                        label: qsTr("Bar stack (relative)")
                        info: qsTr("Add a relative bar stack to the plot.")
                        enabled: (isRM.value === "RM") ||  (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)
                                 && variableColorPlotBuilder.count > 0

                        DoubleField {
                            name: "alphaBarStackRelative"
                            label: qsTr("Transparency")
                            defaultValue: 0.8
                            min: 0
                            max: 1
                        }
                        CheckBox {
                            name: "reverseBarStackRelative"
                            label: qsTr("Reverse order")
                            checked: false
                        }

                    }

                    // Area Stack Absolute
                    CheckBox {
                        name: "addAreaStackAbsolute"
                        label: qsTr("Area stack (absolute)")
                        info: qsTr("Add an absolute area stack to the plot.")
                        enabled: (isRM.value === "RM") || (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)
                                 && variableColorPlotBuilder.count > 0

                        DoubleField {
                            name: "alphaAreaStackAbsolute"
                            label: qsTr("Transparency")
                            defaultValue: 0.4
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "linewidthAreaStackAbsolute"
                            label: qsTr("Line width")
                            defaultValue: 0.25
                            min: 0
                        }
                        CheckBox {
                            name: "reverseAreaStackAbsolute"
                            label: qsTr("Reverse order")
                            checked: false
                        }
                        CheckBox {
                            name: "replaceNaAreaStackAbsolute"
                            label: qsTr("Replace N/A")
                            checked: false
                        }


                    }

                    // Area Stack Relative
                    CheckBox {
                        name: "addAreaStackRelative"
                        label: qsTr("Area Stack (Relative)")
                        info: qsTr("Add a relative area stack to the plot.")
                        enabled: (isRM.value === "RM") ||  (variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)
                                 && variableColorPlotBuilder.count > 0

                        DoubleField {
                            name: "alphaAreaStackRelative"
                            label: qsTr("Transparency")
                            defaultValue: 0.4
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "linewidthAreaStackRelative"
                            label: qsTr("Line Width")
                            defaultValue: 0.25
                            min: 0
                        }
                        CheckBox {
                            name: "reverseAreaStackRelative"
                            label: qsTr("Reverse Order")
                            checked: false
                        }
                        CheckBox {
                            name: "replaceNaAreaStackRelative"
                            label: qsTr("Replace N/A")
                            checked: false
                        }

                    }
                }
            }

            // -------------------------------------------------------------------
            // Mean
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Mean")

                Label {
                    text: qsTr("Represent mean (requires X and Y variables)")
                    wrapMode: Text.Wrap
                    color: "black"
                }

                GridLayout {
                    columns: 3
                    rowSpacing: 40     // Sorok közötti távolság növelése
                    columnSpacing: 40    // Oszlopok közötti távolság növelése

                    // Mean bar
                    CheckBox {
                        name: "addMeanBar"
                        label: qsTr("Mean bar")
                        info: qsTr("Enable to add a mean bar to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMeanBar"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "alphaMeanBar"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "saturationMeanBar"
                            label: qsTr("Saturation")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }
                    }

                    // Mean dash
                    CheckBox {
                        name: "addMeanDash"
                        label: qsTr("Mean dash")
                        info: qsTr("Enable to add dashed mean lines to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMeanDash"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "dashwidthMeanDash"
                            label: qsTr("Dash width")
                            defaultValue: 0.6
                        }
                        DoubleField {
                            name: "linewidthMeanDash"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "alphaMeanDash"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineMeanDash"
                            label: qsTr("Black dash")
                        }
                    }

                    // Mean dot
                    CheckBox {
                        name: "addMeanDot"
                        label: qsTr("Mean dot")
                        info: qsTr("Enable to add mean dots to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMeanDot"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "sizeMeanDot"
                            label: qsTr("Dot size")
                            defaultValue: 5
                        }
                        DoubleField {
                            name: "alphaMeanDot"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineMeanDot"
                            label: qsTr("Black outline")
                        }
                    }

                    // Mean line
                    CheckBox {
                        name: "addMeanLine"
                        label: qsTr("Mean line")
                        info: qsTr("Enable to add mean lines to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMeanLine"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMeanLine"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "alphaMeanLine"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineMeanLine"
                            label: qsTr("Black line")
                        }
                    }

                    // Mean area
                    CheckBox {
                        name: "addMeanArea"
                        label: qsTr("Mean area")
                        info: qsTr("Enable to add mean areas to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMeanArea"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMeanArea"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "alphaMeanArea"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                    }

                    // Mean value
                    CheckBox {
                        name: "addMeanValue"
                        label: qsTr("Mean value")
                        info: qsTr("Enable to add mean values to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "fontsizeMeanValue"
                            label: qsTr("Font size")
                            defaultValue: 14
                        }
                        FormulaField {
                            name: "accuracyMeanValue"
                            label: qsTr("Accuracy")
                            defaultValue: "0.1"
                        }
                        DoubleField {
                            name: "alphaMeanValue"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "hjustMeanValue"
                            label: qsTr("Horizontal justification")
                            defaultValue: 0
                            min: -Infinity

                        }
                        DoubleField {
                            name: "vjustMeanValue"
                            label: qsTr("Vertical justification")
                            defaultValue: 0
                            min: -Infinity
                        }

                        CheckBox {
                            name: "blackOutlineMeanValue"
                            label: qsTr("Black text")
                        }
                    }
                }
            }

            // -------------------------------------------------------------------
            // Median
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Median")

                Label {
                    text: qsTr("Represent median (requires X and Y variables)")
                    wrapMode: Text.Wrap
                    color: "black"
                }

                GridLayout {
                    columns: 3
                    rowSpacing: 40     // Sorok közötti távolság növelése
                    columnSpacing: 40    // Oszlopok közötti távolság növelése

                    // Median bar
                    CheckBox {
                        name: "addMedianBar"
                        label: qsTr("Median bar")
                        info: qsTr("Enable to add a median bar to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMedianBar"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "alphaMedianBar"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "saturationMedianBar"
                            label: qsTr("Saturation")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }
                    }

                    // Median dash
                    CheckBox {
                        name: "addMedianDash"
                        label: qsTr("Median dash")
                        info: qsTr("Enable to add dashed median lines to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMedianDash"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "dashwidthMedianDash"
                            label: qsTr("Dash width")
                            defaultValue: 0.6
                        }
                        DoubleField {
                            name: "linewidthMedianDash"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "alphaMedianDash"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineMedianDash"
                            label: qsTr("Black dash")
                            enabled: addMedianDash.checked
                        }
                    }

                    // Median dot
                    CheckBox {
                        name: "addMedianDot"
                        label: qsTr("Median dot")
                        info: qsTr("Enable to add median dots to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMedianDot"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "sizeMedianDot"
                            label: qsTr("Dot size")
                            defaultValue: 5
                        }
                        DoubleField {
                            name: "alphaMedianDot"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineMedianDot"
                            label: qsTr("Black outline")
                            enabled: addMedianDot.checked
                        }
                    }

                    // Median line
                    CheckBox {
                        name: "addMedianLine"
                        label: qsTr("Median line")
                        info: qsTr("Enable to add median lines to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMedianLine"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMedianLine"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "alphaMedianLine"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineMedianLine"
                            label: qsTr("Black line")
                            enabled: addMedianLine.checked
                        }
                    }

                    // Median area
                    CheckBox {
                        name: "addMedianArea"
                        label: qsTr("Median area")
                        info: qsTr("Enable to add median areas to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeMedianArea"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMedianArea"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "alphaMedianArea"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }

                    }

                    // Median value
                    CheckBox {
                        name: "addMedianValue"
                        label: qsTr("Median value")
                        info: qsTr("Enable to add median values to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "fontsizeMedianValue"
                            label: qsTr("Font size")
                            defaultValue: 14
                        }
                        FormulaField {
                            name: "accuracyMedianValue"
                            label: qsTr("Accuracy")
                            defaultValue: "0.1"
                        }
                        DoubleField {
                            name: "alphaMedianValue"
                            label: qsTr("Transparency")
                            defaultValue: 1
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "hjustMedianValue"
                            label: qsTr("Horizontal justification")
                            defaultValue: 0
                            min: -Infinity
                        }
                        DoubleField {
                            name: "vjustMedianValue"
                            label: qsTr("Vertical justification")
                            defaultValue: 0
                            min: -Infinity
                        }

                        CheckBox {
                            name: "blackOutlineMedianValue"
                            label: qsTr("Black text")
                        }
                    }
                }
            }

            // -------------------------------------------------------------------
            // Error bars and ribbons (range, sd, sem, 95% CI)
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Error bars and ribbons (Range, SD, SEM, 95%CI)")

                Label {
                    text: qsTr("Error bars (requires X and Y variables.")
                    wrapMode: Text.Wrap
                    color: "black"
                }

                GridLayout {
                    columns: 2
                    rows: 2
                    rowSpacing: 40     // Sorok közötti távolság növelése
                    columnSpacing: 70    // Oszlopok közötti távolság növelése

                    // Range Error Bar
                    CheckBox {
                        name: "addRangeErrorBar"
                        label: qsTr("Range error bar")
                        info: qsTr("Enable to add range error bars to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeRangeErrorBar"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "widthRangeErrorBar"
                            label: qsTr("Width")
                            defaultValue: 0.3
                        }
                        DoubleField {
                            name: "linewidthRangeErrorBar"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "transparencyRangeErrorBar"
                            label: qsTr("Transparency")
                            defaultValue: 1
                        }

                        CheckBox {
                            name: "blackOutlineRangeErrorBar"
                            label: qsTr("Black lines")
                        }
                    }

                    // SD Error Bar
                    CheckBox {
                        name: "addSDErrorBar"
                        label: qsTr("SD Error Bar")
                        info: qsTr("Enable to add standard deviation error bars to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeSDErrorBar"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "widthSDErrorBar"
                            label: qsTr("Width")
                            defaultValue: 0.3
                        }
                        DoubleField {
                            name: "linewidthSDErrorBar"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "transparencySDErrorBar"
                            label: qsTr("Transparency")
                            defaultValue: 1
                        }

                        CheckBox {
                            name: "blackOutlineSDErrorBar"
                            label: qsTr("Black lines")
                            enabled: addSDErrorBar.checked
                        }
                    }

                    // SEM Error Bar
                    CheckBox {
                        name: "addSEMErrorBar"
                        label: qsTr("SEM error bar")
                        info: qsTr("Enable to add SEM error bars to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeSEMErrorBar"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "widthSEMErrorBar"
                            label: qsTr("Width")
                            defaultValue: 0.3
                        }
                        DoubleField {
                            name: "linewidthSEMErrorBar"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "transparencySEMErrorBar"
                            label: qsTr("Transparency")
                            defaultValue: 1
                        }

                        CheckBox {
                            name: "blackOutlineSEMErrorBar"
                            label: qsTr("Black lines")
                            enabled: addSEMErrorBar.checked
                        }
                    }

                    // 95% CI Error Bar
                    CheckBox {
                        name: "addCI95ErrorBar"
                        label: qsTr("95% CI Error Bar")
                        info: qsTr("Enable to add 95% confidence interval error bars to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "dodgeCI95ErrorBar"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "widthCI95ErrorBar"
                            label: qsTr("Width")
                            defaultValue: 0.3
                        }
                        DoubleField {
                            name: "linewidthCI95ErrorBar"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                        DoubleField {
                            name: "transparencyCI95ErrorBar"
                            label: qsTr("Transparency")
                            defaultValue: 1
                        }

                        CheckBox {
                            name: "blackOutlineCI95ErrorBar"
                            label: qsTr("Black lines")
                        }
                    }
                }

                Label {
                    text: qsTr("Error ribbons (requires X and Y variables.\nThe optimal dodge value is 0 if both X and Y are continuous.")
                    wrapMode: Text.Wrap
                    color: "black"
                }

                GridLayout {
                    columns: 2
                    rowSpacing: 40     // Sorok közötti távolság növelése
                    columnSpacing: 70    // Oszlopok közötti távolság növelése

                    // Range Ribbon
                    CheckBox {
                        name: "addRangeRibbon"
                        label: qsTr("Range ribbon")
                        info: qsTr("Enable to add a range ribbon to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "alphaRangeRibbon"
                            label: qsTr("Transparency")
                            defaultValue: 0.4
                            min: 0
                            max: 1
                        }

                        // Black outline/fill
                        CheckBox {
                            name: "blackOutlineRangeRibbon"
                            label: qsTr("Black lines")
                        }
                    }

                    // SD Ribbon
                    CheckBox {
                        name: "addSdRibbon"
                        label: qsTr("SD ribbon")
                        info: qsTr("Enable to add an SD ribbon to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "alphaSdRibbon"
                            label: qsTr("Transparency")
                            defaultValue: 0.4
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineSdRibbon"
                            label: qsTr("Black lines")
                            enabled: addSdRibbon.checked
                        }
                    }

                    // SEM Ribbon
                    CheckBox {
                        name: "addSemRibbon"
                        label: qsTr("SEM ribbon")
                        info: qsTr("Enable to add a SEM ribbon to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "alphaSemRibbon"
                            label: qsTr("Transparency")
                            defaultValue: 0.4
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineSemRibbon"
                            label: qsTr("Black lines")
                        }
                    }

                    // 95% CI Ribbon
                    CheckBox {
                        name: "addCi95Ribbon"
                        label: qsTr("95% CI ribbon")
                        info: qsTr("Enable to add a 95% confidence interval ribbon to the plot.")
                        enabled: (isRM.value === "RM")
                                 || ((variableXPlotBuilder.count > 0)
                                     && (variableYPlotBuilder.count > 0))

                        DoubleField {
                            name: "alphaCi95Ribbon"
                            label: qsTr("Transparency")
                            defaultValue: 0.4
                            min: 0
                            max: 1
                        }

                        CheckBox {
                            name: "blackOutlineCi95Ribbon"
                            label: qsTr("Black lines")
                        }
                    }
                }
            }

            // -------------------------------------------------------------------
            // Curve fit and reference lines
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Curve fit and reference lines")
                CheckBox {
                    name: "addCurveFitPlotBuilder"
                    label: qsTr("Add curve fit")
                    info: "With this option you can fit a model on the data points."
                    enabled: ((variableXPlotBuilder.count > 0)
                              && (variableYPlotBuilder.count > 0))
                    columns: 3

                    Group {
                        DropDown {
                            name: "curvaFitMethod"
                            label: qsTr("Method")
                            id: curvaFitMethod
                            indexDefaultValue: 0
                            values: [
                                {label: qsTr("Linear"), value: "lm"},
                                {label: qsTr("LOESS"), value: "loess"}
                            ]
                        }

                        DoubleField {
                            name: "linewidthCurveFit"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }
                    }

                    Group {
                        DoubleField {
                            name: "dodgeCurveFit"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "transparencyCurveFit"
                            label: qsTr("Transparency")
                            defaultValue: 0.2
                            min: 0
                            max: 1
                        }
                    }

                    Group {
                        CheckBox {
                            name: "seCurveFit"
                            label: qsTr("SE")
                            checked: true
                        }
                        CheckBox {
                            name: "blackOutlineCurveFit"
                            label: qsTr("Black lines")
                        }
                    }


                }

                CheckBox {
                    name: "addReferenceLinePlotBuilder"
                    label: qsTr("Add reference line")
                    info: "Add reference lines."
                    columns: 2

                    Group {
                        TextField {
                            name: "xReferenceLine"
                            label: qsTr("X Intersections")
                            placeholderText: qsTr("e.g. 0.5, 1, 3")
                        }

                        TextField {
                            name: "yReferenceLine"
                            label: qsTr("Y Intersections")
                            placeholderText: qsTr("e.g. 0.5, 1, 3")
                        }
                    }

                    Group {
                        DoubleField {
                            name: "linewidhtReferenceLines"
                            label: qsTr("Line width")
                            defaultValue: 0.5
                        }

                        TextField {
                            name: "colorReferenceLine"
                            label: qsTr("Line color")
                            placeholderText: qsTr("e.g. black, #ff5733")
                            defaultValue: "lightgray"
                        }

                        Label {
                            text: qsTr("Note: For available colors, see <a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>this page</a>.")
                            wrapMode: TextBrowser.Wrap
                            textFormat: TextBrowser.RichText

                            MouseArea {
                                anchors.fill: parent
                                onClicked: {
                                    Qt.openUrlExternally("https://r-charts.com/colors/")
                                }
                            }
                        }
                    }
                }
            }

            Label {
                text: qsTr("Axis settings and annotations")
                wrapMode: Text.Wrap
                color: "black"
            }

            // -------------------------------------------------------------------
            // Adjust axis
            // -------------------------------------------------------------------
            Section {
                columns: 3

                title: qsTr("Adjust X axis")


                TextField {
                    label: "Title"
                    name: "titleXPlotBuilder"
                    placeholderText: qsTr("Enter X axis title")
                    fieldWidth: 300
                    info: qsTr("Specify the title for the X-axis.")
                    Layout.columnSpan: 3
                }

                Group {
                    title:"Limits"
                    columns:1

                    TextField {
                        name: "limitFromX"
                        label: "from"
                        fieldWidth: 40
                    }

                    TextField {
                        name: "limitToX"
                        label: "to"
                        fieldWidth: 40
                    }
                }

                Group {
                    title: "Breaks"
                    columns:1

                    TextField {
                        name: "breakFromX"
                        label: "from"
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakToX"
                        label: "to"
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakByX"
                        label: "by"
                        fieldWidth: 40
                    }
                }

                Group {
                    columns: 1
                    title: "Labels"
                    CheckBox {
                        name: "rotateXLabel"
                        label: "Rotate X label"
                    }

                    CheckBox {
                        name: "cutShortScale"
                        label: "Shorten labels"
                        info: qsTr("Whether to shorten axis labels using K for thousand, M for million, and so on.")
                    }


                    CheckBox {
                        name: "enableSort"  // Az azonosító, amelyet az R kódban használsz
                        label: "Enable Sorting"
                        checked: false  // Alapértelmezett állapot


                        DropDown {
                            name: "sortXLabelsOrder"  // Az azonosító, amelyet az R kódban használsz
                            label: "Sort X labels"
                            values: ["Increasing", "Decreasing"]  // A legördülő menü opciói
                            startValue: "Increasing"
                        }


                        DropDown {
                            name: "aggregationFun"  // Az azonosító, amelyet az R kódban használsz
                            label: "By"
                            values: ["mean", "median"]  // A legördülő menü opciói
                            startValue: "Mean"
                        }
                    }
                }

                ComponentsList {
                    name: "xAxisLabelRenamer"
                    title: qsTr("New X-axis labels")
                    addItemManually: true
                    minimumItems: 0
                    rowComponent: Row {
                        TextField {
                            name: "originalXLabel"
                            label: qsTr("Original label")
                            fieldWidth: 100
                        }
                        TextField {
                            name: "newXLabel"
                            label: qsTr("New label")
                            fieldWidth: 150
                        }
                    }
                }
            }


            Section {
                columns: 3

                title: qsTr("Adjust Y axis")

                TextField {
                    label: "Title"
                    name: "titleYPlotBuilder"
                    placeholderText: qsTr("Enter Y axis title")
                    fieldWidth: 300
                    info: qsTr("Specify the title for the Y-axis.")
                    Layout.columnSpan: 3
                }

                Group {
                    title: "Limits"
                    columns: 1

                    TextField {
                        name: "limitFromY"
                        label: "from"
                        fieldWidth: 40
                    }

                    TextField {
                        name: "limitToY"
                        label: "to"
                        fieldWidth: 40
                    }
                }

                Group {
                    title: "Breaks"
                    columns: 1

                    TextField {
                        name: "breakFromY"
                        label: "from"
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakToY"
                        label: "to"
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakByY"
                        label: "by"
                        fieldWidth: 40
                    }
                }

                Group {
                    title: "Labels"
                    columns: 1

                    CheckBox {
                        name: "rotateYLabel"
                        label: "Rotate Y label"
                    }

                    CheckBox {
                        name: "cutShortScaleY"
                        label: "Shorten labels"
                        info: qsTr("Whether to shorten axis labels using K for thousand, M for million, and so on.")
                    }

                    // If you want to enable sorting on the Y axis:
                    CheckBox {
                        name: "enableSortY"
                        label: "Enable Sorting"
                        checked: false

                        DropDown {
                            name: "sortYLabelsOrder"
                            label: "Sort Y labels"
                            values: ["Increasing", "Decreasing"]
                            startValue: "Increasing"
                        }

                        DropDown {
                            name: "aggregationFunY"
                            label: "By"
                            values: ["mean", "median"]
                            startValue: "mean"
                        }
                    }
                }
                ComponentsList {
                    name: "yAxisLabelRenamer"
                    title: qsTr("New Y-axis labels")
                    addItemManually: true
                    minimumItems: 0
                    rowComponent: Row {
                        TextField {
                            name: "originalYLabel"
                            label: qsTr("Original label")
                            fieldWidth: 100
                        }
                        TextField {
                            name: "newYLabel"
                            label: qsTr("New label")
                            fieldWidth: 150
                        }
                    }
                }
            }

            // -------------------------------------------------------------------
            // Title, caption, annotation
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Title, caption, annotation")
                columns:1

                Group {
                    CheckBox {
                        name: "addTitlePlotBuilder"
                        label: "Add title"

                        TextField {
                            name: "titlePlotBuilder"
                            label: qsTr("Title:")
                            placeholderText: qsTr("Enter the plot title here")
                            fieldWidth: 300
                        }
                    }
                }

                Group {
                    CheckBox {
                        name: "addCaptionPlotBuilder"
                        label: qsTr("Add caption")
                    }
                }

                TextArea {
                    name: "captionPlotBuilder"
                    height: 100
                }

                ComponentsList {
                    name: "annotationPlotBuilder"
                    title: qsTr("Annotations")
                    addItemManually: true
                    minimumItems: 0

                    rowComponent: Row {
                        TextField {
                            name: "annotationText"
                            label: qsTr("Text")
                            placeholderText: qsTr("$italic(p)$")
                            fieldWidth: 150
                        }
                        DoubleField {
                            name: "annotationX"
                            label: qsTr("X")
                            defaultValue: 3
                            fieldWidth: 60
                        }
                        DoubleField {
                            name: "annotationY"
                            label: qsTr("Y")
                            defaultValue: 3
                            fieldWidth: 60
                        }

                        DoubleField {
                            name: "annotationSize"
                            label: qsTr("Size")
                            defaultValue: 6
                        }
                    }
                }
            }


            Label {
                text: qsTr("Style and legend")
                wrapMode: Text.Wrap
                color: "black"
            }


            Section{
                title:qsTr("Plot style")
                columns:3

                Group{
                    columns:3
                    DropDown {
                        name: "plotStyle"
                        label: qsTr("Plot style")
                        values: ["JASP", "ggplotgray", "ggpubr", "PlotBuilder"]
                        indexDefaultValue: 0
                    }
                    DoubleField {
                        name: "baseFontSize"
                        label: "Font base size"
                        value: 18
                    }

                    DropDown {
                        name: "colorsAll"
                        label: qsTr("Color schemes")
                        indexDefaultValue: 5
                        values: [
                            { label: "Discrete: Friendly", value: "colors_discrete_friendly" },
                            { label: "Discrete: Seaside", value: "colors_discrete_seaside" },
                            { label: "Discrete: Apple", value: "colors_discrete_apple" },
                            { label: "Discrete: Friendly Long", value: "colors_discrete_friendly_long" },
                            { label: "Discrete: Okabe Ito", value: "colors_discrete_okabeito" },
                            { label: "Discrete: IBM", value: "colors_discrete_ibm" },
                            { label: "Discrete: Metro", value: "colors_discrete_metro" },
                            { label: "Discrete: Candy", value: "colors_discrete_candy" },
                            { label: "Discrete: Alger", value: "colors_discrete_alger" },
                            { label: "Discrete: Rainbow", value: "colors_discrete_rainbow" },
                            { label: "Continuous: Viridis", value: "colors_continuous_viridis" },
                            { label: "Continuous: Magma", value: "colors_continuous_magma" },
                            { label: "Continuous: Inferno", value: "colors_continuous_inferno" },
                            { label: "Continuous: Plasma", value: "colors_continuous_plasma" },
                            { label: "Continuous: Cividis", value: "colors_continuous_cividis" },
                            { label: "Continuous: Rocket", value: "colors_continuous_rocket" },
                            { label: "Continuous: Mako", value: "colors_continuous_mako" },
                            { label: "Continuous: Turbo", value: "colors_continuous_turbo" },
                            { label: "Continuous: Blue Pink Yellow", value: "colors_continuous_bluepinkyellow" },
                            { label: "Diverging: Blue to Red", value: "colors_diverging_blue2red" },
                            { label: "Diverging: Blue to Brown", value: "colors_diverging_blue2brown" },
                            { label: "Diverging: BuRd", value: "colors_diverging_BuRd" },
                            { label: "Diverging: BuYlRd", value: "colors_diverging_BuYlRd" },
                            { label: "Diverging: Spectral", value: "colors_diverging_spectral" },
                            { label: "Diverging: Icefire", value: "colors_diverging_icefire" }
                        ]
                    }
                }

                Group {
                    columns:2
                    Group {
                        columns:1
                        TextField {
                            name: "customColors"
                            label: qsTr("Custom colors")
                            placeholderText: qsTr("e.g. red, blue, ##ff5733")
                            fieldWidth: 150
                        }



                    Label {
                        text: qsTr("Note: For available colors, see <a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>this page</a>.")
                        wrapMode: TextBrowser.Wrap
                        textFormat: TextBrowser.RichText

                        MouseArea {
                            anchors.fill: parent
                            onClicked: {
                                Qt.openUrlExternally("https://r-charts.com/colors/")
                            }
                        }
                     }
                    }

                    Group {
                        columns:1
                        title: qsTr("Margins")
                        DoubleField {
                            name: "topMargin"
                            label: "Top"
                            value: 10
                        }
                        DoubleField {
                            name: "bottomMargin"
                            label: "Bottom"
                            value: 10
                        }
                        DoubleField {
                            name: "leftMargin"
                            label: "Left"
                            value: 10
                        }
                        DoubleField {
                            name: "rightMargin"
                            label: "Right"
                            value: 10
                        }
                    }}





            }

            // -------------------------------------------------------------------
            // Edit style and colors
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Adjust legend")

                Group {
                    title: "Legend settings"
                    columns:2

                    DropDown {
                        name: "legendPosistionPlotBuilder"
                        label: qsTr("Legend position")
                        id: legendPosistionPlotBuilder
                        indexDefaultValue: 0
                        fieldWidth: 150
                        values: [
                            {label: qsTr("Right"), value: "right"},
                            {label: qsTr("Left"), value: "left"},
                            {label: qsTr("Bottom"), value: "bottom"},
                            {label: qsTr("Top"), value: "top"},
                            {label: qsTr("No legend"), value: "none"}
                        ]
                    }

                    CheckBox {
                        name: "removeLegendTitle"
                        label: qsTr("Remove legend title")
                    }

                    ComponentsList {
                        name: "colorLabelRenamer"
                        title: qsTr("New color labels")
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
            }

            Label {
                text: qsTr("Additional options")
                wrapMode: Text.Wrap
                color: "black"
            }


            Section {
                title: qsTr("P value brackets")

                ComponentsList {
                    name: "pairwiseComparisons"
                    title: qsTr("Pairwise comparisons table")

                    addItemManually: true
                    minimumItems: 0
                    maximumItems: -1


                    rowComponent: Row {


                        Group {
                            title: qsTr("Compared groups")
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

                        Group {
                            title: qsTr("P value")
                            TextField {
                                name: "pAdj"
                                label: qsTr("p.adj")
                                fieldWidth: 70
                                defaultValue: "* or 0.49"
                            }

                            TextField {
                                name: "labelcolor"
                                label: qsTr("p.adj")
                                fieldWidth: 70
                                defaultValue: "black"
                            }

                            DoubleField {
                                name: "labelSizePValue"
                                label: qsTr("Label size")
                                defaultValue: 4.5
                            }

                        }

                        Group {
                            title: qsTr("Brackets")
                            DoubleField {
                                name: "yPositionPValue"
                                label: qsTr("Position on Y")
                                decimals: 2
                                fieldWidth: 70
                                value: 70
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
                                value: 1
                            }

                        }


                        Group {
                            title: qsTr("Grouping")
                            DropDown {
                                name: "GroupPValue"
                                label: qsTr("Group level")
                                source: [ { name: "variableColorPlotBuilder", use: "levels" } ]
                            }


                            DropDown {
                                name: "ColumnPValue"
                                label: qsTr("Column")
                                source: [ { name: "columnsvariableSplitPlotBuilder", use: "levels" } ]
                            }

                            DropDown {
                                name: "RowPValue"
                                label: qsTr("Row")
                                source: [ { name: "rowsvariableSplitPlotBuilder", use: "levels" } ]
                            }

                        }

                    }
                }
            }
        }
    }

    Section {
        title: qsTr("Plot layout")
        columns:4

        Group {
            title: qsTr("Arrange the plots by column")

            // Relative width of the columns
            TextField {
                name: "columnWidthInput"
                label: qsTr("Relative width of the columns")
                placeholderText: qsTr("1,1")
                fieldWidth: 300
            }

            // Row Specifications ComponentsList
            ComponentsList {
                id: rowSpecifications
                name: "rowSpecifications"
                title: qsTr("Specify layout")
                addItemManually: true

                rowComponent: Row {
                    Group {
                        title: qsTr("Plots")

                        TextField {
                            name: "plotIDs"
                            label: qsTr("Plot IDs")
                            placeholderText: qsTr("PlotID1, PlotID2")
                            fieldWidth: 120
                        }

                        TextField {
                            name: "rowHeightsColumn"
                            label: qsTr("Plot heights") // Javítottuk a "heigths" helyett
                            placeholderText: qsTr("1,1")
                            fieldWidth: 120
                        }
                    }

                    Group {
                        title: qsTr("Labels and legend")

                        TextField {
                            name: "labelsColumn"
                            label: qsTr("Labels")
                            placeholderText: qsTr("A, B, C, etc...")
                            fieldWidth: 90
                        }

                        CheckBox {
                            name: "getCommonLegendColumn"
                            label: qsTr("Collect legend")
                        }
                    }
                }
            }
        }

        Group {
            title: qsTr("Arrange the plots by plot")

            // Relative height within row layout
            TextField {
                name: "relHeightWithinRowLayout"
                label: qsTr("Relative height of the rows")
                placeholderText: qsTr("1,1")
                fieldWidth: 300
            }

            // Full Row Specifications ComponentsList
            ComponentsList {
                id: fullRowSpecifications
                name: "fullRowSpecifications"
                title: qsTr("Specify full row layout")
                addItemManually: true

                rowComponent: Row {
                    Group {
                        title: qsTr("Plots")

                        TextField {
                            name: "plotIDsFullRow"
                            label: qsTr("Plot IDs")
                            placeholderText: qsTr("PlotID1, PlotID2")
                            fieldWidth: 120
                        }

                        TextField {
                            name: "relWidthsFullRow"
                            label: qsTr("Plot widths")
                            placeholderText: qsTr("c(1,1)")
                            fieldWidth: 120
                        }
                    }

                    Group {
                        title: qsTr("Labels and legend")

                        TextField {
                            name: "labelsFullRow"
                            label: qsTr("Labels")
                            placeholderText: qsTr("A, B, etc...")
                            fieldWidth: 120
                        }

                        CheckBox {
                            name: "getCommonLegendRows"
                            label: qsTr("Legend")
                        }
                    }
                }
            }
        }


        Label {
            text: qsTr("Note: If you have a column and row arrangement,
the column will be the top part of the whole layout
and the row will be the bottom part.")
            wrapMode: Text.Wrap
            color: "black"
        }



        Group {
            title: qsTr("Additional Settings")

            Row {
                spacing: 20

                Group {
                    TextField {
                        name: "relativeHeight"
                        label: qsTr("Relative height of column and row arrangement")
                        placeholderText: qsTr("1,1")
                        fieldWidth: 150
                    }

                    DoubleField {
                        name: "labelSize"
                        label: qsTr("Label size")
                        value: 12
                    }

                    DoubleField {
                        name: "layoutWidth"
                        label: qsTr("Width")
                        value: 1000
                    }

                    DoubleField {
                        name: "layoutHeight"
                        label: qsTr("Height")
                        value: 1000
                    }

                    CheckBox {
                        name: "getCommonLegend"
                        label: qsTr("Common legend for the entire layout")
                    }
                }

                Group {
                    CheckBox {
                        name: "compilePlotGrid"
                        label: qsTr("Compile plot grid")
                    }
                }
            }
        }
    }
}

// End Form


