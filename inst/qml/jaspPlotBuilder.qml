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


    TabView {

        name: "PlotBuilderTab"
        newItemName: qsTr("Plot 1")

        rowComponent: Group {

            childControlsArea.anchors.leftMargin: jaspTheme.contentMargin

            Group{
                columns:2
                RadioButtonGroup {
                    id:						isRM
                    Layout.columnSpan:		1
                    name:					"isRM"
                    title:					qsTr("Repeated measurements")
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
                    preferredHeight: yesRM.checked ? 600  * jaspTheme.uiScale : 300 * jaspTheme.uiScale

                    infoLabel: qsTr("Input")
                    AvailableVariablesList { name: "allVariablesList"; id: allVariablesList}

                    AssignedVariablesList {
                        name: "variableRepeatedMeasures"
                        id: variableRepeatedMeasures
                        title: qsTr("Repeated Measures Variables")
                        visible: yesRM.checked
                        allowedColumns: ["scale"]
                        property bool active: yesRM.checked
                        onActiveChanged: {
                            while (!active && count > 0) {
                                itemDoubleClicked(0)
                            }
                        }
                    }

                    DropDown {
                        name: "rmValueOptionsDropDown"
                        label: qsTr("Repeated Measures Value as")
                        id: rmValueOptionsDropDown
                        visible: yesRM.checked
                        enabled: yesRM.checked && variableRepeatedMeasures.count > 0
                        onEnabledChanged: {
                            if (!enabled) {
                                currentIndex = -1
                            }
                            if (!enabled) {
                                currentIndex = 1
                            }
                        }
                        values: [
                            { label: qsTr("X-Axis Variable"), value: "rmValueAsX" },
                            { label: qsTr("Y-Axis Variable"), value: "rmValueAsY" }
                        ]
                        indexDefaultValue: 1   // alapértelmezett: Variable Y
                    }

                    DropDown {
                        name: "rmFactorOptionsDropDown"
                        label: qsTr("Repeated Measures Factor as")
                        id: rmFactorOptionsDropDown
                        visible: yesRM.checked
                        enabled: yesRM.checked && variableRepeatedMeasures.count > 0
                        onEnabledChanged: {
                            if (!enabled) {
                                currentIndex = -1
                            }
                            if (!enabled) {
                                currentIndex = 0
                            }
                        }
                        values: [
                            { label: qsTr("X-Axis Variable"), value: "rmFactorAsX" },
                            { label: qsTr("Y-Axis Variable"), value: "rmFactorAsY" },
                            { label: qsTr("Group Variable"), value: "rmFactorAsGroup" },
                            { label: qsTr("Split (Columns)"), value: "rmFactorAsColumnSplit" },
                            { label: qsTr("Split (Rows)"), value: "rmFactorAsRowSplit" },
                            { label: qsTr("Grid"), value: "rmFactorAsGrid" },
                            { label: qsTr("Not visible"), value: "rmFactorNotVisible" }
                        ]
                        indexDefaultValue: 0   // alapértelmezett: Variable X
                    }

                    AssignedVariablesList {
                        name: "variableXPlotBuilder"
                        title: qsTr("X-Axis Variable")
                        id: variableXPlotBuilder
                        allowedColumns: ["scale", "ordinal", "nominal"]
                        minLevels: 2
                        singleVariable: true
                        info: qsTr("Select the variable for the X-axis.")
                        property bool active: noRM.checked
                        onActiveChanged: if (!active && count > 0) itemDoubleClicked(0)
                        enabled: isRM.value === "noRM" || (isRM.value === "RM" && variableRepeatedMeasures.count > 0 &&
                                                            (rmFactorOptionsDropDown.value !== "rmFactorAsX" && rmValueOptionsDropDown.value !== "rmValueAsX"))
                        onEnabledChanged: {
                            if (!enabled) {
                                   while (count > 0) {
                                       itemDoubleClicked(0)
                                   }
                               }
                        }
                    }

                    AssignedVariablesList {
                        name: "variableYPlotBuilder"
                        title: qsTr("Y-Axis Variable")
                        allowedColumns: ["scale", "ordinal", "nominal"]
                        id: variableYPlotBuilder
                        singleVariable: true
                        // visible: noRM.checked
                        property bool active: noRM.checked
                        onActiveChanged: if (!active && count > 0) itemDoubleClicked(0)
                        info: qsTr("Select the variable for the Y-axis.")
                        enabled: isRM.value === "noRM" || (isRM.value === "RM" && variableRepeatedMeasures.count > 0 &&
                                                            (rmFactorOptionsDropDown.value !== "rmFactorAsY" && rmValueOptionsDropDown.value !== "rmValueAsY"))
                        onEnabledChanged: {
                            if (!enabled) {
                                   while (count > 0) {
                                       itemDoubleClicked(0)
                                   }
                               }
                        }
                    }

                    AssignedVariablesList {
                        name: "variableColorPlotBuilder"
                        title: qsTr("Group Variable")
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
                        enabled: isRM.value === "noRM" || (isRM.value === "RM" && variableRepeatedMeasures.count > 0 &&
                                                            (rmFactorOptionsDropDown.value !== "rmFactorAsGroup"))

                        onEnabledChanged: {
                            if (!enabled) {
                                   while (count > 0) {
                                       itemDoubleClicked(0)
                                   }
                               }
                        }
                    }

                    AssignedVariablesList {
                        name: "columnsvariableSplitPlotBuilder"
                        title: qsTr("Split (Columns)")
                        id: columnsvariableSplitPlotBuilder
                        allowedColumns: ["ordinal", "nominal"]
                        singleVariable: true
                        info: qsTr("You can choose a variable to split the plots into columns.")
                        enabled: isRM.value === "noRM" || (isRM.value === "RM" && variableRepeatedMeasures.count > 0 &&
                                                           (rmFactorOptionsDropDown.value !== "rmFactorAsColumnSplit"))

                        onEnabledChanged: {
                            if (!enabled) {
                                   while (count > 0) {
                                       itemDoubleClicked(0)
                                   }
                               }
                        }
                    }

                    AssignedVariablesList {
                        name: "rowsvariableSplitPlotBuilder"
                        title: qsTr("Split (Rows)")
                        id: rowsvariableSplitPlotBuilder
                        allowedColumns: ["ordinal", "nominal"]
                        singleVariable: true
                        info: qsTr("You can choose a variable to split the plots into rows.")
                        enabled: isRM.value === "noRM" || (isRM.value === "RM" && variableRepeatedMeasures.count > 0 &&
                                                           (rmFactorOptionsDropDown.value !== "rmFactorAsRowSplit"))
                        onEnabledChanged: {
                            if (!enabled) {
                                   while (count > 0) {
                                       itemDoubleClicked(0)
                                   }
                               }
                        }

                    }

                    AssignedVariablesList {
                        name: "gridVariablePlotBuilder"
                        title: qsTr("Grid")
                        id: gridVariablePlotBuilder
                        allowedColumns: ["ordinal", "nominal"]
                        singleVariable: true
                        info: qsTr("You can choose a variable to make a grid.")
                        enabled: isRM.value === "noRM" || (isRM.value === "RM" && variableRepeatedMeasures.count > 0 &&
                                                           (rmFactorOptionsDropDown.value !== "rmFactorAsGrid"))
                        onEnabledChanged: {
                            if (!enabled) {
                                   while (count > 0) {
                                       itemDoubleClicked(0)
                                   }
                               }
                        }
                    }
                }
            } // End variables form

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
                    info: qsTr("Check this option to show individual data points on the plot.")
                    columns: 4
                    enabled: (
                                 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                 ||

                                 // 2. If repeated measures (yesRM) and at least one of the following is true:
                                 (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                      // a) X variable is assigned and either drop-down defines Y
                                      (variableXPlotBuilder.count > 0 &&
                                       (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                      ||

                                      // b) Y variable is assigned and either drop-down defines X
                                      (variableYPlotBuilder.count > 0 &&
                                       (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                      ||

                                      // c) Drop-downs define both X and Y roles separately
                                      ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                       (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                      ))
                                 )
                    onEnabledChanged: {
                        if (!enabled) {
                            checked = false;
                        }
                    }

                    Group {
                        DoubleField {
                            name: "pointsizePlotBuilder"
                            id: pointsizePlotBuilder
                            label: qsTr("Point size")
                            value: 3
                            min: 0
                            max: 10
                        }
                    }

                    Group {
                        DoubleField {
                            name: "jitterhPlotBuilder"
                            id: jitterhPlotBuilder
                            label: qsTr("Jitter height")
                            value: 0.3
                            min: 0
                            max: 10
                        }
                        DoubleField {
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
                            value: 0.5
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

                    Group{
                        CheckBox {
                            name: "blackOutlineDataPoint"
                            label: qsTr("Black outline")
                            checked: false
                        }

                        CheckBox {
                            name: "emptyCircles"
                            label: qsTr("Empty circles")
                            checked: false
                        }
                    }

                }

                CheckBox {
                    name: "connectRMPlotBuilder"
                    label: qsTr("Connect data points (requires repeated measures)")
                    id: connectRMPlotBuilder
                    checked: false
                    enabled: isRM.value === "RM" & addDataPoint.checked
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
                        label: qsTr("Boxplot")
                        info: qsTr("Check this option to create boxplot. Boxplot and violin plot require both x-axis and y-axis variables.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                            defaultValue: 0.8}


                        DoubleField {
                            name: "alphaBoxplotPlotBuilder"
                            label: qsTr("Transparency")
                            id: alphaBoxplotPlotBuilder
                            defaultValue: 0.8
                            min: 0
                            max: 1
                        }

                        DoubleField {
                            name: "widthLineBoxplotPlotBuilder"
                            label: qsTr("Line width")
                            id: widthLineBoxplotPlotBuilder
                            defaultValue: 0.8
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
                            checked: false
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
                        label: qsTr("Violin plot")
                        info: qsTr("Check this option to add a violin plot to your visualization. Boxplot and violin plot require both x-axis and y-axis variables.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                            defaultValue: 0.8
                        }

                        DoubleField {
                            name: "alphaViolinPlotBuilder"
                            label: qsTr("Transparency")
                            id: alphaViolinPlotBuilder
                            defaultValue: 0.8
                            min: 0
                            max: 1
                        }

                        DoubleField {
                            name: "linewidthViolinPlotBuilder"
                            label: qsTr("Line width")
                            id: linewidthViolinPlotBuilder
                            defaultValue: 0.8
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
                            defaultValue: "0.25, 0.5, 0.75"
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
                        info: qsTr("Enable to add dashed lines to the plot.")
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
                        info: qsTr("Enable to add count dots to the plot.")
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
                        }
                    }

                    // Count Line
                    CheckBox {
                        name: "addCountLine"
                        label: qsTr("Count line")
                        info: qsTr("Enable to add count lines to the plot.")
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
                        info: qsTr("Enable to add a count area to the plot.")
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
                        info: qsTr("Enable to add count values to the plot.")
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
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
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
                columns:2

                Label {
                    text: qsTr("Required: Group Variable AND X OR Y-Axis Variable")
                    wrapMode: Text.Wrap
                    color: "black"
                    Layout.columnSpan: 2
                }


                RadioButtonGroup {
                    Layout.columnSpan: 2
                    id: propModeGroup
                    name: "propMode"
                    title: qsTr("Proportion mode")
                    radioButtonsOnSameRow: true
                    columns: 2
                    RadioButton {
                        value: "absolute"
                        label: qsTr("Absolute")
                        checked: true
                    }
                    RadioButton {
                        value: "relative"
                        label: qsTr("Relative")
                    }
                }

                // Use a GridLayout to arrange the two checkboxes side-by-side
                GridLayout {
                    columns: 2
                    columnSpacing: 40

                    CheckBox {
                        name: "addBarStack"
                        label: qsTr("Bar stack")
                        info: qsTr("Add a bar stack to the plot. The mode (absolute or relative) is set by the Proportion Mode above.")
                        enabled: (
                            // noRM :
                            ( noRM.checked &&
                              ((variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0) && variableColorPlotBuilder.count > 0)
                            )
                            ||
                            // yesRM :
                            ( yesRM.checked && variableRepeatedMeasures.count > 0 &&
                              (
                                 ((variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0) && rmFactorOptionsDropDown.value === "rmFactorAsGroup")
                                 ||
                                 (variableColorPlotBuilder.count > 0 && (rmFactorOptionsDropDown.value === "rmFactorAsX" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                              )
                            )
                        )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false
                            }
                        }
                        // Nested parameters
                        DoubleField {
                            name: "alphaBarStack"
                            label: qsTr("Transparency")
                            defaultValue: 0.8
                            min: 0
                            max: 1
                        }
                        CheckBox {
                            name: "reverseBarStack"
                            label: qsTr("Reverse order")
                            checked: false
                        }
                    }

                    CheckBox {
                        name: "addAreaStack"
                        label: qsTr("Area stack")
                        info: qsTr("Add an area stack to the plot. The mode (absolute or relative) is set by the Proportion Mode above.")
                        enabled: (
                            // noRM :
                            ( noRM.checked &&
                              ((variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0) && variableColorPlotBuilder.count > 0)
                            )
                            ||
                            // yesRM :
                            ( yesRM.checked && variableRepeatedMeasures.count > 0 &&
                              (
                                 ((variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0) && rmFactorOptionsDropDown.value === "rmFactorAsGroup")
                                 ||
                                 (variableColorPlotBuilder.count > 0 && (rmFactorOptionsDropDown.value === "rmFactorAsX" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                              )
                            )
                        )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false
                            }
                        }
                        // Nested parameters
                        DoubleField {
                            name: "alphaAreaStack"
                            label: qsTr("Transparency")
                            defaultValue: 0.4
                            min: 0
                            max: 1
                        }
                        DoubleField {
                            name: "linewidthAreaStack"
                            label: qsTr("Line width")
                            defaultValue: 0.25
                            min: 0
                        }
                        CheckBox {
                            name: "reverseAreaStack"
                            label: qsTr("Reverse order")
                            checked: false
                        }
                        CheckBox {
                            name: "replaceNaAreaStack"
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
                    text: qsTr("Required: X AND Y-Axis Variables")
                    wrapMode: Text.Wrap
                    color: "black"
                }


                GridLayout {
                    columns: 3
                    rowSpacing: 40
                    columnSpacing: 40

                    // Mean bar
                    CheckBox {
                        name: "addMeanBar"
                        label: qsTr("Mean bar")
                        info: qsTr("Enable to add a mean bar to the plot.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            name: "widthMeanBar"
                            label: qsTr("Bar width")
                            defaultValue: 0.8
                        }
                    }

                    // Mean dash
                    CheckBox {
                        name: "addMeanDash"
                        label: qsTr("Mean dash")
                        info: qsTr("Enable to add dashed mean lines to the plot.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

                        DoubleField {
                            name: "dodgeMeanDash"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "dashwidthMeanDash"
                            label: qsTr("Dash width")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMeanDash"
                            label: qsTr("Line width")
                            defaultValue: 1
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

                        DoubleField {
                            name: "dodgeMeanLine"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMeanLine"
                            label: qsTr("Line width")
                            defaultValue: 1
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

                        DoubleField {
                            name: "dodgeMeanArea"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            defaultValue: 0.5
                            min: -Infinity

                        }
                        DoubleField {
                            name: "vjustMeanValue"
                            label: qsTr("Vertical justification")
                            defaultValue: -0.5
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
                    text: qsTr("Required: X AND Y-Axis Variables")
                    wrapMode: Text.Wrap
                    color: "black"
                }

                GridLayout {
                    columns: 3
                    rowSpacing: 40
                    columnSpacing: 40

                    // Median bar
                    CheckBox {
                        name: "addMedianBar"
                        label: qsTr("Median bar")
                        info: qsTr("Enable to add a median bar to the plot.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            name: "widthMedianBar"
                            label: qsTr("Bar width")
                            defaultValue: 0.8

                        }
                    }

                    // Median dash
                    CheckBox {
                        name: "addMedianDash"
                        label: qsTr("Median dash")
                        info: qsTr("Enable to add dashed median lines to the plot.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

                        DoubleField {
                            name: "dodgeMedianDash"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "dashwidthMedianDash"
                            label: qsTr("Dash width")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMedianDash"
                            label: qsTr("Line width")
                            defaultValue: 1
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

                        DoubleField {
                            name: "dodgeMedianLine"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
                        }
                        DoubleField {
                            name: "linewidthMedianLine"
                            label: qsTr("Line width")
                            defaultValue: 1
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

                        DoubleField {
                            name: "dodgeMedianArea"
                            label: qsTr("Dodge")
                            defaultValue: 0.8
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            defaultValue: 0.5
                            min: -Infinity
                        }
                        DoubleField {
                            name: "vjustMedianValue"
                            label: qsTr("Vertical justification")
                            defaultValue: -0.5
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
                title: qsTr("Error bars and ribbons (range, SD, SEM, 95% CI)")

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

                    // Range Error Bar
                    CheckBox {
                        name: "addRangeErrorBar"
                        label: qsTr("Range error bar")
                        info: qsTr("Enable to add range error bars to the plot.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            defaultValue: 1
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
                        label: qsTr("SD error bar")
                        info: qsTr("Enable to add standard deviation error bars to the plot.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            defaultValue: 1
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            defaultValue: 1
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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                            defaultValue: 1
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

                GridLayout {
                    columns: 2
                    rowSpacing: 40
                    columnSpacing: 70

                    // Range Ribbon
                    CheckBox {
                        name: "addRangeRibbon"
                        label: qsTr("Range ribbon")
                        info: qsTr("Enable to add a range ribbon to the plot.")
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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
                        enabled: (
                                     // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                     (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                     ||

                                     // 2. If repeated measures (yesRM) and at least one of the following is true:
                                     (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                          // a) X variable is assigned and either drop-down defines Y
                                          (variableXPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                          ||

                                          // b) Y variable is assigned and either drop-down defines X
                                          (variableYPlotBuilder.count > 0 &&
                                           (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                          ||

                                          // c) Drop-downs define both X and Y roles separately
                                          ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                           (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                          ))
                                     )
                        onEnabledChanged: {
                            if (!enabled) {
                                checked = false;
                            }
                        }

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


                Label {
                    text: qsTr("Required: individual data points (Curve fit)")
                    wrapMode: Text.Wrap
                    color: "black"
                }


                CheckBox {
                    name: "addCurveFitPlotBuilder"
                    label: qsTr("Add curve fit")
                    info: "With this option you can fit a model on the data points."
                    enabled: addDataPoint.checked
                    onEnabledChanged: {
                        if (!enabled) {
                            checked = false;
                        }
                    }

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
                            defaultValue: 1
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
                            label: qsTr("X-axis intercept")
                            placeholderText: qsTr("e.g. 0.5, or c(1,2)")
                            fieldWidth: 100
                        }

                        TextField {
                            name: "yReferenceLine"
                            label: qsTr("Y-axis intercept")
                            placeholderText: qsTr("e.g. 0.5, or c(1,2)")
                            fieldWidth: 100
                        }
                    }

                    Group {
                        DoubleField {
                            name: "linewidhtReferenceLines"
                            label: qsTr("Line width")
                            defaultValue: 1
                        }

                        TextField {
                            name: "colorReferenceLine"
                            label: qsTr("Line color")
                            placeholderText: qsTr("e.g. black, #ff5733")
                            defaultValue: "lightgray"
                            fieldWidth: 100
                        }

                        Label {
                            text: qsTr("Note: For available colors, see %1 this page %2").arg("<a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>").arg("</a>.")
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

                CheckBox {
                    name: "addIdentityLinePlotBuilder"
                    label: qsTr("Add identity line")
                    info: "Add identity line (y = x)."

                    CheckBox{
                        name: "reversedirectionIdentityLine"
                        label: "Reverse direction"
                    }

                    TextField {
                        name: "colorIdentityLine"
                        label: qsTr("Line color")
                        placeholderText: qsTr("e.g. black, #ff5733")
                        defaultValue: "lightgray"
                        fieldWidth: 100
                    }

                }

                Label {
                    text: qsTr("Note: For available colors, see %1this page%2")
                    .arg("<a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>")
                    .arg("</a>")
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

                title: qsTr("X-axis")


                TextField {
                    label: qsTr("Title")
                    name: "titleXPlotBuilder"
                    placeholderText: qsTr("Enter x-axis title")
                    fieldWidth: 300
                    info: qsTr("Specify the title for the x-axis")
                    Layout.columnSpan: 3
                }

                Group {
                    title:qsTr("Limits")
                    columns:1

                    TextField {
                        name: "limitFromX"
                        label: qsTr("From")
                        fieldWidth: 40
                    }

                    TextField {
                        name: "limitToX"
                        label: qsTr("To")
                        fieldWidth: 40
                    }
                }

                Group {
                    title: "Breaks"
                    columns:1

                    TextField {
                        name: "breakFromX"
                        label: qsTr("From")
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakToX"
                        label: qsTr("To")
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakByX"
                        label: qsTr("By")
                        fieldWidth: 40
                    }
                }

                Group {
                    columns: 1
                    title: qsTr("Labels")
                    CheckBox {
                        name: "rotateXLabel"
                        label: "Rotate"
                    }

                    CheckBox {
                        name: "cutShortScale"
                        label: qsTr("Shorten")
                        info: qsTr("Whether to shorten axis labels using K for thousand, M for million, and so on.")
                    }


                    CheckBox {
                        name: "enableSort"
                        label: qsTr("Sort")
                        checked: false  // Alapértelmezett állapot


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

                title: qsTr("Y-axis")

                TextField {
                    label: qsTr("Title")
                    name: "titleYPlotBuilder"
                    placeholderText: qsTr("Enter y-axis title")
                    fieldWidth: 300
                    info: qsTr("Specify the title for the Y-axis.")
                    Layout.columnSpan: 3
                }

                Group {
                    title: qsTr("Limits")
                    columns: 1

                    TextField {
                        name: "limitFromY"
                        label: qsTr("From")
                        fieldWidth: 40
                    }

                    TextField {
                        name: "limitToY"
                        label: qsTr("To")
                        fieldWidth: 40
                    }
                }

                Group {
                    title: qsTr("Breaks")
                    columns: 1

                    TextField {
                        name: "breakFromY"
                        label: qsTr("From")
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakToY"
                        label: qsTr("To")
                        fieldWidth: 40
                    }

                    TextField {
                        name: "breakByY"
                        label: qsTr("By")
                        fieldWidth: 40
                    }
                }

                Group {
                    title: qsTr("Labels")
                    columns: 1

                    CheckBox {
                        name: "rotateYLabel"
                        label: qsTr("Rotate")
                    }

                    CheckBox {
                        name: "cutShortScaleY"
                        label: qsTr("Shorten")
                        info: qsTr("Whether to shorten axis labels using K for thousand, M for million, and so on.")
                    }

                    // If you want to enable sorting on the Y axis:
                    CheckBox {
                        name: "enableSortY"
                        label: qsTr("Enable sorting")
                        checked: false

                        DropDown {
                            name: "sortYLabelsOrder"
                            label: qsTr("Sort")
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

                ComponentsList {
                    name: "annotationPlotBuilder"
                    id: annotationPlotBuilder
                    title: qsTr("Annotations")
                    addItemManually: true
                    minimumItems: 0
                    enabled: (
                                 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                 (noRM.checked && variableXPlotBuilder.count > 0 || variableYPlotBuilder.count > 0)

                                 ||

                                 // 2. If repeated measures (yesRM) and at least one of the following is true:
                                 (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                      // a) Y variable is assigned and either drop-down defines Y
                                      (variableYPlotBuilder.count > 0 ||
                                       (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                      ||

                                      // b) X variable is assigned and either drop-down defines X
                                      (variableXPlotBuilder.count > 0 ||
                                       (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                      ||

                                      // c) Drop-downs define both X and Y roles separately
                                      ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                       (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                      ))
                                 )
                    onEnabledChanged: {
                        if (!enabled) {
                                   // remove all components one by one
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
                                        visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsColumnSplit"
                                        name: "ColumnAnnotation"
                                        label: qsTr("Column")
                                        source: [ { name: "columnsvariableSplitPlotBuilder", use: "levels" } ]
                                    }
                                    DropDown {
                                        visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsColumnSplit" && isRM.value === "RM"
                                        name: "RMColumnAnnotation"
                                        label: qsTr("Column")
                                        source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                        enabled: isRM.value === "RM"
                                        onEnabledChanged: {
                                            if (!enabled) {
                                                currentIndex = -1
                                            }
                                        }
                                    }

                                    DropDown {
                                        visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsRowSplit"
                                        name: "RowAnnotation"
                                        label: qsTr("Row")
                                        source: [ { name: "rowsvariableSplitPlotBuilder", use: "levels" } ]
                                    }
                                    DropDown {
                                        visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsRowSplit" && isRM.value === "RM"
                                        name: "RMRowAnnotation"
                                        label: qsTr("Row")
                                        source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                        enabled: isRM.value === "RM"
                                        onEnabledChanged: {
                                            if (!enabled) {
                                                currentIndex = -1
                                            }
                                        }
                                    }

                                    DropDown {
                                        visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsGrid"
                                        name: "GridAnnotation"
                                        label: qsTr("Grid")
                                        source: [ { name: "gridVariablePlotBuilder", use: "levels" } ]
                                    }
                                    DropDown {
                                        visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsGrid" && isRM.value === "RM"
                                        name: "RMGridAnnotation"
                                        label: qsTr("Grid")
                                        source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                        enabled: isRM.value === "RM"
                                        onEnabledChanged: {
                                            if (!enabled) {
                                                currentIndex = -1
                                            }
                                        }
                                    }
                                }


                            }

                            Group {
                                title:qsTr("Appearance")

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

                Group {
                    title: qsTr("Color settings")
                    columns: 1

                    RadioButtonGroup {
                        name: "colorByGroup"
                        title: qsTr("Color by")
                        radioButtonsOnSameRow: true
                        columns: 4

                        RadioButton {
                            value: "none"
                            label: qsTr("None")
                            enabled: true
                            checked: !colorXRadio.checked && !colorYRadio.checked
                            onEnabledChanged: { if (!enabled && checked) { checked = false; } }
                        }
                        RadioButton {
                            value: "grouping"
                            label: qsTr("Group variable")
                            enabled: variableColorPlotBuilder.count > 0
                            checked: variableColorPlotBuilder.count > 0
                            onEnabledChanged: { if (!enabled && checked) { checked = false; } }
                        }
                        RadioButton {
                            id: colorXRadio
                            value: "x"
                            label: qsTr("X variable")
                            enabled: variableXPlotBuilder.count > 0 && variableColorPlotBuilder.count === 0
                            checked: false
                            onEnabledChanged: { if (!enabled && checked) { checked = false; } }
                        }
                        RadioButton {
                            id: colorYRadio
                            value: "y"
                            label: qsTr("Y variable")
                            enabled: variableYPlotBuilder.count > 0 && variableColorPlotBuilder.count === 0
                            checked: false
                            onEnabledChanged: { if (!enabled && checked) { checked = false; } }
                        }
                        RadioButton {
                            value: "rm"
                            label: qsTr("Repeated measures")
                            enabled: yesRM.checked
                            checked: false
                            onEnabledChanged: { if (!enabled && checked) { checked = false; } }
                        }

                        RadioButton {
                            value: "splitColumn"
                            label: qsTr("Split (column)")
                            enabled: columnsvariableSplitPlotBuilder.count > 0
                            checked: false
                            onEnabledChanged: { if (!enabled && checked) { checked = false; } }
                        }

                        RadioButton {
                            value: "splitRow"
                            label: qsTr("Split (rows)")
                            enabled: rowsvariableSplitPlotBuilder.count > 0
                            checked: false
                            onEnabledChanged: { if (!enabled && checked) { checked = false; } }
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
                        placeholderText: qsTr("e.g. red, blue, ##ff5733")
                        fieldWidth: 150
                    }
                    Label {
                        text: qsTr("Note: For available colors, see %1 this page %2")
                        .arg("<a href='https://r-charts.com/colors/' style='color: blue; text-decoration: underline;'>")
                        .arg("</a>.")
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

            Section {
                title: qsTr("Size and margins")
                columns: 1

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

                // Jobb oszlop: Margók
                Group {
                    title: qsTr("Margins")
                    columns: 4

                    DoubleField {
                        name: "topMargin"
                        label: qsTr("Top")
                        value: 10
                    }
                    DoubleField {
                        name: "bottomMargin"
                        label: qsTr("Bottom")
                        value: 10
                    }
                    DoubleField {
                        name: "leftMargin"
                        label: qsTr("Left")
                        value: 10
                    }
                    DoubleField {
                        name: "rightMargin"
                        label: qsTr("Right")
                        value: 10
                    }
                }
            }

            // -------------------------------------------------------------------
            // Edit style and colors
            // -------------------------------------------------------------------
            Section {
                title: qsTr("Legend")

                Group {
                    columns:2

                    DropDown {
                        name: "legendPosistionPlotBuilder"
                        label: qsTr("Position")
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
                        label: qsTr("Remove title")
                    }
                }
                ComponentsList {
                    name: "colorLabelRenamer"
                    title: qsTr("Rename labels")
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

            Label {
                text: qsTr("Split and grid control")
                wrapMode: Text.Wrap
                color: "black"
                font.bold: true
            }

            Section {
                title: qsTr("Split control")

                GridLayout {
                    columns: 2
                    rowSpacing: 10
                    columnSpacing: 20
                    enabled: columnsvariableSplitPlotBuilder.count > 0


                    Label {
                        text: qsTr("Scale and axis settings")
                    }

                    Column {
                        spacing: 10

                        DropDown {
                            name: "scales"
                            label: qsTr("Scale range")
                            values: [
                                { label: qsTr("shared across all facets"), value: "fixed" },
                                { label: qsTr("vary across X"), value: "free_x" },
                                { label: qsTr("vary across Y"), value: "free_y" },
                                { label: qsTr("vary across both axes"), value: "free" }
                            ]
                            startValue: "fixed"
                        }

                        DropDown {
                            name: "axes"
                            label: qsTr("Show axis lines")
                            values: [
                                { label: qsTr("only on outer panels"), value: "margins" },
                                { label: qsTr("on all X axes"), value: "all_x" },
                                { label: qsTr("on all Y axes"), value: "all_y" },
                                { label: qsTr("on all axes"), value: "all" }
                            ]
                            startValue: "margins"
                        }

                        DropDown {
                            name: "axisLabels"
                            label: qsTr("Axis label visibility")
                            values: [
                                { label: qsTr("show on all axes"), value: "all" },
                                { label: qsTr("only on outer axes"), value: "margins" },
                                { label: qsTr("only on interior X axes"), value: "all_x" },
                                { label: qsTr("only on interior Y axes"), value: "all_y" }
                            ]
                            startValue: "all"
                        }
                    }

                    Label {
                        text: qsTr("Layout settings")
                    }

                    Column {
                        spacing: 10

                        RadioButtonGroup {
                            id: asTableGroup
                            name: "asTable"
                            title: qsTr("Highest value at")
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
                            values: [
                                { label: qsTr("same size"), value: "fixed" },
                                { label: qsTr("free width (X)"), value: "free_x" },
                                { label: qsTr("free height (Y)"), value: "free_y" },
                                { label: qsTr("free width and height"), value: "free" }
                            ]
                            startValue: "fixed"
                        }

                        CheckBox {
                            id: marginsCheckBox
                            name: "margins"
                            label: qsTr("Include marginal plots")
                            checked: false
                        }

                        // New text fields for X/Y axis titles:
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
                GridLayout {

                    enabled: gridVariablePlotBuilder.count > 0

                    columns: 2
                    rowSpacing: 10
                    columnSpacing: 20

                    Label {
                        text: qsTr("Layout settings")
                    }

                    Column {
                        spacing: 10

                        DoubleField {
                            name: "ncolFacetWrap"
                            label: qsTr("Number of columns")
                            defaultValue: gridVariablePlotBuilder.levels ? Math.floor(gridVariablePlotBuilder.levels / 2) + 1 : 0
                        }

                        DoubleField {
                            name: "nrowFacetWrap"
                            label: qsTr("Number of rows")
                            defaultValue: gridVariablePlotBuilder.levels ? Math.floor(gridVariablePlotBuilder.levels / 2) + 1 : 0
                        }


                        RadioButtonGroup {
                            name: "asTableFacetWrap"
                            title: qsTr("Highest value at")
                            radioButtonsOnSameRow: true

                            RadioButton {
                                value: "bottom-rightFacetWrap"
                                label: qsTr("bottomright")
                                checked: true
                            }
                            RadioButton {
                                value: "top-rightFacetWrap"
                                label: qsTr("top right")
                            }
                        }
                    }

                    Label {
                        text: qsTr("Scales and strip settings")
                    }

                    Column {
                        spacing: 10

                        DropDown {
                            name: "scalesFacetWrap"
                            label: qsTr("Scale range")
                            values: [
                                { label: qsTr("shared across all facets"), value: "fixed" },
                                { label: qsTr("vary across X"), value: "free_x" },
                                { label: qsTr("vary across Y"), value: "free_y" },
                                { label: qsTr("vary across both axes"), value: "free" }
                            ]
                            startValue: "fixed"
                        }

                        DropDown {
                            name: "stripPosition"
                            label: qsTr("Strip position")
                            values: [
                                { label: qsTr("top"), value: "top" },
                                { label: qsTr("bottom"), value: "bottom" },
                                { label: qsTr("left"), value: "left" },
                                { label: qsTr("right"), value: "right" }
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
                columns: 4

                Label {
                    text: qsTr("Required: X AND Y-Axis Variables")
                    wrapMode: Text.Wrap
                    color: "black"
                }


                Group{
                    columns: 2

                    Group{
                    columns: 1
                        TextField {
                            name: "labelcolor"
                            label: qsTr("P value color")
                            fieldWidth: 70
                            defaultValue: "black"
                        }

                        DoubleField {
                            name: "labelSizePValue"
                            label: qsTr("P value size")
                            defaultValue: 4.5
                        }
                    }

                    Group{
                        columns:1
                        DoubleField {
                            name: "yPositionPValue"
                            label: qsTr("Y-Axis position of the first P value")
                            decimals: 2
                            fieldWidth: 70
                            value: 70
                        }

                        DoubleField {
                            name: "stepDistance"
                            label: qsTr("Step distance")
                            decimals: 2
                            fieldWidth: 70
                            value: 0.15
                        }

                        Label {
                            text: qsTr("Note: If custom Y-Axis limits are set, the starting" + "\n" + "position for the P value must fall within the defined interval")
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
                                 // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                                 (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                                 ||

                                 // 2. If repeated measures (yesRM) and at least one of the following is true:
                                 (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                      // a) X variable is assigned and either drop-down defines Y
                                      (variableXPlotBuilder.count > 0 &&
                                       (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                      ||

                                      // b) Y variable is assigned and either drop-down defines X
                                      (variableYPlotBuilder.count > 0 &&
                                       (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                      ||

                                      // c) Drop-downs define both X and Y roles separately
                                      ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                       (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                      ))
                                 )

                    onEnabledChanged: {
                        if (!enabled) {
                                   // remove all components one by one
                                   for (var i = pairwiseComparisons.count - 1; i >= 0; --i) {
                                       pairwiseComparisons.removeItem(i)
                                   }
                               }
                    }

                    rowComponent: Row {

                        Group {
                        title: qsTr("Bracket ") + (rowIndex + 1)
                        columns: 3

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

                        Group{
                            title: qsTr("P value and brackets")

                            TextField {
                                name: "pAdj"
                                label: qsTr("P value")
                                fieldWidth: 70
                                defaultValue: "* or 0.49"
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

                        Group {
                            title: qsTr("Position")
                            columns: 2

                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsGroup"
                                name: "GroupPValue"  // Normál mód
                                label: qsTr("Group level")
                                source: [ { name: "variableColorPlotBuilder", use: "levels" } ]
                            }
                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsGroup" && isRM.value === "RM"
                                name: "RMGroupPValue"  // RM mód
                                label: qsTr("Group level")
                                source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                enabled: isRM.value === "RM"
                                onEnabledChanged: {
                                    if (!enabled) {
                                        currentIndex = -1
                                    }
                                }

                            }

                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsColumnSplit"
                                name: "ColumnPValue"
                                label: qsTr("Column")
                                source: [ { name: "columnsvariableSplitPlotBuilder", use: "levels" } ]
                            }
                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsColumnSplit" && isRM.value === "RM"
                                name: "RMColumnPValue"
                                label: qsTr("Column")
                                source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                enabled: isRM.value === "RM"
                                onEnabledChanged: {
                                    if (!enabled) {
                                        currentIndex = -1
                                    }
                                }
                            }

                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsRowSplit"
                                name: "RowPValue"
                                label: qsTr("Row")
                                source: [ { name: "rowsvariableSplitPlotBuilder", use: "levels" } ]
                            }
                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsRowSplit" && isRM.value === "RM"
                                name: "RMRowPValue"
                                label: qsTr("Row")
                                source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                enabled: isRM.value === "RM"
                                onEnabledChanged: {
                                    if (!enabled) {
                                        currentIndex = -1
                                    }
                                }

                            }

                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsGrid"
                                name: "GridPValue"
                                label: qsTr("Grid")
                                source: [ { name: "gridVariablePlotBuilder", use: "levels" } ]
                            }
                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsGrid" && isRM.value === "RM"
                                name: "RMGridPValue"
                                label: qsTr("Grid")
                                source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                enabled: isRM.value === "RM"
                                onEnabledChanged: {
                                    if (!enabled) {
                                        currentIndex = -1
                                    }
                                }
                            }
                        }

                        }

                    }
                }
            }

            Section{
            title: qsTr("Custom comparison lines")

            Label {
                text: qsTr("Required: X AND Y-Axis Variables")
                wrapMode: Text.Wrap
                color: "black"
            }

            Label {
                text: qsTr("Note: If custom Y-Axis limits are set, the starting" + "\n" + "position for the Y-Axis starnd and end values must fall within the defined interval")
                wrapMode: Text.Wrap
                color: "black"
            }

            ComponentsList {
                name: "annotationLineList"
                id:annotationLineList
                title: qsTr("Add annotation lines")
                addItemManually: true
                minimumItems: 0
                enabled: (
                             // 1. If not repeated measures (noRM) and both X and Y variables are assigned
                             (noRM.checked && variableXPlotBuilder.count > 0 && variableYPlotBuilder.count > 0)

                             ||

                             // 2. If repeated measures (yesRM) and at least one of the following is true:
                             (yesRM.checked && variableRepeatedMeasures.count > 0 && (
                                  // a) X variable is assigned and either drop-down defines Y
                                  (variableXPlotBuilder.count > 0 &&
                                   (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))

                                  ||

                                  // b) Y variable is assigned and either drop-down defines X
                                  (variableYPlotBuilder.count > 0 &&
                                   (rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX"))

                                  ||

                                  // c) Drop-downs define both X and Y roles separately
                                  ((rmValueOptionsDropDown.value === "rmValueAsX" || rmFactorOptionsDropDown.value === "rmFactorAsX") &&
                                   (rmValueOptionsDropDown.value === "rmValueAsY" || rmFactorOptionsDropDown.value === "rmFactorAsY"))
                                  ))
                             )

                onEnabledChanged: {
                    if (!enabled) {
                               // remove all components one by one
                               for (var i = annotationLineList.count - 1; i >= 0; --i) {
                                   annotationLineList.removeItem(i)
                               }
                           }
                }
                rowComponent: Row {


                    Group {
                        columns: 5
                        title: qsTr("Line ") + (rowIndex + 1)

                        Group{
                            title: qsTr("Label")
                            TextField {
                                name: "textAnnotationline"
                                label: qsTr("Label")
                                fieldWidth: 60
                            }
                        }


                        Group {
                        title: qsTr("Position")
                        columns: 4

                        Group {
                            DoubleField {
                                name: "xAnnotation"
                                label: qsTr("X-Axis start")
                            }
                            DoubleField {
                                name: "xendAnnotation"
                                label: qsTr("X-Axis end")
                            }
                        }

                        Group {
                            DoubleField {
                                name: "yAnnotation"
                                label: qsTr("Y-Axis start")
                            }
                            DoubleField {
                                name: "yendAnnotation"
                                label: qsTr("Y-Axis end")
                            }
                        }

                        Group{

                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsColumnSplit"
                                name: "ColumnAnnotationCompLine"
                                label: qsTr("Column")
                                source: [ { name: "columnsvariableSplitPlotBuilder", use: "levels" } ]
                            }
                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsColumnSplit" && isRM.value === "RM"
                                name: "RMColumnCompLine"
                                label: qsTr("Column")
                                source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                enabled: isRM.value === "RM"
                                onEnabledChanged: {
                                    if (!enabled) {
                                        currentIndex = -1
                                    }
                                }
                            }


                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsRowSplit"
                                name: "RowAnnotationCompLine"
                                label: qsTr("Row")
                                source: [ { name: "rowsvariableSplitPlotBuilder", use: "levels" } ]
                            }
                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsRowSplit" && isRM.value === "RM"
                                name: "RMRowCompLine"
                                label: qsTr("Row")
                                source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                enabled: isRM.value === "RM"
                                onEnabledChanged: {
                                    if (!enabled) {
                                        currentIndex = -1;
                                    }
                                }
                            }

                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue !== "rmFactorAsGrid"
                                name: "GridAnnotationCompLine"
                                label: qsTr("Grid")
                                source: [ { name: "gridVariablePlotBuilder", use: "levels" } ]
                            }
                            DropDown {
                                visible: rmFactorOptionsDropDown.currentValue === "rmFactorAsGrid" && isRM.value === "RM"
                                name: "RMGridCompLine"
                                label: qsTr("Grid")
                                source: [ { name: "variableRepeatedMeasures", use: "list" } ]
                                enabled: isRM.value === "RM"
                                onEnabledChanged: {
                                    if (!enabled) {
                                        currentIndex = -1
                                    }
                                }
                            }

                        }

                        }

                        Group{
                            title: qsTr("Appearance")

                            TextField {
                                name: "colorAnnotationLine"
                                label: qsTr("Line color")
                                placeholderText: qsTr("e.g. black, #ff5733")
                                defaultValue: "black"
                                fieldWidth: 60
                            }

                            DoubleField {
                                name: "textSizeAnnotationLine"
                                label: qsTr("Label size")
                                defaultValue: 5.5
                                fieldWidth: 60
                            }

                            DoubleField {
                                name: "textDistanceAnnotationLine"
                                label: qsTr("Label distance")
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

        Group {
            title: qsTr("Arrange plots by column")

            // Relative width of the columns
            TextField {
                name: "columnWidthInput"
                label: qsTr("Relative column widths")
                placeholderText: qsTr("1,1")
            }

            // Row Specifications ComponentsList
            ComponentsList {
                id: rowSpecifications
                name: "rowSpecifications"
                title: qsTr("Specify layout")
                addItemManually: true
                newItemName: qsTr("Column 1")
                Layout.preferredWidth: form.width - 2 * jaspTheme.generalAnchorMargin

                rowComponent: Row {
                    Group {
                        title: qsTr("Column ") + (rowIndex + 1)

                        TextField {
                            name: "plotIDs"
                            label: qsTr("Plot IDs")
                            placeholderText: qsTr("Plot 1, Plot 2, ...")
                        }

                        TextField {
                            name: "rowHeightsColumn"
                            label: qsTr("Plot heights")
                            info: qsTr("Specify the relative heights of the plots in the specified column.")
                            placeholderText: qsTr("1,1")
                        }
                    }

                    Group {
                        title: " "

                        TextField {
                            name: "labelsColumn"
                            label: qsTr("Labels")
                            placeholderText: qsTr("A, B, C, ...")
                            fieldWidth: 150
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
            title: qsTr("Arrange plots by row")

            // Relative height within row layout
            TextField {
                name: "relHeightWithinRowLayout"
                label: qsTr("Relative row heights")
                placeholderText: "1,1"
            }

            // Full Row Specifications ComponentsList
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
                            placeholderText: qsTr("Plot 1, Plot 2, ...")
                        }

                        TextField {
                            name: "relWidthsFullRow"
                            label: qsTr("Plot widths")
                            info: qsTr("Specify the relative widths of the plots in the specified row.")
                            placeholderText: "1,1"
                        }
                    }

                    Group {
                        title: " "

                        TextField {
                            name: "labelsFullRow"
                            label: qsTr("Labels")
                            placeholderText: qsTr("A, B, C, ...")
                            fieldWidth: 150
                        }

                        CheckBox {
                            name: "getCommonLegendRows"
                            label: qsTr("Collect legend")
                        }
                    }
                }
            }
        }


        Label {
            text: qsTr("Note: If you have column and row arrangement," + "\n" + "the column will be the top part of the layout" + "\n" + "and the row will be the bottom part of the layout.")
            wrapMode: Text.Wrap
            color: "black"
        }



        Group {
            columns: 3
            title: qsTr("Label settings")

            DoubleField {
                name: "labelSize"
                label: qsTr("Label size")
                value: 18
            }

            DoubleField {
                name: "labelDistance1"
                label: qsTr("Horizontal position")
                value: 0.05
                min: 0
                max: 1
            }


            DoubleField {
                name: "labelDistance2"
                label: qsTr("Vertical position")
                value: 0.95
                min: 0
                max: 1
            }
        }

        Group {
            title: qsTr("Additional settings")

            Row {
                spacing: 20

                Group {
                    TextField {
                        name: "relativeHeight"
                        label: qsTr("Column heights/row widths")
                        placeholderText: "1,1"
                        info: qsTr("Specify the relative heights of the columns or widths of the rows in the plot layout.")
                        fieldWidth: 150
                    }

                    DoubleField {
                        name: "layoutWidth"
                        label: qsTr("Width")
                        value: 500
                    }

                    DoubleField {
                        name: "layoutHeight"
                        label: qsTr("Height")
                        value: 500
                    }

                    DoubleField {
                        name: "plotSpacing"
                        label: qsTr("Spacing")
                        value: 10
                    }

                    CheckBox {
                        name: "getCommonLegend"
                        label: qsTr("Collect legend across layout")
                    }
                }
            }
        }
    }
}

// End Form
