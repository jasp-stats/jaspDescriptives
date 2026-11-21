// Run with: node tests/upgrades/plotBuilder-options.js
// Exercise the JavaScript used by the QML upgrade with both supported variable formats.
const fs = require("node:fs");
const path = require("node:path");
const assert = require("node:assert/strict");

const source = fs.readFileSync(path.join(__dirname, "../../inst/Upgrades.qml"), "utf8");
const start = source.indexOf("function(options)");
let depth = 0;
let end;
for (let i = source.indexOf("{", start); i < source.length; i++) {
    if (source[i] === "{") depth++;
    if (source[i] === "}" && --depth === 0) {
        end = i + 1;
        break;
    }
}
const upgrade = eval("(" + source.slice(start, end) + ")");
assert.match(source.slice(0, start), /fromVersion: "0\.98\.5"/);
assert.match(source.slice(0, start), /toVersion: "0\.98\.6"/);

const formats = [
    (name, type) => ({value: name, types: type}),
    (name, type) => ({value: name, types: [type]}),
    (name, type) => ({value: {name, types: [type], filterId: 1}}),
    (name, type) => ({value: [{name, types: [type], filterId: 1}]})
];
let cases = 0;
for (const xFormat of formats) for (const yFormat of formats) {
    for (const isRM of ["noRM", "RM"]) {
        for (const xType of ["scale", "ordinal", "nominal"]) {
            for (const yType of ["scale", "ordinal", "nominal"]) {
                const tab = {
                    isRM,
                    variableXPlotBuilder: xFormat("x", xType),
                    variableYPlotBuilder: yFormat("y", yType),
                    xVarRM: xFormat("time", xType), enableSort: true, enableSortY: true,
                    meanDotMode: "position", medianDotMode: "center",
                    addMeanBar: true, addMedianLine: true, addSumDot: true, addSEMErrorBar: true,
                    addMeanDot: true, addMedianDot: true, addMeanEllipse: false,
                    addCountDot: true, addStatEllipse: true
                };
                const before = structuredClone(tab);
                const result = upgrade({PlotBuilderTab: [tab]})[0];
                const positional = isRM === "RM" || xType !== "scale" || yType !== "scale";
                for (const name of ["addMeanBar", "addMedianLine", "addSumDot", "addSEMErrorBar"])
                    assert.equal(result[name], positional);
                for (const name of ["addMeanDot", "addMedianDot", "addCountDot", "addStatEllipse"])
                    assert.equal(result[name], true);
                assert.equal(result.addMeanEllipse, false);
                assert.equal(result.enableSort, xType !== "scale");
                assert.equal(result.enableSortY, isRM !== "RM" && yType !== "scale");
                assert.equal(result.meanDotMode, undefined);
                assert.equal(result.medianDotMode, undefined);
                assert.deepEqual(result.variableXPlotBuilder, before.variableXPlotBuilder);
                assert.deepEqual(result.variableYPlotBuilder, before.variableYPlotBuilder);
                assert.deepEqual(upgrade({PlotBuilderTab: [structuredClone(result)]})[0], result);
                cases++;
            }
        }
    }
}
for (const empty of ["", null, {value: "", types: []}, {value: []}]) {
    const result = upgrade({PlotBuilderTab: [{variableXPlotBuilder: empty,
        variableYPlotBuilder: {value: "y", types: "scale"}, addMeanBar: true}]})[0];
    assert.equal(result.addMeanBar, true);
    cases++;
}
console.log(`Plot Builder upgrade: ${cases} cases passed.`);
