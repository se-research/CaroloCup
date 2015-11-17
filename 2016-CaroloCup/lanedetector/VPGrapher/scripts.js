__VPGrapher = {
    data: [],
    debugging: false,
    init: function() {
        d3.json("data/scenarios.json", function(scenarios) { // get all scenario names
            var dispatch = d3.dispatch("dataLoaded", "scenarioLoaded");
            var scenariosLoaded = 0,
                totalScenarios = scenarios.length;

            scenarios.forEach(function(scenario) { // loop through each scenario
                var groundTruthDataPath = "data/ground-truth/" + scenario + ".csv",
                    calculatedDataPath = "data/calculated/" + scenario + ".csv";

                d3.csv(groundTruthDataPath, function(groundTruthData) { // load ground truth scenario file
                    d3.csv(calculatedDataPath, function (calculatedData) { // load calculated data scenario file
                        if (calculatedData.length != groundTruthData.length) {
                            totalScenarios--;
                            return;
                        }

                        var errorAngles = [];
                        groundTruthData.forEach(function (groundTruth, index) {
                            var calculated = calculatedData[index];

                            var errorAngle =  Math.abs(__VPGrapher.science.getAngle(groundTruth)  - __VPGrapher.science.getAngle(calculated));

                            errorAngles.push(errorAngle);

                            if (__VPGrapher.debugging) __VPGrapher.debug.printErrorAngles(errorAngle);
                        });

                        __VPGrapher.data.push({name: scenario, data: errorAngles});

                        dispatch.scenarioLoaded();
                    });
                });
            });

            dispatch.on("scenarioLoaded", function() {
                scenariosLoaded++;

                if (scenariosLoaded == totalScenarios) dispatch.dataLoaded();
            });

            dispatch.on("dataLoaded", function() {
                __VPGrapher.draw.normalDistributionCDF();
            });
        });
    },
    draw: {
        normalDistributionCDF: function() {
            var margin = {
                    top: 20,
                    right: 20,
                    bottom: 30,
                    left: 50
                },
                width = 960 - margin.left - margin.right,
                height = 500 - margin.top - margin.bottom;

            var maxErrorAngles = [];
            __VPGrapher.data.forEach(function(scenario) {
                maxErrorAngles.push(d3.max(scenario.data));
            });

            var x = d3.scale.linear()
                .range([0, width])
                .domain([0, d3.max(maxErrorAngles)]);

            var y = d3.scale.linear()
                .range([height, 0])
                .domain([0, 1]);

            var xAxis = d3.svg.axis()
                .scale(x)
                .ticks(20)
                .orient("bottom");

            var yAxis = d3.svg.axis()
                .scale(y)
                .ticks(15)
                .orient("left")
                .tickFormat(function(d) { return parseInt(d * 100, 10) + "%" });

            var svg = d3.select("body").append("svg")
                .attr("width", width + margin.left + margin.right)
                .attr("height", height + margin.top + margin.bottom)
                .append("g")
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

            svg.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(0," + height + ")")
                .call(xAxis);

            svg.append("g")
                .attr("class", "y axis")
                .call(yAxis);

            svg.append("text")
                .attr("class", "x label")
                .attr("text-anchor", "end")
                .attr("x", width)
                .attr("y", height - 6)
                .text("distance");

            svg.append("text")
                .attr("class", "y label")
                .attr("text-anchor", "end")
                .attr("y", 6)
                .attr("dy", ".75em")
                .attr("transform", "rotate(-90)")
                .text("frames");

            __VPGrapher.data.forEach(function(scenario, index) {
                var data = scenario.data,
                    mean = d3.mean(data),
                    deviation = d3.deviation(data);

                data.forEach(function(value, index) {
                    data[index] = {x: value, y: __VPGrapher.science.gaussian.cdf(value, mean, deviation)};
                });

                data.sort(function(a, b) {
                    return a.x - b.x;
                });

                var line = d3.svg.line()
                    .x(function(d) {
                        return x(d.x);
                    })
                    .y(function(d) {
                        return y(d.y);
                    });

                svg.append("path")
                    .datum(data)
                    .attr("class", "line")
                    .attr("style", "stroke: " + __VPGrapher.colours(index))
                    .attr("d", line);

                svg.append("text")
                    .attr("x", width - 30)
                    .attr("y", height - 37 - (30 * index))
                    .attr("text-anchor", "end")
                    .text(scenario.name);

                svg.append("rect")
                    .attr("x", width - 20)
                    .attr("y", height - 50 - (30 * index))
                    .attr("width", 20)
                    .attr("height", 20)
                    .attr("stroke", "black")
                    .attr("stroke-width", "0.5px")
                    .attr("fill", __VPGrapher.colours(index));
            });
        }
    },
    science: {
        gaussian: {
            cdf: function (x, mean, sigma) {
                x = (x - mean) / sigma;
                return .5 * (1 + __VPGrapher.science.gaussian.helper.erf(x / Math.SQRT2));
            },
            helper: {
                erf: function (x) {
                    var a1 = 0.254829592,
                        a2 = -0.284496736,
                        a3 = 1.421413741,
                        a4 = -1.453152027,
                        a5 = 1.061405429,
                        p = 0.3275911;

                    // Save the sign of x
                    var sign = x < 0 ? -1 : 1;
                    if (x < 0) {
                        sign = -1;
                        x = -x;
                    }

                    // A&S formula 7.1.26
                    var t = 1 / (1 + p * x);
                    return sign * (
                        1 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1)
                        * t * Math.exp(-x * x));
                }
            }
        },
        /**
         * Get angle between car line and vanishing point
         * @returns {number}
         * @param vp
         */
        getAngle: function (vp) {
            var A = {x: vp["VP_x"], y: vp["VP_y"]},
                B = {x: 376, y: 300}, // center point
                C = {x: 376, y: 0};

            var AB = Math.sqrt(Math.pow(B.x-A.x,2)+ Math.pow(B.y-A.y,2));
            var BC = Math.sqrt(Math.pow(B.x-C.x,2)+ Math.pow(B.y-C.y,2));
            var AC = Math.sqrt(Math.pow(C.x-A.x,2)+ Math.pow(C.y-A.y,2));
            return Math.acos((BC*BC+AB*AB-AC*AC)/(2*BC*AB)) * (180/Math.PI);
        }
    },
    debug: {
        printErrorAngles: function(errorAngle) {
            var container = d3.select("#print");

            var html = container.html();
            html += errorAngle + "<br>";

            container.html(html);
        }
    },
    colours: d3.scale.category20()
};