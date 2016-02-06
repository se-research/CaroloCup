__VPGrapher = {
    data: [],
    debugging: false,
    dispatch: null,
    secondaryScenariosLoaded: false,
    compare: {
        primary: {
            l: 0,
            m: 0,
            r: 0,
            max: 0
        },
        secondary: {
            l: 0,
            m: 0,
            r: 0,
            max: 0
        }
    },
    processScenarios: function (inputFolder) {
        __VPGrapher.data = []; // reset data

        d3.json("data/calculated/" + inputFolder + "/scenarios.json", function (scenarios) { // get all scenarios
            if (! scenarios) __VPGrapher.dispatch.noCSV();

            var scenariosLoaded = 0,
                totalScenarios = scenarios.length;

            scenarios.forEach(function (scenario) { // loop through each scenario
                var scenarioName = scenario.name,
                    groundTruthDataPath = "data/ground-truth/" + scenarioName + ".csv",
                    calculatedDataPath = "data/calculated/" + inputFolder + "/" + scenarioName + ".csv";

                d3.csv(groundTruthDataPath, function (groundTruthData) { // load ground truth scenario file
                    d3.csv(calculatedDataPath, function (calculatedData) { // load calculated data scenario file
                        if (calculatedData.length != groundTruthData.length) {
                            totalScenarios--;
                            return;
                        }

                        var totalFrames = calculatedData.length,
                            validFrames = totalFrames;

                        var errorAngles = [];
                        groundTruthData.forEach(function (groundTruth, index) {
                            var calculated = calculatedData[index];
                            var groundAngle = __VPGrapher.science.getAngle(groundTruth);
                            var calculatedAngle = __VPGrapher.science.getAngle(calculated);

                            var errorAngle = Math.abs(groundAngle - calculatedAngle);

                            if (errorAngle > 45) { // ignore angles bigger than the wheel can handle
                                validFrames--;
                                return;
                            }

                            errorAngles.push(errorAngle);
                        });

                        __VPGrapher.data.push({
                            name: scenarioName,
                            data: errorAngles,
                            framesValid: Math.round(validFrames / totalFrames * 100),
                            fps: scenario.fps
                        });

                        __VPGrapher.dispatch.scenarioLoaded();
                    });
                });
            });

            __VPGrapher.dispatch.on("scenarioLoaded", function () {
                scenariosLoaded++;

                if (scenariosLoaded == totalScenarios) __VPGrapher.dispatch.dataLoaded();
            });

            __VPGrapher.dispatch.on("dataLoaded", function () {
                __VPGrapher.draw.normalDistributionCDF(inputFolder);

                __VPGrapher.dispatch.scenarioProcessed();

                if (__VPGrapher.debugging) __VPGrapher.printScenariosAnglesErrorToCSV();
            });
        });
    },
    init: function() {
        this.dispatch = d3.dispatch("dataLoaded", "scenarioLoaded", "scenarioProcessed", 'noCSV');

        this.processScenarios("secondary");

        this.dispatch.on("noCSV", processPrimary);
        this.dispatch.on("scenarioProcessed", processPrimary);

        function processPrimary() {
            // run once
            if (! __VPGrapher.secondaryScenariosLoaded) {
                __VPGrapher.secondaryScenariosLoaded = true;
                __VPGrapher.processScenarios("primary");
            } else {
                __VPGrapher.draw.comparison();
            }
        }
    },
    draw: {
        normalDistributionCDF: function(inputFolder) {
            var margin = {
                    top: 20,
                    right: 20,
                    bottom: 30,
                    left: 50
                },
                width = 960 - margin.left - margin.right,
                height = 600 - margin.top - margin.bottom;

            var maxErrorAngles = [];
            __VPGrapher.data.forEach(function(scenario) {
                maxErrorAngles.push(d3.max(scenario.data));
            });
            var maxErrorAngle = d3.max(maxErrorAngles);
            var x = d3.scale.linear()
                .range([0, width])
                .domain([0, maxErrorAngle]);

            var y = d3.scale.linear()
                .range([height, 0])
                .domain([0, 1]);

            var xAxis = d3.svg.axis()
                .scale(x)
                .ticks(5)
                .orient("bottom");

            var yAxis = d3.svg.axis()
                .scale(y)
                .orient("left")
                .tickFormat(function(d) { return parseInt(d * 100, 10) + "%" });

            var svg = d3.select("#" + inputFolder).append("svg")
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

            var dataCDF = [];

            __VPGrapher.data.forEach(function(scenario) {
                var data = clone(scenario.data, false),
                    mean = d3.mean(data),
                    deviation = d3.deviation(data);

                data.forEach(function(value, i) {
                    data[i] = {x: value, y: __VPGrapher.science.gaussian.cdf(value, mean, deviation)};
                });

                data.sort(function(a, b) {
                    return a.x - b.x;
                });

                dataCDF.push(data);
            });

            // draw perfect line area
            var perfectLine = [{x: 0, y: 0.98}];
            for (var i = 1; i <= maxErrorAngle; i++) {
                perfectLine.push({x: i, y: 1});
            }

            var perfectArea = d3.svg.area()
                .x(function(d) { return x(d.x); })
                .y0(height)
                .y1(function(d) { return y(d.y); });

            svg.append("path")
                .datum(perfectLine)
                .attr("fill", "#EB7282")
                .attr("d", perfectArea);

            // draw maximum line area
            var maximumLine = __VPGrapher.draw.minMaxLineCDF(dataCDF, svg, x, y, maxErrorAngle, false);

            var maxArea = d3.svg.area()
                .x(function(d) { return x(d.x); })
                .y0(height)
                .y1(function(d) { return y(d.y); });

            svg.append("path")
                .datum(maximumLine)
                .attr("fill", "#FFF098")
                .attr("d", maxArea);

            // draw minimum line area
            var minimumLine = __VPGrapher.draw.minMaxLineCDF(dataCDF, svg, x, y, maxErrorAngle, true);

            var minArea = d3.svg.area()
                .x(function(d) { return x(d.x); })
                .y0(height)
                .y1(function(d) { return y(d.y); });

            svg.append("path")
                .datum(minimumLine)
                .attr("fill", "#7FBF90")
                .attr("d", minArea);

            // append error angle text
            svg.append("text")
                .attr("class", "x label")
                .attr("text-anchor", "end")
                .attr("x", width - 6)
                .attr("y", height - 6)
                .text("error angle");

            svg.append("text")
                .attr("class", "y label")
                .attr("text-anchor", "end")
                .attr("y", 9)
                .attr("x", -15)
                .attr("dy", ".75em")
                .attr("transform", "rotate(-90)")
                .text("frames");

            dataCDF.forEach(function(data, i) {
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
                    .attr("stroke", __VPGrapher.colours(i))
                    .attr("d", line);

                svg.append("text")
                    .attr("x", width - 36)
                    .attr("y", height - 37 - (30 * i))
                    .attr("text-anchor", "end")
                    .text(
                        __VPGrapher.data[i].name
                        + " (" + __VPGrapher.data[i].framesValid + "% v.f.)"
                        + " (" + Math.round(__VPGrapher.data[i].fps) + " fps)"
                    );

                svg.append("rect")
                    .attr("x", width - 26)
                    .attr("y", height - 50 - (30 * i))
                    .attr("width", 20)
                    .attr("height", 20)
                    .attr("stroke", "black")
                    .attr("stroke-width", "0.5px")
                    .attr("fill", __VPGrapher.colours(i));
            });

            // draw perfect line
            var line = d3.svg.line()
                .x(function(d) {
                    return x(d.x);
                })
                .y(function(d) {
                    return y(d.y);
                });

            svg.append("path")
                .datum(perfectLine)
                .attr("class", "line")
                .attr("stroke", '#000')
                .attr("d", line);

            // draw maximum line circles
            maximumLine.forEach(function(point) {
                svg.append("circle")
                    .datum(point)
                    .attr("r", 2.5)
                    .attr("cx", function(d) { return x(d.x); })
                    .attr("cy", function(d) { return y(d.y); });
            });

            // draw minimum line circles
            minimumLine.forEach(function(point) {
                svg.append("circle")
                    .datum(point)
                    .attr("r", 2.5)
                    .attr("cx", function(d) { return x(d.x); })
                    .attr("cy", function(d) { return y(d.y); });
            });

            // append the left area number
            var leftAreaNum = maxErrorAngle - __VPGrapher.science.trapz(maximumLine);
            svg.append("text")
                .attr("x", width - 6)
                .attr("y", 20)
                .attr("text-anchor", "end")
                .text("Left Area: " + Math.round(leftAreaNum / maxErrorAngle * 100) + "%");

            // append the middle area number
            var middleAreaNum = __VPGrapher.science.trapz(maximumLine) - __VPGrapher.science.trapz(minimumLine);
            svg.append("text")
                .attr("x", width - 6)
                .attr("y", 40)
                .attr("text-anchor", "end")
                .text("Middle Area: " + Math.round(middleAreaNum / maxErrorAngle * 100) + "%");

            // append the right area number
            var rightAreaNum = __VPGrapher.science.trapz(minimumLine);
            svg.append("text")
                .attr("x", width - 6)
                .attr("y", 60)
                .attr("text-anchor", "end")
                .text("Right Area: " + Math.round(rightAreaNum / maxErrorAngle * 100) + "%");

            // save areas
            __VPGrapher.compare[inputFolder].l = maxErrorAngle - __VPGrapher.science.trapz(maximumLine);
            __VPGrapher.compare[inputFolder].m = __VPGrapher.science.trapz(maximumLine) - __VPGrapher.science.trapz(minimumLine);
            __VPGrapher.compare[inputFolder].r = __VPGrapher.science.trapz(minimumLine);
            __VPGrapher.compare[inputFolder].max = maxErrorAngle;
        },
        minMaxLineCDF: function(data, svg, x, y, maxErrorAngle, isMinimumLine) {
            var line = [];
            for(var i = 0; i <= maxErrorAngle; i++) {
                var Ys = [];

                data.forEach(function (scenario) {
                    // find x with distance i
                    var x = scenario.map(function(d) {return d.x}).indexOf(i);

                    if (x == -1) {
                        Ys.push(undefined);
                    } else {
                        Ys.push(scenario[x].y);
                    }
                });

                var Y = true;
                if (isMinimumLine) {
                    Y = d3.min(Ys);
                } else {
                    Y = d3.max(Ys);
                }

                if (Y != undefined) {
                    line.push({x: i, y: Y});
                }
            }

            return line;
        },
        comparison: function() {
            var primary = __VPGrapher.compare.primary,
                secondary = __VPGrapher.compare.secondary;

            var leftAreaPrimary = Math.round((primary.max - primary.l) * 100 / primary.max),
                middleAreaPrimary = Math.round((primary.max - primary.m) * 100 / primary.max),
                rightAreaPrimary = Math.round(primary.r * 100 / primary.max);

            var leftAreaSecondary = Math.round((secondary.max - secondary.l) * 100 / secondary.max),
                middleAreaSecondary = Math.round((secondary.max - secondary.m) * 100 / secondary.max),
                rightAreaSecondary = Math.round(secondary.r * 100 / secondary.max);

            var leftArea = leftAreaSecondary - leftAreaPrimary,
                middleArea = middleAreaSecondary - middleAreaPrimary,
                rightArea = rightAreaSecondary - rightAreaPrimary;

            if (leftArea > 0) leftArea = "+" + leftArea;
            if (middleArea > 0) middleArea = "+" + middleArea;
            if (rightArea > 0) rightArea = "+" + rightArea;

            var leftAreaContainer = document.createElement("span"),
                middleAreaContainer = document.createElement("span"),
                rightAreaContainer = document.createElement("span");

            leftAreaContainer.style.color = "#EB7282";
            middleAreaContainer.style.color = "#FFF098";
            rightAreaContainer.style.color = "#7FBF90";

            leftAreaContainer.innerHTML = leftArea  + "% / ";
            middleAreaContainer.innerHTML = middleArea + "% / ";
            rightAreaContainer.innerHTML = rightArea + "%";

            document.getElementById("comparison")
                .appendChild(leftAreaContainer)
                .appendChild(middleAreaContainer)
                .appendChild(rightAreaContainer);
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

            var swing = (vp["VP_x"] < B.x) ? -1 : 1;

            var AB = Math.sqrt(Math.pow(B.x-A.x,2)+ Math.pow(B.y-A.y,2));
            var BC = Math.sqrt(Math.pow(B.x-C.x,2)+ Math.pow(B.y-C.y,2));
            var AC = Math.sqrt(Math.pow(C.x-A.x,2)+ Math.pow(C.y-A.y,2));
            var result = Math.acos((BC*BC+AB*AB-AC*AC)/(2*BC*AB)) * (180/Math.PI);

            return Math.round(result) * swing;
        },
        /**
         * Returns area using trapezoidal rule
         * @param data
         * @returns {number}
         */
        trapz: function(data) {
            var sum = 0;
            for (var i = 1; i < data.length; i++) {
                sum += (data[i].x - data[i-1].x) * (data[i].y + data[i-1].y);
            }
            return Math.round(sum * 0.5);
        }
    },
    colours: d3.scale.category20(),
    printScenariosAnglesErrorToCSV: function() {
        __VPGrapher.data.forEach(function(scenario) {
            scenario = clone(scenario);
            scenario.data.sort(function(a, b) {
                return a - b;
            });

            nanoajax.ajax({
                url: "print-csv.php",
                method: "POST",
                body: "scenario=" + JSON.stringify(scenario)
            }, function() {
                console.log(scenario.name + " done printing");
            });
        });
    }
};
