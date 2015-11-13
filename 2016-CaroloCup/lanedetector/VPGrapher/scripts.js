__VPGrapher = {
    init: function() {
        this.handleCSVs();
    },
    handleCSVs: function() {
        var debugging = false;

        var scenarioPath = "data/ground-truth/fulltrack2.csv",
            calculatedDataPath = "data/calculated/fulltrack2.csv";

        d3.csv(scenarioPath, function(scenarioData) {
            d3.csv(calculatedDataPath, function (calculatedData) {
                var data = [];

                scenarioData.forEach(function (scenario, index) {
                  var calculated = (calculatedData[index]) ? calculatedData[index] : {VP_x: 0, VP_y: 0};

                  var distance = __VPGrapher.science.getDistanceBetweenTwoPoints(scenario, calculated);

                  if (distance > 1000) return; // remove garbage data

                  data.push(distance);

                  if (debugging) __VPGrapher.debug.printDistances(distance);
                });

                __VPGrapher.draw.normalDistributionCDF(data);
            });
        });
    },
    draw: {
        normalDistributionCDF: function(originalData) {
            var frames = originalData.length;

            var data = originalData.slice(), // copy array
                mean = d3.mean(data),
                deviation = d3.deviation(data);

            data.forEach(function(value, index) {
                data[index] = {x: value, y: __VPGrapher.science.gaussian.cdf(value, mean, deviation)};
            });

            data.sort(function(a, b) {
                return a.x - b.x;
            });

            // draw graph
            var margin = {
                    top: 20,
                    right: 20,
                    bottom: 30,
                    left: 50
                },
                width = 960 - margin.left - margin.right,
                height = 500 - margin.top - margin.bottom;

            var x = d3.scale.linear()
                .range([0, width])
                .domain([0, d3.max(data, function(d) { return d.x; })]);

            var y = d3.scale.linear()
                .range([height, 0])
                .domain([0, 1]);

            console.log(d3.max(data, function(d) { return d.y; }));

            var xAxis = d3.svg.axis()
                .scale(x)
                .ticks(20)
                .orient("bottom");

            var yAxis = d3.svg.axis()
                .scale(y)
                .ticks(15)
                .orient("left")
                .tickFormat(function(d) { return parseInt(d * 100, 10) + "%" });

            var line = d3.svg.line()
                .x(function(d) {
                    return x(d.x);
                })
                .y(function(d) {
                    return y(d.y);
                });

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

            svg.append("path")
                .datum(data)
                .attr("class", "line")
                .attr("d", line);

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
        }
    },
    science: {
        gaussian: {
            cdf: function(x, mean, sigma) {
                x = (x - mean) / sigma;
                return .5 * (1 + __VPGrapher.science.erf(x / Math.SQRT2));
            }
        },
        erf: function(x) {
            var a1 =  0.254829592,
                a2 = -0.284496736,
                a3 =  1.421413741,
                a4 = -1.453152027,
                a5 =  1.061405429,
                p  =  0.3275911;

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
        },
        getDistanceBetweenTwoPoints: function(point1, point2) {
            return Math.round(
                Math.sqrt(
                    Math.pow((point2['VP_x'] - point1['VP_x']), 2) +
                    Math.pow((point2['VP_y'] - point1['VP_y']), 2)
                )
            );
        }
    },
    debug: {
        printDistances: function(distance) {
            var container = d3.select("#print");

            var html = container.html();
            html += distance + "<br>";

            container.html(html);
        }
    }
};