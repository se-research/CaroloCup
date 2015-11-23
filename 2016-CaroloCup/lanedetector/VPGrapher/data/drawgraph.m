dataPath = 'angles-error/';
scenarioPaths = dir(dataPath);

scenarios = cell(1);
scenarioNames = cell(1);

% get data from csv files
j = 1;
for i = 1:length(scenarioPaths)
    scenarioName = scenarioPaths(i).name;
    
    if ~(strcmp(scenarioName, '.') || strcmp(scenarioName, '..') || strcmp(scenarioName, '.gitkeep'))
        scenarios{j} = csvread(strcat(dataPath, scenarioName));
        scenarioNames{j} = scenarioName(1:end-4);
        j = j + 1;
    end
end

% graph the data
scenariosCDF = cell(1);
for i = 1:length(scenarios)
    data = scenarios{i};
    mu = mean(data);
    sigma = std(data);
    y = normcdf(data, mu, sigma);
    scenariosCDF{i} = y;
    plot(data, y);
    hold on
end

% graph the minimum points line
maxAngles = zeros(length(scenarios), 1);
for i = 1:length(scenarios)
    maxAngles(i) = max(scenarios{i});
end
maxAngle = max(maxAngles);

minimumLineX = [];
minimumLineY = [];

k = 1;
for i = 0:maxAngle
    minimumYs = zeros(length(scenarios),1);
    
    for j = 1:length(scenarios)
        % wasteful to find min every time
        y = min(scenariosCDF{j}(scenarios{j} == i));
        if isempty(y)
            minimumYs(j) = inf;
        else
            minimumYs(j) = y;
        end
    end
    
    minimumY = min(minimumYs);
    if minimumY ~= inf
        minimumLineX(k) = i;
        minimumLineY(k) = minimumY;
        k = k + 1;
    end
end

plot(minimumLineX, minimumLineY, 'o');
a = area(minimumLineX, minimumLineY);
a.FaceColor = [207/255, 228/255, 228/255];
occupiedArea = trapz(minimumLineX, minimumLineY);

scenarioNames{length(scenarioNames) + 1} = 'min. line';
scenarioNames{length(scenarioNames) + 1} = 'min. area';
legend(scenarioNames);

ylim=get(gca,'ylim');
xlim=get(gca,'xlim');
areaText = text(xlim(2) - 1 , ylim(1) + 0.05, strcat('Area: ', num2str(round(occupiedArea)), '/', num2str(maxAngle)));
areaText.HorizontalAlignment = 'right';
