<?php

$scenario = json_decode($_POST['scenario']);

if (! $scenario) return;


$filePath = "data/angles-error/" . $scenario->name . ".csv";

file_put_contents($filePath, $scenario->name . "\n");

foreach ($scenario->data as $errorAngle) {
    file_put_contents($filePath, $errorAngle . "\n", FILE_APPEND);
}
