<?php

$scenario = json_decode($_POST['scenario']);

if (! $scenario) return;


$filePath = "data/angles-error/" . $scenario->name . ".csv";

if (file_exists($filePath)) unlink($filePath);

foreach ($scenario->data as $errorAngle) {
    file_put_contents($filePath, $errorAngle . "\n", FILE_APPEND);
}
