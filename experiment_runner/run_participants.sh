#!/bin/bash

$PWD/Participant1/experiment_runner_linux_64bits --player &
sleep 2
$PWD/Participant2/experiment_runner_linux_64bits --player &
sleep 2
$PWD/Participant3/experiment_runner_linux_64bits --player &