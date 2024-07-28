# PredictingAdoptionWebTool
This repository contains all of the R and STAN code for running modules one and two of the Scaling4Nature web tool. There is also example data for the forecast model for module 2. All other background data must be downloaded from this link:

Here are the instructions for running the code. This code relies on the following R packages, all available from CRAN: tidyverse, Rstan, Rstan, SF, Raster, Terra. There will also be dependencies. 

## Module 1
* Step one: Run the code from the PlotAOI.R script. This will plot the area of interest, including European Space Agency land cover classification estimates for both the specific area and the surrounding bounding box. This code is required to be run before the other code from Module 1.
* Step two: Run EITHER the SettlementsPlotsCalculations.R script or the IndividualsPlotsCalculations.R Script. The 'settlements' script calculates the total number of human settlements of a specified size within the project area and has optional functionality to filter the number of settlements within the area to only those within a specified buffer distance of a resource type of interest, e.g., mangrove, grassland, ocean, etc. The 'individuals' script does the same calculations for the total number of people, rather than settlements.
* Optional step three: Run the SettlementsBoundaryPlot.R script to plot the boundaries of all human settlements within the project area. This is not necessary and is candidate functionality for cutting from the tool.
* The end result of this module is a figure of the potential adopters in space and an R object named 'PotentialAdopters' that contains the total number of potential adopters in the population.

## Module 2
* The RunForecast.R script requires data to fit the predictive model. If no data are available, users can use provided example data which shows progress toward adoption of a fictional intervention for each of the 338 human settlements in the Kruger to Canyons Biosphere Reserve.
* This code is assumed to use the 'PotentialAdopters' object to specify the total potential adopters in the model. Users can also self-specify this value in the script if desired.
* This code will result in a figure showing the model fit to the observed and forecasted adoption.
* We will need to think more about how to create the standard datasheet for use in this script.
* NOTE: This code requires Rstan to be runable on the user's machine. STAN requires users to first configure their C++ toolchain. This is a bit of a pain, but only required to be done once. Here are instructions for how to do that: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

