# SpatioTemporal Prism

## Overview
Time geography proposes the concept of "anchors" - fixed activities in space and time - that structure people's daily schedules. Typically, home and work locations during certain hours are used as anchors. However, the increasing complexity of human mobility calls for reexamining assumptions that home and work always serve as anchors. 
This repository develops data-driven methods for extracting personalized anchors from multi-day GPS trajectory surveys. 

This repository includes visualization and analysis components tailored for paper publication. 
The associated paper has been officially published and is accessible at [(Zhang et al. 2022)](https://doi.org/10.1007/s11116-022-10352-2).

## Files 
The repository contains the following key files and scripts:

* `Step1-Read-Data-Preprocessing.R`: Reads in the raw GPS survey data and preprocesses it into an analysis-ready format
* `Step2-Spatial-Fixity.R`: Implements analysis to assess spatial fixity of activities, including classifying stationary vs non-stationary activities and clustering locations
* `Step3-Temporal-Fixity.R`:  Calculates metrics of temporal fixity - repetitive ratio across days for each activity
* `Step4-Spatio-Temporal-Fixity.R`: Combines spatial and temporal components to identify repetitive intervals of activities occurring at fixed locations over time
* `Step5-Anchor-Summary.R`: Summarizes the extracted anchors for each person and activity type based on the space-time fixity analysis
* `_Spatial_Operation_.R`: Defines functions for spatial operations like calculating trajectory metrics, e.g., activity space and distance to the mean center
* `_Temporal_Operation_.R`: Defines functions for temporal operations like repetitive ratio, interval detection, and anchor extraction
* `ST_Pattern_Pipeline.R`: Executes the pipeline of analyses by calling the required processing and analysis scripts in sequence
