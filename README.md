# Geo tools for R

![Cover](media/Cover.png)

## Authors
- **Alessandro FASSÒ**, University of Bergamo (alessandro.fasso@unibg.it).
- **Alessandro FUSTA MORO**, University of Bergamo (alessandro.fustamoro@unibg.it).
- **Lorenzo LEONI**, University of Bergamo (lorenzo.leoni1998@gmail.com).

## Description
The ```geotools``` package is an R library designed to simplify the management, analysis, and processing of space-time data. In particular, it provides a comprehensive set of modules that enable the computation of statistics for space-time variables (e.g., meteorological and air quality data) whose values are available on a spatial grid at the polygonal scale (e.g., municipal or regional).

## Installation
Install the ```geotools``` package via GitHub:
``` r
# install.packages("devtools")
devtools::install_github("GRINS-Spoke0-WP2/geotools")
library(geotools)
``` 

## Overview
The ```geotools``` R package is structured as follows:
| **Function** | **Description** | **Available** |
|:------------:|:----------------|:-------------:|
| ```geomatching``` | Maps input space-time data from different spatial grids onto a specific common grid, even when their geographic reference systems may differ. This procedure is known as **spatial overlay**. It can handle two input data formats: R dataframe or matrix | Yes |
| ```idw2hr``` | Maps input space-time data onto a high-resolution spatial grid using **IDW** (**Inverse Distance Weighting**). The goal is to assign (interpolated) observations within the boundaries of smaller municipalities (which are polygons) as well | Yes |
| ```hr2poly``` | Maps input high-resolution space-time data onto polygons and computes the user-specified statistics (e.g., mean, median, and standard deviation) for each space-time variable | Yes |
| ```df2poly``` | Executes consecutively ```geomatching```, ```idw2hr```, and ```hr2poly``` | Coming soon |

## Flowchart

```mermaid
flowchart TB

    %% input nodes
    input1a["**data.frame**
        (_lon_ _lat_ _time_)"]:::input
    input1b["**matrix**
        (_lon_ x _lat_ x _time_)"]:::input
    
    %% output node
    output3["**data.frame**
        (stats per polygon)"]:::output

    subgraph main_flow [**DF2POLY**]
        direction TB
        style main_flow opacity:.5

        %% input nodes
        input2b["**high resolution (HR)
            grid**"]:::input
        input3b["**polygons**
            (_.shp_)"]:::input

        %% function nodes
        function1("**GEOMATCHING**"):::function
        function2("**IDW2HR**"):::function
        function3("**HR2POLY**"):::function

        %% settings nodes
        settings1("format
            type
            crs"):::settings
        settings2("crs
            outgrid_params
            col_names
            interest_vars
            idp
            ncores
            "):::settings
        settings3("polygon_type
            col_names
            stats
            crs
            ncores
            keep_geometry
            "):::settings

        %% output nodes
        output1["**data.frame**
            (_lon_ _lat_ _time_)"]:::output
        output2["**HR data.frame**
            (_lon_ _lat_ _time_)"]:::output

        %% relations
        settings1 --> |"SETTINGS"| function1
        settings2 --> |"SETTINGS"| function2
        input2b -->|"INPUT"| function2
        output1 -->|"INPUT"| function2
        function2 -->|"OUTPUT"| output2
        settings3 --> |"SETTINGS"| function3
        input3b -->|"INPUT"| function3
        output2 -->|"INPUT"| function3
    end

    %% relations
    input1a -->|"INPUT"| function1
    input1b -->|"INPUT"| function1
    function1 -->|"OUTPUT"| output1
    function3 -->|"OUTPUT"| output3

    %% styles
    classDef input stroke:royalblue,stroke-width:2px;
    classDef function stroke:orangered,stroke-width:4px;
    classDef settings stroke:gold,stroke-width:2px;
    classDef output stroke:limegreen,stroke-width:2px;
```
