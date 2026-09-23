# NETNCoastalBirds

An R package for importing, summarizing, visualizing, and analyzing data from the **National Park Service Northeast Temperate Network (NETN) Coastal Breeding Bird Monitoring Program**.

The package provides tools for working with monitoring data collected through nest, incubation, creche, and American Oystercatcher surveys. Functions are provided for importing raw monitoring data, incorporating survey effort, producing standardized summaries, and generating time-series plots for analysis and reporting.

[NETN Coastal Birds Monitoring Program](https://www.nps.gov/im/netn/coastal-birds.htm)

---

## Overview

`NETNCoastalBirds` was developed to support reproducible analysis and reporting of data from the NETN Coastal Breeding Bird Monitoring Program.

The package provides a workflow for:

* importing monitoring data from the NETN Coastal Bird Access database or exported database tables;
* retrieving survey-effort information;
* standardizing raw survey data;
* summarizing observations by date, year, island, segment, species, and life stage;
* calculating effort-adjusted observations;
* examining survey history and sampling effort;
* producing data tables suitable for reporting; and
* generating standardized plots of coastal bird monitoring data.

The package currently includes workflows for American Oystercatcher, Common Eider, Common Tern, Double-crested Cormorant, and gull monitoring, as well as nest surveys for several additional coastal bird species.

---

## Installation

The development version can be installed from GitHub using `remotes`:

```r
# Install remotes if necessary
install.packages("remotes")

# Install NETNCoastalBirds
remotes::install_github("aaronweed/NETNCoastalBirds")
```

Load the package with:

```r
library(NETNCoastalBirds)
```

---

# Data workflow

There are two primary ways to work with the monitoring data.

### Option 1: Use the data bundled with the package

The package contains saved versions of the monitoring datasets. These are convenient for analysis and examples but may not represent the most recent data in the NETN Coastal Bird database.

For example:

```r
nest_data <- GetNestData(connect = "No")

incubation_data <- GetIncubationData(connect = "No")

creche_data <- GetCrecheData(connect = "No")

amoy_data <- GetAMOYData(connect = "No")
```

### Option 2: Retrieve current data from the database

Users with access to the NETN Coastal Bird Access database can retrieve current data using the Windows ODBC connection named `NETNCB`.

For example:

```r
nest_data <- GetNestData()

incubation_data <- GetIncubationData()

creche_data <- GetCrecheData()

amoy_data <- GetAMOYData()
```

The data-retrieval functions use the underlying Access database to construct standardized data frames for subsequent analysis.

> **Note:** The current package data were last validated on **August 20, 2025**. The bundled data should therefore not be assumed to represent the current state of the monitoring database.

---

# Importing exported database data

The package also supports analysis using tables exported from the NETN Coastal Bird database.

`importCBBData()` imports the exported database views and stores them in the `CBB_TABLES` environment by default.

For example:

```r
importCBBData(
  path = "C:/Coastal_Birds/exports/NETN"
)
```

A ZIP archive containing the exported tables can also be imported:

```r
importCBBData(
  path = "C:/Coastal_Birds/exports/NETN",
  zip_name = "CBB_Dataset_Export_20230119.zip"
)
```

The imported database views include:

* Events
* Incubation surveys
* Nest surveys
* Creche surveys
* American Oystercatcher surveys
* American Oystercatcher/Piping Plover summaries
* Tern summaries
* Surveillance surveys
* Incidental observations

The resulting tables can then be supplied directly to the summary functions.

For example:

```r
SumNestSurveys(
  df = CBB_TABLES$qry_Dataset_3_Survey_Nest,
  time = "year"
)
```

---

# Survey effort

Many monitoring summaries can be expressed as effort-adjusted observations. The package includes `GetSurveyData()` for retrieving survey effort by species, island, segment, and survey type.

For example:

```r
GetSurveyData(
  species = "DCCO",
  survey = "Incubation"
)
```

Survey effort includes the area or distance surveyed and the corresponding units.

The package also includes the `SurveyEffortBySpecies` dataset containing standardized survey-effort information.

---

# Summarizing monitoring data

## Nest surveys

`SumNestSurveys()` summarizes ground-based nest surveys by date or year and can return raw and effort-adjusted counts.

For example:

```r
nest_summary <- SumNestSurveys(
  time = "year",
  species = "COEI",
  output = "graph"
)
```

The function can summarize:

* nests;
* chicks;
* eggs;
* chicks per nest;
* eggs per nest; and
* clutch-related measures.

Annual summaries account for repeated surveys, including the use of the maximum nest count for species and sites where multiple surveys occur within a year.

---

## Incubation surveys

`SumIncubation()` summarizes boat-based incubation surveys, particularly for Double-crested Cormorants and gulls.

```r
incubation_summary <- SumIncubation(
  time = "year",
  species = "DCCO",
  output = "graph"
)
```

Annual summaries can include statistics such as:

* sum;
* mean;
* maximum; and
* minimum.

The function can also return observations summarized by observer for date-level analyses.

---

## Common Eider creche surveys

`SumCreche()` summarizes Common Eider (`COEI`) creche surveys.

```r
creche_summary <- SumCreche(
  time = "year",
  output = "graph"
)
```

The resulting summaries include measures such as:

* adult females tending ducklings;
* ducklings;
* total female Common Eiders observed; and
* average creche size.

The function can also calculate effort-adjusted observations based on survey distance.

For observer-level summaries:

```r
creche_observer <- SumCreche(
  time = "date",
  ByObserver = "yes"
)
```

---

## Common Tern surveys

`SumCOTE()` summarizes Common Tern (`COTE`) incubation observations.

```r
cote_summary <- SumCOTE(
  time = "year",
  output = "graph"
)
```

The current implementation summarizes observations from the Spinnaker platform.

---

## Gull and Double-crested Cormorant surveys

`SumGulls_DCCO()` provides summaries of gull and Double-crested Cormorant observations from incubation and nest surveys.

For example:

```r
dcco <- SumGulls_DCCO(
  time = "year",
  species = "DCCO",
  output = "graph"
)
```

Species can include:

```r
"DCCO"
"GBBG"
"HERG"
```

---

## American Oystercatcher

`GetAMOYData()` retrieves raw American Oystercatcher survey observations.

```r
amoy <- GetAMOYData()
```

The package also includes `AMOY_MatingPairSumm()`, which returns the end-of-season American Oystercatcher mating-pair summary.

```r
amoy_pairs <- AMOY_MatingPairSumm()
```

---

# Visualizing monitoring data

`PlotBirds()` provides a common plotting interface for summarized monitoring data.

For example:

```r
PlotBirds(
  nest_summary,
  species = "COEI",
  var = "Nests"
)
```

The function can be used with output from:

* `SumNestSurveys()`
* `SumIncubation()`
* `SumCreche()`

It supports:

* raw or effort-adjusted counts;
* filtering by island;
* filtering by species;
* selecting a life-stage or other variable;
* annual statistics;
* log-scaled plots;
* faceting by island, species, or variable; and
* overlaying species, life stages, or islands.

For example:

```r
PlotBirds(
  dcco,
  species = "DCCO",
  var = "Incubating adults",
  stat = "mean"
)
```

---

# Examining survey history

`GetSurveyMat()` creates a survey-history matrix showing when particular surveys were conducted.

For example:

```r
GetSurveyMat(
  survey = "Nest",
  species = "COEI",
  year = 2009,
  time = "Date"
)
```

The resulting matrix can be used to examine the spatial and temporal coverage of monitoring.

---

# Bundled datasets

Several standardized datasets are included with the package and can be accessed directly after loading `NETNCoastalBirds`.

Examples include:

| Dataset                 | Description                                                    |
| ----------------------- | -------------------------------------------------------------- |
| `SurveyEffortBySpecies` | Survey effort by species, island, segment, and survey type     |
| `CrecheByObserver`      | Common Eider creche observations summarized by observer        |
| `CrecheSurveysByDate`   | Common Eider creche observations summarized by island and date |
| `IncubationByObserver`  | Incubation observations summarized by observer                 |
| `IncubationByYear`      | Incubation observations summarized by island and year          |
| `NestSurveysByDate`     | Nest-survey observations summarized by island and date         |
| `NestSurveysByYear`     | Nest-survey observations summarized by island and year         |

These objects provide convenient starting points for analysis without requiring a connection to the monitoring database.

---

# Typical analysis workflow

A typical workflow using the package is:

### 1. Load the package

```r
library(NETNCoastalBirds)
```

### 2. Obtain the monitoring data

Either use the bundled data:

```r
nest_data <- GetNestData(connect = "No")
```

or retrieve current data from the database:

```r
nest_data <- GetNestData()
```

### 3. Summarize the observations

```r
nest_summary <- SumNestSurveys(
  df = nest_data,
  time = "year",
  species = "COEI",
  output = "graph"
)
```

### 4. Visualize the results

```r
PlotBirds(
  nest_summary,
  species = "COEI",
  var = "Nests"
)
```

The same general workflow can be used for incubation and creche monitoring:

```r
incubation <- GetIncubationData()

incubation_summary <- SumIncubation(
  df = incubation,
  time = "year",
  species = "DCCO",
  output = "graph"
)

PlotBirds(
  incubation_summary,
  species = "DCCO"
)
```

---

# Monitoring species

The current package contains functions supporting multiple coastal breeding bird monitoring components, including:

* American Oystercatcher (`AMOY`)
* Black-crowned Night-Heron (`BCNH`)
* Common Eider (`COEI`)
* Common Tern (`COTE`)
* Double-crested Cormorant (`DCCO`)
* Great Black-backed Gull (`GBBG`)
* Great Egret (`GREG`)
* Great Blue Heron (`GBBG`/related survey records)
* Herring Gull (`HERG`)
* Least Tern (`LETE`)
* Snowy Egret (`SNEG`)
* Spotted Sandpiper (`SPSA`)
* Willet (`WILL`)
* Glossy Ibis (`GLIB`)

Species availability varies among survey types.

---

# Function reference

The primary exported functions are:

### Data access

* `GetAMOYData()`
* `GetCrecheData()`
* `GetIncubationData()`
* `GetNestData()`
* `GetSurveyData()`
* `importCBBData()`

### Summaries

* `AMOY_MatingPairSumm()`
* `SumCOTE()`
* `SumCreche()`
* `SumGulls_DCCO()`
* `SumIncubation()`
* `SumNestSurveys()`

### Visualization and survey history

* `PlotBirds()`
* `GetSurveyMat()`

Function documentation is available from within R:

```r
?GetNestData
?SumNestSurveys
?SumIncubation
?SumCreche
?PlotBirds
?GetSurveyMat
```

---

# Data provenance

The monitoring data used by this package originate from the **NETN Coastal Breeding Bird Monitoring Program** and its associated database and monitoring protocols.

The package currently contains data last validated on:

**August 20, 2025**

Because the underlying monitoring database is periodically updated, users conducting analyses intended to represent the most current monitoring information should retrieve current data when database access is available.

The package also provides the ability to work from exported database tables when direct ODBC access is not available.

---

# Monitoring program and protocol

Additional information about the NETN Coastal Breeding Bird Monitoring Program, including monitoring protocols and program documentation, is available from the National Park Service:

[NETN Coastal Birds](https://www.nps.gov/im/netn/coastal-birds.htm)

The package documentation cites:

> Trocki, C. L., B. R. Mitchell, and P. W. C. Paton. 2015. *Coastal breeding bird monitoring protocol for Boston Harbor Islands National Recreation Area: 2015 revision.* Natural Resource Report NPS/NETN/NRR—2015/954. National Park Service, Fort Collins, Colorado.

---

# Development status

`NETNCoastalBirds` is under active development.

The current development version is **0.1.5**.

The package is being updated to improve:

* consistency among data-import functions;
* documentation;
* data-processing workflows;
* summary functions;
* visualization;
* testing; and
* reproducibility of monitoring reports.

Please report problems or suggestions using the [GitHub Issues](https://github.com/aaronweed/NETNCoastalBirds/issues) page.

---

# License

The current package metadata specifies **No license**. A formal open-source license should be added if the package is intended for broader redistribution or reuse.

---

# Contact

For questions about `NETNCoastalBirds`, please open an issue in the GitHub repository.

For information about the NETN Coastal Breeding Bird Monitoring Program, visit the [National Park Service NETN Coastal Birds](https://www.nps.gov/im/netn/coastal-birds.htm) webpage.
