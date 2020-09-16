# ecoevosimulator

 [![Travis build status](https://travis-ci.org/sylvainschmitt/ecoevosimulator.svg?branch=master)](https://travis-ci.org/sylvainschmitt/ecoevosimulator)
 [![Codecov test coverage](https://codecov.io/gh/sylvainschmitt/ecoevosimulator/branch/master/graph/badge.svg)](https://codecov.io/gh/sylvainschmitt/ecoevosimulator?branch=master)

Simulating eco-evolutionnary dynamics of virtual populations with topography and forest gap dynamics.

## Installation

```
devtools::install_github("sylvainschmitt/ecoevosimulator")
```

## Test

```
library(ecoevosimulator)
plotSim(simulator())
```

## ToDo

* 2d dispersal kernel
* viability
* lognormal distribution
* non-overlapping generations (background and treefall mortality)
* topography generator
* forest gap dynamics generator
* explore hypotheses
