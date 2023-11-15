
# FiPhA

<!-- badges: start -->
<!-- badges: end -->

FiPhA (**Fi**ber **Ph**otometry **A**nalysis) is a platform for interactive dataset exploration, event visualization, and summary analysis of fiber photometry datasets in neurobehavior experiments.

Alongside a variety of data transformations, event series can be defined as sets of named intervals of interest relative to a given event signal, with filters that allow for their refinement and a variety of normalizations for standardizing individual responses. The resulting event datasets can be analyzed using the built-in statistical tools or exported for further processing in other applications.

Visualizations use the plotly library, have a number of configurable display options and can be saved at the click of a button. Entire sessions can be saved for later retrieval, and all datasets used during processing exist as data.table/data.frame objects in a central R object for future reference even without the application.

## Installation

FiPhA was developed using R v4.2.x and RStudio v2023.x.

You can install the latest development version directly from GitHub using the devtools package in RStudio with the following commands:

```r
# install devtools if not already (or `remotes`)
install.packages("devtools")

# download and install FiPhA package branch from github
devtools::install_github("mfbridge/FiPhA@pkg")
```

## Getting Started

Once installed, FiPhA can be started by navigating to its entry within the RStudio Addins menu. Alternatively, it can be started directly from the console by entering ``r FiPhA::FiPhA()``.

## Updating

Since this project is under active development, users may update it to the latest development version by selecting the appropriate entry in the RStudio Addins menu. This operation is just a shortcut to e-installing without upgrading any dependency packages.

## User Guide

See the [user guide](https://github.com/mfbridge/FiPhA/blob/pkg/docs/articles/user-guide.html) for more details and examples.

## Sample Data

A small sample of fiber photometry datasets may be loaded through the import menu.

## Acknowledgements

This work was supported by the National Institute of Environmental Health Sciences under contract GS-00F-173CA-75N96021F00109 to Social and Scientific Systems, a DLH Holdings Corp. Company.

## License

See [here](https://github.com/mfbridge/FiPhA/blob/pkg/LICENSE).
