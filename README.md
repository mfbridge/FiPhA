
# FiPhA

FiPhA (**Fi**ber **Ph**otometry **A**nalysis) is a platform for
interactive dataset exploration, event visualization, and summary
analysis of fiber photometry datasets in neurobehavior experiments.

Alongside a variety of data transformations, event series can be defined
as sets of named intervals of interest relative to a given event signal,
with filters that allow for their refinement and a variety of
normalizations for standardizing individual responses. The resulting
event datasets can be analyzed using the built-in statistical tools or
exported for further processing in other applications.

Visualizations use the plotly library, have a number of configurable
display options and can be saved at the click of a button. Entire
sessions can be saved for later retrieval, and all datasets used during
processing exist as data.table/data.frame objects in a central R object
for future reference even without the application.

## Recent updates

**June 2023** - A generalized framework that replaces the internal code
of the Shiny app with a more structured and standardized feature set
available to R without Shiny interactivity is currently a work in
progress and will be part of the next major application update along
with a more polished layout that utilizes updated libraries.

## Links

- [Features](#features)
  - [Import Formats](#import-formats)
  - [Transformations](#transformations)
  - [Events](#events)
- [Usage](#usage)
- [Examples](#examples)
- [References](#references)
- [Acknowledgements](#acknowledgements)

## Features

### Import Formats

FiPhA supports a variety of import formats; from the ubiquitous \*.xlsx
and the humble \*.csv to more specialized spectral-based recordings.

- **Comma-separated value files (\*.csv)**
- **Microsoft Excel workbook sheets (\*.xlsx)**
- **Raw tab-delimited spectrometer recordings (\*.txt)**
  - Summaries of user-defined wavelength ranges without reference
    spectra (e.g. average intensity of 500-550 nm light)
  - Linear unmixing with reference spectra, see \[ref\]
- **TDT data tanks (\*.tev & \*.tsq)**
  - High-frequency fiber photometry binary data stores used by TDT
    devices without any external dependencies.
  - Only a subset of the fiber photometry gizmo-specific features are
    supported. See \[ref\]

### Transformations

Prior to or independent of any event processing, a number of common
dataset operations are available to be performed as well as a few that
are more specific to investigating and processing fiber photometry time
series (e.g. photobleaching).

- **Dataset manipulations**
  - Subsetting based on time or observation number
  - Joining additional datasets by time (such as behavior data)
    - Alignment of different recording frequencies by nearest timestamp
      value
  - Concatenation of datasets
  - Ratio calculation
- **Signal processing**
  - Downsampling
  - Low-pass filtering
  - lag-autocorrelation
  - power density spectrum
  - spectrogram
  - peak detection
- **De-trending**
  - Photobleaching compensation
  - Removal of linear and exponential decay models
- **Custom R code**
  - A place for execution of R code snippets to perform arbitrary
    operations on the given dataset
    - Executed in an R environment with a pre-populated
      data.table/data.frame object

### Events

Events are defined using a simple structure that labels regions of an
event relative to a particular signal, such as that given by a binary
TTL signal. “Filters” are available to refine the listing of events, and
perform more complex operations like excluding events that do not meet
certain criteria, a custom R expression based on dataset variables,
aggregation of events that occur within a certain time of one another, a
rate-based inclusion criteria, etc. Finally, a normalization can be
applied to particular variables of interest, such as a standard z-score,
δF/F, or more robust z-score alternative (see \[ref\]).

- **Signals**
  - arbitrary events specified by start & end timestamps
  - a binary TTL-like signal where each continuous pulse is a single
    event
  - bins of a particular length
- **Intervals**
  - interval corresponding to the raw event signal
  - an interval that starts before another, relative to that interval’s
    start time (e.g. a baseline period before an event)
  - an interval and relative to another interval’s end time (e.g. a
    post-event period)
- **Filters**
  - keep or drop the first N events
  - keep or drop the last N events
  - shift all events by a fixed offset
  - filter out events that do not meet a minimum or maximum length
  - filter events based on timestamps
  - aggregate if overlapping and events occur within X seconds of each
    other
  - keep events based on the number of events within X seconds
  - keep events that meet some arbitrary R expression
- **Normalizations**
  - standard z-score relative to a baseline
  - a z-score-ish calculation that uses medians, see \[ref\]
  - delta-F / F0, or percent depature from baseline
  - percent change over time

## Usage

FiPhA requires a recent version of R & RStudio, and builds on top of
numerous R packages available on CRAN. However, only the `shiny` package
needs to be installed in order to start, as the application will install
any additional packages from CRAN when initializing.

Assuming the project has been opened in RStudio, starting the Shiny
interface can be done anywhere by executing `shiny::runApp()` in the
console.

Alternatively, navigating to global.R or ui.R in the RStudio editor will
present a Shiny-specific “Run App” button in the top right of the editor
pane.

TODO: An even more standalone version using the Electron framework may
be available on the
[releases](https://github.com/mfbridge/FiPhA/releases) page.

## Examples

TODO: Add some examples figures and screenshots

## References

TODO: Add references

## Acknowledgements

This work was supported by the National Institute of Environmental
Health Sciences under contract GS-00F-173CA-75N96021F00109 to Social and
Scientific Systems, a DLH Holdings Corp. Company.

## License

See [here](https://github.com/mfbridge/FiPhA/blob/main/LICENSE).
