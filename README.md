<p align="center">
  <img src="https://github.com/EIvimeyCook/shinyDigitise/blob/master/inst/shinyDigitise/www/img/shinyDigitise.png" width = "200"/>
</p>

<div align="center">
 <h1>shinyDigitise</h1>
</div>

<!-- badges: start -->
[![DOI](https://img.shields.io/badge/DOI-10.1002%2Fjrsm.1663-blue)](https://doi.org/10.1002/jrsm.1663)
[![License: MIT](https://img.shields.io/badge/license-MIT-green)](LICENSE.md)
<!-- badges: end -->

shinyDigitise builds upon the popular metaDigitise package and provides users
with an **interactive UI to help extraction of data from five different plot
types**. The main functions used in shinyDigitise are called from the
[metaDigitise package](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13118),
which allows users to extract descriptive statistics from various plot types.

A large share of the effect sizes in any meta-analysis are read off figures in
the primary literature, and that step is usually undocumented, unarchived, and
impossible to check. Importantly, both packages allow for **replotting and
checking of data extraction from graphs** — which all contribute to increased
reproducibility. The app walks you through calibration and point selection, keeps
the calibration data alongside the extracted values, and can redraw your
extraction over the original image so errors are visible rather than silent.

Now published in
[Research Synthesis Methods](https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1663).

## Features

- **Five plot types.** Mean/error, boxplot, xy mean/error, histogram, and
  scatterplot.
- **Guided, six-stage workflow.** The app walks you through each stage in order.
- **Live orientation and calibration.** Flip or rotate a figure with the rotation
  angle shown on screen, then set the axes by double-clicking.
- **Zoom and adjust on the fly.** Click and drag a box over any area to zoom, and
  adjust point size and group label positions as you work.
- **Replot and check.** Extractions can be redrawn over the source figure, so
  mistakes are caught rather than archived.
- **Deliberately manual.** Semi-automated point detection makes users complacent
  and introduces errors that are hard to spot; extraction here is your judgement,
  recorded so others can check it.
- **Output you can archive.** Results are returned as an object and saved as a
  `.csv` in the same folder as the images.

## Installation

```r
devtools::install_github("EIvimeyCook/ShinyDigitise")
```

**If you plan on using shinyDigitise to extract data from xy mean graphs, you
need the dev version of metaDigitise.**

```r
devtools::install_github("joelpick/metaDigitise")
```

There is a known bug with some versions of RStudio
(<https://github.com/rstudio/rstudio/issues/12649>). Be sure to update RStudio to
the latest version.

## Usage

Video tutorial [here](https://www.youtube.com/watch?v=b9KvRsO8SPY).

```r
library(shinyDigitise)
df <- shinyDigitise("folder where your images are located")
```

or — where you specify the folder through a menu system within SD:

```r
df <- shinyDigitise()
```

## Basic workflow

The app will walk you through each stage. If a directory is provided, you'll skip
straight to the file selection phase. If not, you'll be asked to select a file
from the image folder.

1. **Choose plot type** — mean/error, boxplot, xy mean/error, histogram, or
   scatterplot.
2. **Orientate figure** — flip or rotate the graph (rotation angle is shown on
   screen).
3. **Calibrate axes** — click calibrate mode on, and, depending on the plot type
   shown, double click on the axes in the relevant order and add the variable
   name and values. These will then appear on the plot (the size will depend on
   the point size slider).
4. **Add groups** — clicking add group will cause a pop up to appear to enter data
   (name and sample size). This will then appear in the table. Clicking the group
   will allow you to either double click points on the graph (you have to press
   the click points button first) or delete the group. Lastly, you can select the
   type of error shown (if a mean/error graph).
5. **Comments** — add a comment to your data.
6. **Finished!** The app will close when you've reached the end of your graphs.

*To view the extracted data just simply call the object you've created or view
the resulting `.csv` file (which will be saved into the same folder as the
images).*

> **Tip.** You can adjust point size and group name positions on the fly, as well
> as zoom in on the image by clicking and dragging a box over the desired area.

## Bug reports and contributions

Please file issues and feature requests at
<https://github.com/EIvimeyCook/shinyDigitise/issues>. Pull requests are welcome.

## Related tools

- [**metaDigitise**](https://github.com/daniel1noble/metaDigitise) — the
  underlying extraction engine
- [**metRscreen**](https://github.com/EIvimeyCook/metRscreen) — title and abstract
  screening
- [**DataExtraction**](https://github.com/EIvimeyCook/DataExtraction) — data and
  code for the accompanying paper

## Citation

> Ivimey-Cook, E. R., Noble, D. W. A., Nakagawa, S., Lajeunesse, M. J., & Pick,
> J. L. (2023). Advice for improving the reproducibility of data extraction in
> meta-analysis. *Research Synthesis Methods*, 14(6), 911–915.
> <https://doi.org/10.1002/jrsm.1663>

A machine-readable [`CITATION.cff`](CITATION.cff) is included, so GitHub's
"Cite this repository" button gives formatted APA and BibTeX.

## License

Released under the [MIT License](LICENSE.md).
