# shinyDigitise (development version)

Version 0.1.1, on the `development` branch. The stable version on `master` is
0.1.0. Check which one you have with `packageVersion("shinyDigitise")`.

## New features

* **Sample sizes you type are used.** For scatterplots and histograms, the
  sample size typed when adding a group is now the exported `n`. If you leave it
  blank, `n` is estimated from your clicks as before (points per group for
  scatterplots, total of the bar heights for histograms). On `master` the
  clicked estimate was always used, so overlapping or hidden points gave a wrong
  `n`.
* **Log axes.** Boxplots and scatterplots can have a log-transformed or
  log-stretched axis, in any base, stored in the same format as metaDigitise.
  For scatterplots this is the x or the y axis, not both.
* **Step checks.** Each step in the sidebar shows a tick or a warning, the app
  says what is still missing or wrong before a figure can be saved, and it goes
  straight to the step that needs fixing instead of back to step 1.
* The xy mean/error plot type is only offered when the installed metaDigitise
  can export it (the CRAN version, 1.0.2, cannot, and the whole export would
  fail).
* If one figure cannot be exported, it is skipped with a warning and the rest
  are still exported, instead of the whole export failing.
* The sidebar wraps on narrow screens instead of being cut off.

## Bug fixes

* Removing a group also removed every group whose name contained it (e.g.
  removing "males" also removed "females").
* Editing a value in the group table changed every group that happened to share
  that value, not just the one edited.
* Editing the group table after reloading a figure could corrupt the edited
  value.
* Figures digitised with `metaDigitise()` (rather than shinyDigitise) could not
  be viewed or edited, because they have no per-point sample size (issue #33).
* Figures could not be reopened after the image folder was moved or shared,
  because the absolute image path was used. The image in the current folder is
  now used.
* Files saved by older versions of shinyDigitise, which stored the image name
  without its extension, now open.
* Figures saved with no comment made the development version of metaDigitise
  fail on export.
* `shinyDigitise(dir = )` now checks that the folder exists, and no longer
  writes `dir` into your global environment.

## Other changes

* `emojifont` and `stringr` are no longer needed.
* The README's note on sample sizes rewritten to match the new behaviour.
