# MT5751BowheadWhales
MT5751 Group Assignment 1: Transect Distance Sampling for Bowhead Whale Data

Using data from Rekdal et al. (2015), we conducted simple distance sampling analysis on three different alternatives of the processed dataset--original, binned, and truncated. We considered repeating this another three times to stratify within the analysis of the detection function; but generated estimates were identical; so this was omitted.

The final report can be found in `Report.Rmd` and `Report.pdf`. `MT5751BowheadWhales.Rproj` contains the working environment to directly load up our workspace to your RStudio. `dfModels.RData` contain the final models to avoid re-running every time we knit the `.Rmd` report.

All contained analysis can be found in `.R` files:

* `Analysis.R` -- the original data analysis and processing
* `AnalysisBinned.R` -- re-doing the original analysis with (manually) binned data
* `AnalysisTruncated.R` -- re-doing original analysis with data further truncated
* `FinalAnalysis.R` -- combining the above three `.R` files into a simplified analysis for reporting 
* `Assignment 1 analysis.R` -- alternative approach to conducting original and binned analysis in tandem
