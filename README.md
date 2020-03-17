# covid19
R scripts and markdown files for analyzing and reporting covid-19 data from Johns Hopkins University.

Following the guidelines listed here (https://www.tableau.com/about/blog/2020/3/ten-considerations-you-create-another-chart-about-covid-19), I'm only reporting descriptives and not publishing the reports.


Procedure:
1. Run Countries.Rmd: this calls 1_Build_GIT.r to build the dataset and write a local csv file.  It then calls ByCountry.R, a script that creates and writes individual plots to the Countries_Plots directory. The .rmd file writes Countries.Rmd.
2. Run States.Rmd: this follows the same procedure as in step 1.  It calls 1_Build_GIT.r and then calls ByState.R and ultimately writes States.pdf.