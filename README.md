# ROC-Curve-Manuscript

R code for the manuscript analysing structural MRI markers (gray matter, white matter, white matter hyperintensities, AD signature, and hippocampal occupancy) for distinguishing cognitive status groups.

## What the script does

`ROC_Analysis.R` runs the full analysis in order:

1. **Data preparation** – keeps each participant's most recent MRI, checks the MRI-to-diagnosis date gap, selects and renames variables.
2. **Descriptives** – summary statistics by cognitive status, boxplots (Figure 2), ANOVA with Tukey HSD.
3. **Group comparisons (Table 1)** – NL vs MCI/Dementia, MCI vs Dementia, NL vs Dementia: demographics, chi-square/Fisher tests, and linear models adjusted for TICV, age, sex, race, enrollment status, and education.
4. **Univariate ROC analyses** for each comparison – AUC with DeLong 95% CI, Youden-optimal cutoff, sensitivity/specificity/PPV/NPV, and 1,000-replicate stratified bootstrap internal validation (optimism correction and out-of-bag estimates).
5. **Biomarker summary table** across all diagnostic groups.

## Requirements

R packages: `ggplot2`, `dplyr`, `pROC`, `rpart`, `tidyr`, `rms`

```r
install.packages(c("ggplot2", "dplyr", "pROC", "rpart", "tidyr", "rms"))
```

## Usage

The data are not included in this repository. Set the path to your data file on the `read.csv()` line near the top of `ROC_Analysis.R`, then run the script. Tables (`.csv`) and figures (`.tiff`, `.pdf`, `.png`) are written to the working directory.
