# Empirical Compactness

Code to compute the **geographically-adjusted compactness** of U.S. congressional districts and to identify districts and statewide maps whose compactness is anomalously low after accounting for the geography a state's mapmakers actually had to work with.

Full methodology is in the accompanying paper:
**"Empirical Compactness"** — https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5278125

## What it does

Raw compactness scores (Polsby-Popper, Reock) penalize states with irregular borders and/or internal irregularities — characteristics outside a mapmaker's control. This project:

1. Measures two compactness metrics for every district: **Polsby-Popper** and **Reock**
2. Builds a set of per-district *explanatory* statistics describing each state's geography (state shape, number of seats, population density, county-level compactness, coastline share, statewide map compactness, etc.).
3. Fits a **beta regression** of district compactness on those statistics, producing an expected compactness distribution for each district *given its state's geography*.
4. Converts each district's raw score into a **geography-adjusted percentile** and runs two **outlier tests** — one for individual districts and one for whole statewide maps — to flag the least-compact-for-their-geography districts and maps.
5. Validates the outlier tests against known gerrymanders via **ROC analysis**.
6. Applies the calibrated model and tests **out-of-sample** to newly proposed mid-decade maps that were not used in calibration, demonstrating real-world performance on unseen data.

The model and outlier tests are **calibrated** on the **108th (2003-04)**, **113th (2013-14)**, and **118th (2023-24)** Congresses. Newly proposed **mid-decade** maps (e.g. 2025-26 TX / NC / VA / MO redraws) are held out of calibration and scored as a final **out-of-sample** test.

## Pipeline

The workflow is a Python (Jupyter/GeoPandas) stage that turns shapefiles into per-district statistics, followed by an R stage that does the modeling and outlier analysis. Steps 1-4 build and validate the model and tests on the three calibration Congresses; step 5 then applies the frozen model and tests to held-out mid-decade maps.

### 1. Compute shape statistics (Python notebooks → `Process data/`)

| Notebook | Purpose |
|---|---|
| `Get_CD108_data.ipynb`, `Get_CD113_data.ipynb`, `Get_CD118_data.ipynb` | Read the raw Census TIGER congressional-district shapefiles, set projection and precision, restrict to the lower-48, clip to the national boundary, and write cleaned `CD###raw` shapefiles. |
| `Calc_district _shape_statistics.ipynb` | Compute Polsby-Popper, Reock, and the state/county/coastline geometry statistics for each district; write the `CD###_shape_stats.csv` files. |
| `Countydata.ipynb` | Compute county-level Polsby/Reock/ReockX (`County2023.csv`), used to summarize each state's "natural" compactness. |

### 2. Compile the master dataset (`Data compile.R`)

Reads the three `CD###_shape_stats.csv` files plus `Statenames.csv`, `Apportionment20XX.csv`, and `County2023.csv`; drops single-district states; derives ~20+ explanatory variables; and writes **`Compiled_data.csv`**. Also writes a `Correlations.csv` multicollinearity report.  `Median checks.R`, `ST compactness check.R`provide supporting data analysis and diagnostics.

### 3. Fit the regression models (`Beta Regression Polsby.R`, `Beta Regression Reock.R`)

Beta regression of district compactness on the geography statistics. Each script writes its fitted coefficients to a `ModelParams_*.csv` file (one per metric / specification). `Regression sensitivity *.R` explores the sensitivity of the regession results to subsets of the data.

### 4. Relative compactness, outlier detection & validation

| Script | Purpose |
|---|---|
| `Relative compactness.R` | Apply a fitted model to compute each district's geography-adjusted compactness **percentile**. |
| `Outlier detection.R`, `Outliers_All.R` | The two outlier tests from the paper: a Kolmogorov-Smirnov test for biased statewide maps and a 1% threshold for individual districts; writes the outlier CSVs and geographic plots. |
| `ROC analysis Polsby.R`, `ROC analysis Reock.R`, `ROC plots both metrics.R` | ROC analysis benchmarking the outlier tests against known gerrymanders. |
| `HIstograms.R`, `Distribution HIstograms.R`, `Relative compactness plots.R`, `Outlier plots.R`, `Shape plots worst.R`, `Polsby RST analysis.R` | Supporting figures and diagnostic checks used in the paper. |

### 5. Out-of-sample test on mid-decade maps

The final step takes the model and outlier tests calibrated in steps 1-4 — unchanged — and applies them to **newly proposed mid-decade maps that were never used in calibration**. This measures genuine out-of-sample performance: how well the tests flag suspect districts and maps on data they have not seen.

| Notebook / Script | Purpose |
|---|---|
| `Get_Mid_decade_data.ipynb` | Read and clean the proposed mid-decade district shapefiles (e.g. 2025-26 TX / NC / VA / MO). |
| `Calc_district _shape_statistics_Mid_decade.ipynb` | Compute the same shape statistics for the mid-decade maps. |
| `Data compile Mid_decade.R` | Build the mid-decade modeling table in the same format as `Compiled_data.csv`. |
| `Outliers_Mid_decade.R` | Score the mid-decade maps with the calibrated model and run the outlier tests on this held-out data. |

## Repository layout

```
Get_CD*_data.ipynb / Calc_district _shape_statistics*.ipynb / Countydata.ipynb
                                  Python stage: shapefiles -> per-district stats
Data compile*.R                   build Compiled_data.csv (master modeling table)
Beta Regression *.R               fit beta-regression compactness models
Relative compactness*.R           geography-adjusted percentiles
Outlier detection.R / Outliers_*  outlier tests + maps
ROC analysis *.R / ROC plots*.R   validation against known gerrymanders
*.R (histograms, sensitivity)     supporting figures / diagnostics
Census data/                      INPUT shapefiles (git-ignored, see below)
Mid_decade_data/                  mid-decade (out-of-sample) map inputs (git-ignored)
Process data/                     intermediate + result CSVs (CSVs tracked; shapefiles ignored)
```

## Data setup

The bulk geographic inputs are **not** stored in git (they exceed GitHub's file-size limits and are freely re-downloadable). Before running the pipeline, download the shapefiles below into a `Census data/` folder:

| Data | Source |
|---|---|
| 108th Congress (2003-04) districts | https://www2.census.gov/geo/tiger/TIGER2010/CD/108/ |
| 113th Congress (2013-14) districts | https://www2.census.gov/geo/tiger/TIGER2013/CD/ |
| 118th Congress (2023-24) districts | https://www2.census.gov/geo/tiger/TIGER2023/CD/ |
| 2023 counties | https://www2.census.gov/geo/tiger/TIGER2023/COUNTY/ |
| 2023 national + coastline cartographic boundaries | https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2019.html |

What **is** tracked in `Process data/`: the small result CSVs (`Compiled_data.csv`, `CD###_shape_stats.csv`, `ModelParams_*.csv`, `ROCdata-*.csv`, `Correlations.csv`, apportionment tables, `Statenames.csv`).

> **Paths:** the notebooks and R scripts currently use absolute working-directory paths (e.g. `Shapepath` in the notebooks and `Dpath <- ".../Process data"` in the R scripts). Update these to your local layout before running.

## Dependencies

- **Python:** `geopandas`, `shapely`, `numpy`, `scipy`, `pandas`, `session_info` (Jupyter). Districts are reprojected to an equal-area CRS before any area/perimeter math.
- **R:** `dplyr`, `tidyr`, `sf`, `ggplot2`, `svglite`, `ggspatial`, `betareg`, `fitdistrplus`, `rstatix`, `agricolae`, `corrgram`, `scales`.

## License

See [`LICENSE`](LICENSE).
