# NORC AmeriSpeak Omnibus Analysis

This repository contains code and notebooks for analysis of the NORC AmeriSpeak Omnibus, focusing on voter demographics and policy positions in the 2024 presidential election. 

## Project Setup

The project uses `renv` for package management to ensure reproducibility. 

The environment is managed through three key files:

- `packages.yml`: Defines all required R packages in a single location
- `r-setup.R`: Manages `renv` package versions and dependencies
  - Initializes `renv` if needed
  - Installs packages defined in `packages.yml`
  - Creates/updates the `renv.lock` file
- `project-setup.R`: Loads packages and sources utility files
  - Loads all packages defined in `packages.yml`
  - Sources utility files (colors, themes, data loading, etc.)
- `render-quarto-notebooks.R`: Compiles all analysis notebooks
  - Renders all .qmd files in sequence
  - Generates both HTML and Word outputs where specified

To set up the project:

1. Clone the repository
2. Source `r-setup.R` to initialize renv and install required packages
3. Source `project-setup.R` to load packages and utility files
4. Run `Rscript render-quarto-notebooks.R` to compile all analysis notebooks
5. Run `7_render_tables.R` to take the final produced tables and convert them into Word documents.

The `renv.lock` file records the exact package versions used in this analysis to ensure reproducibility.

## Running the Analysis

The analysis is split into five files. While the reports are numbered, the only requirement is to run step 1 first; the results in 2-5 do not depend on each other.

1. `1_norc-data-tabulation.qmd`: Processes raw data and creates tabulations
   - Creates pre-processed data files in the `data/` directory
   - Generates demographic and policy position tabulations

2. `2_norc-report-generation.qmd`: Creates the main report tables
   - Uses pre-processed data from step 1
   - Generates tables showing demographic characteristics and policy positions

3. `3_paper-artifact-creation.qmd`: Creates visualizations and additional analyses
   - Uses pre-processed data from step 1
   - Generates figures and supplementary analyses

4. `4_statistical-tests.qmd`: Performs statistical analysis of party differences
   - Uses pre-processed data from step 1
   - Analyzes differences between party groups:
     - Democrat vs. Republican (baseline comparison)
     - New Trump Voter vs. Biden/Harris Voter (switchers to Trump)
     - New Harris Voter vs. Biden/Harris Voter (switchers to Harris)
     - New Trump Voter vs. Trump Voter (Trump consistency)
     - New Harris Voter vs. Trump Voter (cross-party comparison)
   - Uses weighted logistic regression with multiple testing corrections:
     - Bonferroni (most conservative)
     - Holm (less conservative)
     - FDR (least conservative)
   - Reports odds ratios and confidence intervals for each comparison

5. `5_table-generation.qmd`: Generates formatted tables for publication
   - Uses pre-processed data from step 1
   - Creates publication-ready tables with weighted percentages and standard errors
   - Includes demographic cross-tabulations and policy position comparisons
   - Outputs both HTML and Word formats
   - Draft attemps

6. `6_final-tables.qmd`: Final publication tables
   - Creates final, publication-ready tables with weighted percentages and standard errors
   - Focuses on demographic cross-tabulations and vaccine-related policy positions
   - Features tabbed views with and without standard errors
   - Outputs both HTML and Word formats
   - Final version for publication

## Project Structure

### Analysis Notebooks

```
1_norc-data-tabulation.qmd      # Initial data processing and tabulation
2_norc-report-generation.qmd    # Main report generation
3_paper-artifact-creation.qmd   # Visualizations and analyses
4_statistical-tests.qmd         # Party difference analysis
5_table-generation.qmd          # Draft tables
6_final-tables.qmd              # Final publication tables
```

### Utility Scripts

```
7_render_tables.R               # Script to convert final tables into Word Documents
```

### Setup & Configuration

```
packages.yml                    # Package definitions
r-setup.R                       # renv initialization
project-setup.R                 # Package loading
build-meta-df.R                 # Creates metadata frame from codebook
build-data-df.R                 # Processes raw data and creates analysis variables
build-svy-design.R              # Creates survey design object for analysis
render-quarto-notebooks.R       # Notebook compilation
```

### Styling & Utilities

```
colors.R                        # Color definitions
theme-jhu.R                     # JHU plotting theme
load-data.R                     # Data loading functions
table-utils.R                   # Table formatting
data-labels.R                   # Variable labels and recoding functions
utils.R                         # General utilities
```

### Data & Output

```
data/                           # Processed data files
images/                         # Generated visualizations
```

## Data Processing Pipeline

The data processing pipeline has been refactored into modular components:

1. **Metadata Processing** (`build-meta-df.R`)
   - Creates metadata frame from codebook
   - Handles variable descriptions and response sets

2. **Data Processing** (`build-data-df.R`)
   - Loads raw SAS data
   - Applies variable labels
   - Creates derived variables through modular functions:
     - `create_demographic_vars()`: Demographic variables
     - `create_voting_vars()`: Voting-related variables
     - `create_party_vars()`: Party affiliation and switching

3. **Survey Design** (`build-svy-design.R`)
   - Creates `survey` design object
   - Handles weights and survey design

## Author

Erik Westlund  
Johns Hopkins Biostatistics Center  
Department of Biostatistics  
Johns Hopkins Bloomberg School of Public Health  
ewestlu1@jhu.edu

