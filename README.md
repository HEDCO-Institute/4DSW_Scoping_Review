## Four-Day School Week Scoping Review Repository 

This repository contains the data and R code to reproduce the results of our Four Day School Week Scoping Review <br>
Additional materials, including the protocol, study materials, and dissemination products are available at <https://osf.io/kqyc6/>.

## Computational Environment

All of our analyses were ran on a Windows 10 Enterprise platform (x86_64-w64-mingw32/x64). <br>
Our analyses were conducted in R (version 4.2.2) <br>
Code to install and load all required packages are included in the beginning of the coding scripts. See below for the full list of packages and versions used in the analysis script:

- `pacman` (0.5.1)
- `rio` (0.5.29)
- `here` (1.0.1)
- `readxl` (1.4.0)
- `janitor` (2.1.0)
- `tidyverse` (1.3.2)
- `gt` (0.8.0.9000)

## Data

The excel data for this overview are included in the `data` folder of this repository. All data files are also publicly available at <https://osf.io/kqyc6/>. The value `-999` indicates a missing value that was not extracted for all data files. See below for a description of each data file:  <br>
<br>


| Data File | Description | File Structure |
|-----------|:----------:|-------------|
| `4dsw_study_data.xlsx` | Extracted descriptive data on eligible studies | One row per included study |
| `4dsw_eligibility_data.xlsx` | Eligibility information from each study | One row per reference ID (study citation) |
| `4dsw_all_citations.xlsx` | Citation information for all references (including reports of main studies) | One row per reference ID |
| `4dsw_linked_references.csv` | Reference information for additional reports of studies | One row per main reference and linked reference combo |
| `4dsw_duplicate_refs.csv` | Duplicate references from search results | One row per reference ID |
| `4dsw_app_data.xlsx` | Study data transformed for presenting in the data dashboard | One row per study |

## Code

Code necessary to reproduce our findings are included in the `code` folder: 

- `Analysis_Script.Rmd`: Cleaning and analysis script to reproduce descriptive results and produce appendices. 

Code to produce additional outputs, such as the data dashboard, can be found in the `outputs` folder.

## Replication Steps

To replicate our results: 

**If you have Rstudio and Git installed and connected to your GitHub account:**

1. Clone this repository to your local machine ([click for help](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2))
1. Run `Analysis_Script.Rmd` in the `code` folder to reproduce descriptive results

**If you need to install or connect R, Rstudio, Git, and/or GitHub:**

1. [Create a GitHub account](https://happygitwithr.com/github-acct.html#github-acct)
1. [Install R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
1. [Install git](https://happygitwithr.com/install-git.html)
1. [Link git to your GitHub account](https://happygitwithr.com/hello-git.html)
1. [Sign into GitHub in Rstudio](https://happygitwithr.com/https-pat.html)

**To reproduce our results without using Git and GitHub, you may use the following steps:** 

1. Create an R project on your local machine ([click for help](https://rpubs.com/Dee_Chiluiza/create_RProject))
1. Create the following folders in your R project root directory: `data`, `code`
1. Download all data files listed above from the repository and put them in the `data` folder you created
1. Download the `Analysis_Script.Rmd` listed above from the repository and put it in the `code` folder you created
1. Run `Analysis_Script.Rmd` to reproduce descriptive results

## Outputs
The output folder contains subfolders related to specific outputs or products:

1. `appendices`: HTML appendices output from the `Analysis_Script.Rmd`
2. `data_dashboard`: all data and code necessary to produce our [4DSW Data Dashboard](https://hedcoinstitute.uoregon.edu/dashboards/four-day-school-week-research-database)
3. `technical_report`: all data and code necessary to produce our [Technical Report](https://hedco-institute.github.io/4DSW_Scoping_Review/technical_report/)

## Contact

If you have any questions, concerns, or feedback, feel free to email Shaina Trevino at [strevino\@uoregon.edu](mailto:strevino@uoregon.edu)




