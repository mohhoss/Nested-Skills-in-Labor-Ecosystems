# Nested-Skills-in-Labor-Ecosystems
The repository holds a subset of R and Python code used in preparing the paper Skill dependencies uncover nested human capital.
Find the published version in Nature: https://www.nature.com/articles/s41562-024-02093-2

## Networks
The binary data based on Serrano et al 2009 is in file: Binary Skill-Occupation Bipartite - a la Serrano et al 2009 - alpha_in = 0.4, alpha_out = 0.275 - Nov 2022.csv
The skill clustering and nested division of skills is in file (also produced with the R code in the repo): preferred Skill Clustering and subtypes - Aug 5 2023.csv
The code to produce the skill hierarchy: Creating the skill hierarchy network.ipynb

## Bootstrapping Burning Glass Job Sequence
The sample upon which the code operates is cleaned job sequences obtained from Burning Glass resumes in the form of Data.Frame.

Input: The code imports such a job sequence Data.Frame, imports O*NET Skills (skill, ability, and knowledge data), as well as skill clusters (groups) and categories, as defined by the manuscript.

Output: The output is a randomized Data.Frame of job switches, and measured skill levels as per randomized job switches.

## Forming Synthetic Cohorts in Current Population Survey
The file shows our process of producing synthetic cohorts from CPS data by considering individuals born in each year as "cohorts" and following their skill compositions over time.

Input: CPS micro data, US Annual Inflation data, and O*NET Skills (skill, ability, and knowledge data), as well as skill clusters (groups) and categories, as defined by the manuscript. The included file merely describes the formation of cohorts (and excludes merging skills to the cohort data, which is trivial).

Output: a data.frame of individuals, each with a data field showing their birth year, that describes their synthetic cohort assignment.
