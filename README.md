# COVID-19 Vaccination Analysis 

The main objective of this analysis is to compute descriptive statistics of the covid-19 vaccination data and forecast in how many days countries will vaccinate all of their population aged 16+. 

Forecasting was made using two methods: a naive method using the vaccination rate (ppl / day) and a "robust" method using Holt's Linear Trend modelling.

This repo contains all the data and R code to reproduce the analysis.

## Data sources

There are four data sources used in this analysis:

* Hannah Ritchie, _et al._ (2021), _Coronavirus Pandemic (COVID-19)_. Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/coronavirus' [Online Resource].
* World Bank (2021), _World Bank Country and Lending Groups_. Published online at DataHelpDesk.WorldBank.org. Retrieved from: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups [Online Resource].
* United Nations (2019), _World Population Prospects 2019_. Published online at population.un.org. Retrieved from: https://population.un.org/wpp/Download/Standard/CSV/
* United Nations (2021), _Standard country or area codes for statistical use_. Published online at UnStats.un.org. Retrieved from: https://unstats.un.org/unsd/methodology/m49 [Online Resource].

The last data source was used to match covid-19 indicators with the UN's population prospects.

## Output

The `Out/` directory contains all the output from the R code execution. The output is organized in three main subdirectories: `Data/`, `Plots/`, and `Summaries/`. The first one contains the "clean" versions of the covid-19 data and the UN population prospects. The second one contains plots. The third one contains some summary tables in CSV format.
