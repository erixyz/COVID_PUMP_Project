# COVID_PUMP_Project
A year-long project funded both by CSUN's NSF and PUMP programs. This is an archive of the work done. Includes regression analysis and elementary epidemiological modelling. NSF Award #: DMS-1842386

# Description
 Here I will describe each of the files present.
 
 FILES______________________________________________________________________________________________________________________________
  - additional_code: file containing extra code. this is just a bunch of scrapped ideas that hold no bearing on the actual project.
  - percentiles: file containing csv describing which percentile US counties belong to. This was pulled and scrubbed from wikipedia. link -- https://en.wikipedia.org/wiki/List_of_United_States_counties_by_per_capita_income
 
 JUPYTER NOTEBOOKS__________________________________________________________________________________________________________________
  - Influenza_SIR: the initial direction of the project. this was a toy example of trying to model influenza at a school using the SIR epidemiological model.
  - LA_County_COVID_Analysis: an attempt at modeling the COVID-19 cases in LA county.\
  - COVID_and_Poverty: a change of direction. here a visual analysis is performed to understand if poverty alone plays a role in the spread of the pandemic in different regions. The tail end of this code marks the beginning of the creation of a CSV to perform regression analysis on R.
  - Add_Race_COVID: adds additional variables on to the CSV created in the COVID_and_Poverty file. adds ethnic/racial totals and percentages + population density + average households.
 
 CSV________________________________________________________________________________________________________________________________
  - final_covid_pls: the csv used to perform regression analysis in R. this is its final form.
 
 R SCRIPTS__________________________________________________________________________________________________________________________
  - dtime_counties: discrete probabalistic models are used to predict the doubling rate of the pandemic across the US. includes Poisson and Negative Binomial regression models.
  - regr_covid_county_final: ordinary least squares models are built to predict the doubling rate of the pandemic across the US. uses OLS models with Box-Cox transformations.
