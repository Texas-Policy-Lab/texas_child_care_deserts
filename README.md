# Texas Child Care Deserts

API to create data for any count on child care deserts in Texas

## Function and attributes of the Texas Child Care Deserts Package

1. Create a database of data to be used to create the Texas Child Care Deserts

```{r}
root <- "F:/Early_Childhood/04_Tarrant_County"
child_care_db(root = root)
```

2. Load an child care deserts database or a subset of the database with clean and processed data for use

```{r}
pth <- "F:/Early_Childhood/04_Tarrant_County/data/processed/child_care_env.RData"
load_env(pth)
```

3. Compute child care distance distributions

```{r}
pth <- "F:/Early_Childhood/04_Tarrant_County/data/processed/child_care_env.RData"
load_env(pth)
county <- c("48201", "48439")
calc.distance_decile_table(df = DF_ACF, county_list = county)
````

```{r}
pth <- "F:/Early_Childhood/04_Tarrant_County/data/processed/child_care_env.RData"
load_env(pth)
county <- c("48201", "48439")
calc.distance_density_plot(df = DF_ACF, county_list = county)
```

4. Calculate subsidy providers average capacity

Note: The mileage should be informed by the distribution that parents travel to get
childcare which can be calculated using the above listed functions. Choose year and
quarters of interest. Three quarter-years must be used.

```{r}
pth <- "F:/Early_Childhood/04_Tarrant_County/data/processed/child_care_env.RData"
load_env(pth)
county <- c("48439", "48201")
tract_radius <- 3
calc.subsidy_capacity(county = county,
                      tract_radius = tract_radius,
                      xwalk_tracts = XWALK_TRACTS,
                      df_hhsc_ccl = DF_HHSC_CCL,
                      df_acf = DF_ACF,
                      qtrs = c("1","2","4"))
```

5. Subset the database to a specific county to application development

```{r}
pth <- "F:/Early_Childhood/04_Tarrant_County/data/processed/child_care_env.RData"
county <- c("48439", "48201")
tract_radius <- 3 # This should be informed by the literature and the calculations from child care distance distribution functions
home_prvdr_capacity <- .85 
center_prvdr_capacity <- .85
subsidy_prvdr_capacity <- .65 # This should be informed by the calculations from the subsidy capacity functions. Tarrant = .65, Harris = .78

save_subset_child_care_db(pth, county, tract_radius, home_prvdr_capacity, center_prvdr_capacity, subsidy_prvdr_capacity)
```

## Project workflow

### Clone the analysis and set-up the environment

1. Open Git Bash
2. Type `git clone https://github.com/Texas-Policy-Lab/texas_child_care_deserts.git`
3. Open the R-project
4. Type `install.packages("renv")`
5. In the console type `renv::restore()` to create the local R-environment

### Create a new branch
1. Assign a JIRA ticket to yourself
2. In the console type `git checkout -b features/TCCCD-10` or whatever JIRA ticket number you've assigned to yourself
3. Make the changes to the code
4. Stage your commit by typing `git add file_name` and then type `git commit -m "commit message"
5. To push your changes to the server type `git push`

### Test the code

1. Make changes to the code according the to ticket you are working on
2. Load the package `devtools::load_all()`
3. Run the `child_care_db` function
```{r}
root <- "F:/Early_Childhood/04_Tarrant_County"
child_care_db(root = root)
```
4. Update the function documentation `devtools::document()`
5. Run all the tests to make sure they are passing`devtools::test()`

### Ask for review
1. Ask a team member to review your work by sending a pull request
