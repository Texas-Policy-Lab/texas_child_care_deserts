# Texas Child Care Deserts

API to create data for any count on child care deserts in Texas

## Function and attributes of the Texas Child Care Deserts Package

1. Create a database of data to be used to create the Texas Child Care Deserts

```{r}
root <- "F:/Early_Childhood/04_Tarrant_County"
child_care_db(root = root,
              naeyc_pth1 = naeyc_pth1,
              naeyc_pth2 = naeyc_pth2)
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

*Note*: The tract_radius (in miles) should be informed by analysis using the above functions.

```{r}
pth <- "F:/Early_Childhood/04_Tarrant_County/data/processed/child_care_env.RData"
load_env(pth)

config <- list(`48439` = list(tract_radius = 3),
               `48201` = list(tract_radius = 3))

calc.subsidy_capacity(config = config,
                      xwalk_tracts = XWALK_TRACTS,
                      adj_tracts = ADJ_TRACTS,
                      df_hhsc_ccl = DF_HHSC_CCL,
                      df_acf = DF_ACF)
                      
calc.subsidy_capacity(config = config,
                      xwalk_tracts = XWALK_TRACTS,
                      adj_tracts = ADJ_TRACTS,
                      df_hhsc_ccl = DF_HHSC_CCL,
                      df_acf = DF_ACF,
                      grouping_vars = "center_prvdr")
```

5. Subset the database to a specific county to application development

*Note*: The tract_radius (in miles) and capacity estimates should be informed by analysis using the above functions.

```{r}
pth <- "F:/Early_Childhood/04_Tarrant_County/data/processed/child_care_env.RData"
config <- list(`48439` = list(tract_radius = 3,
                              home_prvdr_non_sub_capacity = .85,
                              center_prvdr_non_sub_capacity = .85,
                              home_prvdr_sub_capacity = .75,
                              center_prvdr_sub_capacity = .65),
               `48201` = list(tract_radius = 3,
                              home_prvdr_non_sub_capacity = .85,
                              center_prvdr_non_sub_capacity = .85,
                              home_prvdr_sub_capacity = .84,
                              center_prvdr_sub_capacity = .78))

save_subset_child_care_db(pth = pth,
                          config = config)
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
