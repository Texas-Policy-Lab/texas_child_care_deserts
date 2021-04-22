# Texas Child Care Deserts

API to create data for any count on child care deserts in Texas

## Getting started

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
3. Run the `childcare_db` function
```{r}
root <- "F:/Early_Childhood/04_Tarrant_County"

childcare_db(root = root)
```
4. Update the function documentation `devtools::document()`
5. Run all the tests to make sure they are passing`devtools::test()`


### Ask for review
1. Once you've completed analysis of the questions in JIRA, ask a team member to review your work by sending a pull request
