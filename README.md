# alzheimers-synthetic-data
Code to generate synthetic data following the pattern of research datasets in Alzheimer's Disease

## Code contents

This module generates a list of .csv files relating to different tables of a synthetic dataset from Alzheimer's Disease research.
The output folder for the .csv files is `inst/mockup_data/`. 
The list of generated tables is:
- apoe.csv
- cdr.csv
- csf.csv
- dot_counting.csv
- flanker.csv
- four_mountains.csv
- rbans.csv
- socio_demographics.csv
- vital_signs.csv
- volumetric.csv

## Executing the code

Clone the repo using: 
`git clone git@github:aridhia/alzheimers-synthetic-data`

### Using R-Studio
1. Open up R-Studio (available at [https://rstudio.com/](https://rstudio.com/))
2. Use the open project button in the top-right-hand corner
3. Open up the `alzheimers-synthetic-data.Rproj` in the root folder of the repo.
4. Navigate to the `Build` tab in R-Studio.
5. Click `Install and Restart` to install the library and restart the R Console.
6. Execute `inst/scripts/create_data.R` to generate data.
    - The number of synthetic data entries in the dataset can be modified by altering the `N` variable in `inst/scripts/create_data.R`.
    - Different datasets can be produced by modifying the random seed at the beginning of `inst/scripts/create_data.R`.
7. The data appears in the `inst/mockup_data/` folder as various .csv files relating to different tables in the dataset.
