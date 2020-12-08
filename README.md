# International Migrant Stock Nowcasting

## Requirements
* Python 3.8.5
* R
* Pip

## Requirements
To download the Python packages needed, run:
`pip install -r requirements.txt`

To run XGBoost on Mac OSX, you may be required to download OpenMP runtime. You can download this using Homebrew with the following command:
`brew install libomp`

## Data
The training data was pulled using the Facebook Marketing API. For each country, we collect the following information:
* `total_expat`: The total number of expats in a country
* `female_expat`: The total number of female expats in a country
* `male_expat`: The total number of male expats in a country
* `total_expat_ageX`: The number of expats in a country for age group X
* `male_expat_ageX`: The number of male expats in a country for age group X
* `female_expat_ageX`: The number of female expats in a country for age group X

We denoted the age groups as follows: 
Variable Name (X) | Age Group
------------ | -------------
0| 15-19
1| 20-24
2| 25-29
3| 30-34
4| 35-39
5| 40-44
6| 45-49
7| 50-54
8| 55-59
9| 60-64
10| 65+

The data pulled from Facebook can be found in `data/facebookData.csv`. 

The ground-truth data comes from the United Nation's measurements of international migrant stock. 

Our cleaned and combined data from the UN and Facebook can be found in `data/FB_UN_totals.csv`, which include the total migrant populations estimates, and `data/FB_UN_age_sex.csv`, which includes the age-sex subgroups. 

## Code
* `code/Migrant Predictions.ipynb` contains our three different regression techniques (linear ression, random forest, and XGBoost) and six different proposed models across three data splits. The raw results are found in `model_mapes.csv`
* The R code in the folder generates the figures found in the `figs` folder. 

