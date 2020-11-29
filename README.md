# International Migrant Stock Nowcasting

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

The ground-truth data comes from the United Nation's measurements of international migrant stock. 
