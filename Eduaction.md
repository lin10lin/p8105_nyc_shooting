Untitled
================
Chenhui Yan
2024-12-02

``` r
# Load necessary library
library(readxl)

# Load the Excel file and read the relevant sheet into a DataFrame
file_path = "Soc_1822_CCD.xlsx"
sheet_name = "SocData"

# Read the specific sheet
df = read_excel(file_path, sheet = sheet_name)

# List of columns to extract based on your dataset description
columns_to_extract = c(
  'GeogName', 'Pop3plEnE', 'SE_NScPScE', 'SE_KndgtnE', 'SE_G1t8E',
  'SE_G9t12E', 'SE_ClgGScE', 'EA_P25plE', 'EA_LT9GE', 'EA_9t12NDE',
  'EA_HScGrdE', 'EA_SClgNDE', 'EA_AscDE', 'EA_BchDE', 'EA_GrdPfDE',
  'EA_LTHSGrE', 'EA_BchDHE'
)

# Check which columns are available in the dataset and select those
available_columns = columns_to_extract[columns_to_extract %in% colnames(df)]

# Create a new DataFrame with only the available columns
selected_columns_df = df[, available_columns]

# Create a mapping of old column names to new, more understandable names
column_name_mapping = c(
  'GeogName' = 'City Council districts',
  'Pop3plEnE' = 'Population 3 Years and Over Enrolled in School',
  'SE_NScPScE' = 'Nursery School, Preschool',
  'SE_KndgtnE' = 'Kindergarten',
  'SE_G1t8E' = 'Elementary School (Grades 1-8)',
  'SE_G9t12E' = 'High School (Grades 9-12)',
  'SE_ClgGScE' = 'College or Graduate School',
  'EA_P25plE' = 'Population 25 Years and Over',
  'EA_LT9GE' = 'Less Than 9th Grade',
  'EA_9t12NDE' = '9th to 12th Grade, No Diploma',
  'EA_HScGrdE' = 'High School Graduate (Includes Equivalency)',
  'EA_SClgNDE' = 'Some College, No Degree',
  'EA_AscDE' = 'Associate\'s Degree',
  'EA_BchDE' = 'Bachelor\'s Degree',
  'EA_GrdPfDE' = 'Graduate or Professional Degree',
  'EA_LTHSGrE' = 'Less Than High School Graduate',
  'EA_BchDHE' = 'Bachelor\'s Degree or Higher'
)

# Rename the columns in the DataFrame
names(selected_columns_df) = column_name_mapping[names(selected_columns_df)]

# Display the first few rows of the resulting DataFrame
print(head(selected_columns_df))
```

    ## # A tibble: 6 × 17
    ##   `City Council districts` Population 3 Years and Over …¹ Nursery School, Pres…²
    ##   <chr>                                             <dbl>                  <dbl>
    ## 1 City Council District 01                          29819                   1665
    ## 2 City Council District 02                          36203                   1472
    ## 3 City Council District 03                          19979                   1344
    ## 4 City Council District 04                          24773                   2211
    ## 5 City Council District 05                          26400                   3115
    ## 6 City Council District 06                          28444                   3369
    ## # ℹ abbreviated names: ¹​`Population 3 Years and Over Enrolled in School`,
    ## #   ²​`Nursery School, Preschool`
    ## # ℹ 14 more variables: Kindergarten <dbl>,
    ## #   `Elementary School (Grades 1-8)` <dbl>, `High School (Grades 9-12)` <dbl>,
    ## #   `College or Graduate School` <dbl>, `Population 25 Years and Over` <dbl>,
    ## #   `Less Than 9th Grade` <dbl>, `9th to 12th Grade, No Diploma` <dbl>,
    ## #   `High School Graduate (Includes Equivalency)` <dbl>, …

``` r
# Write the cleaned DataFrame to a new CSV file
write.csv(selected_columns_df, 'cleaned_data.csv', row.names = FALSE)
```
