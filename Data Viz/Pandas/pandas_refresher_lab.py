# -*- coding: utf-8 -*-
"""Pandas Refresher Lab.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1qllMMZA-Rt152PB4qk-4xIJ0eixjnhYl

# Pandas Refresher Lab

![image.png](attachment:image.png)

#### Exercises built from dataset used for PyCon 2018 presentation by Kevin Markham

### 1. Import pandas and check the version.
"""

import pandas as pd
print(pd.__version__)

"""### 2. Import matplotlib and specify inline usage."""

# Commented out IPython magic to ensure Python compatibility.
# %matplotlib inline
import matplotlib.pyplot as plt

"""## Dataset: Stanford Open Policing Project

https://openpolicing.stanford.edu/

![image.png](attachment:image.png)

### 3. Read the police csv into a dataframe.
"""

#df = pd.read_csv("./data/police.csv")
from google.colab import drive
drive.mount('/content/drive')

df = pd.read_csv('/content/drive/My Drive/police.csv')

"""### 4. Use the head command to print the first five rows."""

df.head()

"""### 5. Determine the shape of the dataframe."""

df.shape

"""### 6. Determine the data type of each column."""

df.dtypes

"""### 7. Print out the number of null (missing) values in each column."""

df.isnull().sum()

"""### 8. Remove the column that only contains missing values."""

df.drop(columns=['county_name'])

"""### 9. Print out the shape of the updated dataframe."""

df.shape

"""### 10. Print out a list of the dataframe columns."""

df.columns

"""### 11. Determine the number of speeding violations by gender."""

print(df[(df['driver_gender'] == 'M') & (df['violation'] == 'Speeding')]['driver_gender'].count())
print(df[(df['driver_gender'] == 'F') & (df['violation'] == 'Speeding')]['driver_gender'].count())

"""### 12. When a man is pulled over, what percent of the time is it for speeding?"""

df_sex = df['driver_gender'] == 'M'
df_violation = df['violation'] == 'Speeding'
df_prob = df[df_sex & df_violation].count()
df_tot = df[df_sex & df['violation']].count()
prob = df_prob / df_tot
prob['violation'] * 100

"""### 13. When a woman is pulled over, what percent of the time is it for speeding?"""

df_sex = df['driver_gender'] == 'F'
df_violation = df['violation'] == 'Speeding'
df_prob = df[df_sex & df_violation].count()
df_tot = df[df_sex & df['violation']].count()
prob = df_prob / df_tot
prob['violation'] * 100

"""### 14. Use a groupby to determine the percentage of violation type for each gender.

For example, the distribution for women might be 80% speeding, 10% seat belt, and 10% moving violation.
"""

df_tot = df.groupby('violation').driver_gender.count()
print('Men: ')
print(df[df.driver_gender == 'M'].groupby('violation').violation.count() / df_tot * 100)
print('Women: ')
print(df[df.driver_gender == 'F'].groupby('violation').violation.count() / df_tot * 100)

"""### 15. Does gender affect who gets searched during a stop? Use a groupby or similar approach to compare.

In other words, what percent of the time does a male get searched during a stop? What about a female?
"""

df_tot = df.groupby('search_conducted').driver_gender.count()
print('Men: ')
print(df[df.driver_gender == 'M'].groupby('search_conducted').search_conducted.count() / df_tot * 100)
print('Women: ')
print(df[df.driver_gender == 'F'].groupby('search_conducted').search_conducted.count() / df_tot * 100)

"""### 16. What if one gender gets pulled over for violations that necessitate a search? Let's look at this another way. Determine the percentage of time that a driver is searched based on violation and then gender."""

df.groupby(['violation', 'driver_gender']).search_conducted.mean() * 100

"""### 17. We next want to examine how often the driver is frisked, but this is a little tricky due to multiple search types in that field. First, do a value_counts on the search_type column."""

df['search_type'].value_counts()

"""### 18. Parse the strings in the search_type column to get a count of how many times each search type was part of the search conducted.

For example, "Inventory,Reasonable Suspicion     4" should increase the count of both Inventory and Reasonable Suspicion by 4.
"""

df['explode'] = df['search_type'].str.split(',').explode('search_type')
df['explode'].value_counts()

"""### 19. Create a new column called "frisk" that is true if the phrase "Protective Frisk" is in the search_type field and false if it is not."""

import numpy as np
df['frisk'] = np.where(df['explode'] == 'Protective Frisk', True, False)
df

"""### 20. What percent of the time was a person frisked when a search was conducted?"""

df_frisk = df['frisk'] == True
df_search = df['search_conducted'] == True
df_total = df[df_search].count()
df_num = df[df_frisk].count()
prob = df_num / df_total
prob['frisk'] * 100

"""### 21. How many stops were there in each of the years represented in this dataset?"""

df['year'] = df['stop_date'].str.split('-').str[0]
df.groupby([df['year']]).search_conducted.value_counts()

"""### 22. Why do you think one particular year's number of stops is so much lower than the rest?

2005 had the least amount of conducted searches because the total number of stops was less.

### 23. What percent of the time is the stop related to drugs?
"""

df_drugs = df['drugs_related_stop'] == True
df_total = df[df_drugs].count()
prob = df_total / df['drugs_related_stop'].count()
prob['drugs_related_stop'] * 100

"""### 24. How does this percentage of stops related to drugs vary over the course of a day?"""

df['hour'] = df['stop_time'].str.split(':').str[0].astype(int)
df['time'] = df['hour'].apply(lambda x: 'morning' if 4 <= x < 12 else ('afternoon' if 12 <= x < 20 else 'night'))
hour = df.groupby([df['time']]).drugs_related_stop.value_counts()
hour

"""More drug related stops at night than during the morning or afternoon

### 25. Make a basic plot of the drug related stop percentage by hour of the day.
"""

hour_prob = (hour.count() / df['hour'].value_counts()) * 100
hour_prob.sort_index().plot.bar()

"""### 26. Are there any surprises in the drug related percentage by hour plot?

There are many drug related stops around 4-5 AM, which is questionable. They are significantly higher than the rest of the values.

### 27. What are the five most common hours that people are stopped?
"""

df_tod = df['stop_time'].str.split(':').str[0]
df_tod.value_counts().head(5)

"""### 28. Compare the age distributions for each violation by grouping by violation and then using the describe function."""

df.groupby([df['violation']]).driver_age.describe()

"""### 29. Who is the oldest person to get a speeding violation in this dataset?

The oldest person to get a speeding violation was 90.

### 30. Use ChatGPT to write a story about why this person was speeding.

In the heart of Philadelphia, nestled among the city's iconic streets and bustling neighborhoods, there lived a remarkable 90-year-old woman named Martha. Martha had always been known for her indomitable spirit and a zest for life that belied her age. She had weathered decades of change in the city she called home, but one thing that never wavered was her love for her vintage turquoise convertible, a 1955 Thunderbird.

One sunny afternoon, Martha decided to take her cherished Thunderbird for a spin down the city's historic boulevards. With the wind in her hair and the engine's thunderous roar, she couldn't help but give in to the temptation of speed. The exhilaration of going faster and faster was irresistible, and her Thunderbird seemed to come alive beneath her.

Unbeknownst to Martha, Officer Johnson, a vigilant and experienced traffic cop, was patrolling the area. The radar gun in his hand blinked with disbelief as he clocked Martha's Thunderbird going at a speed that would rival some sports cars on the highway.

As Martha's Thunderbird zoomed past him, Officer Johnson knew he had to give chase to address this unexpected speedster. With lights flashing and sirens blaring, he pursued her for a few thrilling minutes, finally convincing Martha to pull over to the side of the road.

Officer Johnson approached, a mix of awe and concern in his eyes. "Ma'am, do you have any idea how fast you were going?"

Martha, with a wry smile, replied, "Well, officer, I suppose I let the spirit of my Thunderbird get the better of me today. Did I set a new record?"

Officer Johnson, still catching his breath, checked her license and noted her birthdate. "You're 90 years old, ma'am? You sure know how to keep things exciting."

Martha laughed heartily. "Age may be a number, officer, but that doesn't mean I should ignore speed limits. I promise to be more mindful."

In the end, Officer Johnson issued Martha a speeding violation, a reminder that rules applied to all, regardless of age. As she drove away, her Thunderbird a bit tamer, Martha couldn't help but smile. She knew that while a ticket was a small price to pay, the memories of that exhilarating ride, at a speed that made even the officer raise an eyebrow, would stay with her forever, proof that a youthful spirit knew no boundaries.
"""