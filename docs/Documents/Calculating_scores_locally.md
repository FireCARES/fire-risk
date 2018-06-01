# Calculating scores from synthetic data
The purpose of this Wiki is to familiarize users with the process of generating performance scores locally as well as how to calculate scores based on custom queries. For example, a user may want to calculate a performance score for a specific department for only a specific year. Furthermore, this guide should ensure that users incorporate all required elements for the score calculations so that locally generated scores match those displayed on the FireCARES website. 

Prior to using this guide, please make sure that you have successfully installed according to the instructions described at https://github.com/FireCARES/fire-risk. You must have PostgresSQL installed (available here: https://www.postgresql.org/download) Also, make sure your IP address is whitelisted for queries to the NFIRS database (contact Joe Meilinger for access). You should also receive a username and password for accessing the database.
Once installed, the DIST model can be found in the directory /fire-risk/fire_risk/models/DIST/DIST.py

It accepts arguments of fire spread counts. The idea is that higher performing departments are able to keep more fire contained to lower spread classifications. Let’s go ahead and calculate our first performance score. To do this, navigate to the folder that contains DIST.py and create a script with the following lines:

```python
from fire_risk.models import DIST

dist = DIST(floor_of_origin=34, beyond=12, object_of_origin=170, room_of_origin=190, building_of_origin=74, floor_extent=False)
dist.gibbs_sample()
```
This will import the DIST model to your current script. Then it creates a DIST object called “dist” that holds the fire spread counts of the hypothetical department to be evaluated. It is worth noting that object of origin fires and room of origin fires combined into one bin, and setting floor_extent to false makes it so that the floor of origin and building of origin counts are combined into the same bin. The last line executes the gibbs_sample method, which returns the actual performance score. Keep in mind that the proportion of each spread category is what determines the scores, not the total number of fires.  

# Calculating scores with NFIRS data
Running the DIST model with hardcoded values is useful for testing the code or making plots, but generally, it is more useful to query actual department data from NFIRS. Before doing this, it is recommended that you ensure you are able to connect to the NFIRS database through the terminal by running the following command:
```
'psql -h firecares-restore.c3gxdjk57saa.us-east-1.rds.amazonaws.com -U [user_name] -d nfirs'
```
You will then be prompted to enter your password. If everything is configured properly, you will have access to the NFIRS database through the terminal. 

Next, you will need to edit .pg_service.conf using your favorite text editor. If it does not exist,  go ahead and create the file. Add the following to .pg_service.conf:

```
[nfirs]
host=firecares-restore.c3gxdjk57saa.us-east-1.rds.amazonaws.com
port=5432
user=[user_name]
dbname=nfirs
password=[password]
```
You are now ready to calculate performance scores using real NFIRS data. Let’s create a script for generating local scores at the root directory of fire-risk called localscores.py.
First we will need to import the required modules. Obviously, we will need to import the DIST model and postgres for querying purposes. We will also need to import the query itself, which contains the lines of code that return the needed results from NFIRS. This is located in the fire_risk/backends/queries.py file and will be discussed in more detail later. Furthermore, for some communities, the performance score accounts for actual building area data and response time data specific to that community, which requires importing a building area module and a response time module to generate the same scores that are shown on the site.  The response time module also requires importing a log-normal draw module. The following lines of code show how to import all of the aforementioned required modules:

```python
#Importing the DIST model
from fire_risk.models import DIST
#Importing postgres
from fire_risk.backends import PostgresBackend
#Importing the query 
from fire_risk.backends.queries import RESIDENTIAL_FIRES_BY_FDID_STATE
#Importing building area data
from fire_risk.models.DIST.providers.ahs import ahs_building_areas
#Importing response time distributions
from fire_risk.models.DIST.providers.iaff import response_time_distributions
from fire_risk.utils import LogNormalDraw
```
Next, you need to specify the department you are interested in by creating a list that contains its FDID and state:

```python
#inputting the FDID and state of interest
fd = ('AW824', 'TX')
```

Now get the spread counts from NFIRS as follows:

```python
#Querying NFIRS
with PostgresBackend(dict(service='nfirs')) as backend:
    results = backend.get_firespread_counts(query=RESIDENTIAL_FIRES_BY_FDID_STATE, query_params=fd)
```

Next, add the response time information to results if it exists:

```python
#Get response time info
response_times = response_time_distributions.get('{0}-{1}'.format(*fd))
if response_times:
    results['arrival_time_draw'] = LogNormalDraw(*response_times, multiplier=60)
    print 'Added response time distribution for {}-{}'.format(*fd)
```
...then building draw information if it exists:

```python
#Getting building draw info if it exists
bld_draw = ahs_building_areas(*fd)
if not bld_draw is None:
    results['building_area_draw'] = bld_draw
    print 'Added building area draw results for {}-{}'.format(*fd)
```

Finally, run the DIST model with the results generated for the department of interest:
```python
print DIST(floor_extent=False, **results).gibbs_sample()
```
# Calculating scores based on custom conditions
By default, the aforementioned method will calculate scores for a department based on all available fire spread counts for that department. In some cases, it is desirable to calculate scores based only on fires that meet some user specified condition. For example, one may want to calculate the performance score for a department based only on fire incidents in the year 2009. Here we will show how to adapt the previous code to do this. 

First, note that we included the query called "RESIDENTIAL_FIRES_BY_FDID_STATE." Let's take a look at what exactly is in this query by opening fire-risk/fire_risk/backends/queries.py. The query contains the following lines:

```python
RESIDENTIAL_FIRES_BY_FDID_STATE = """
SELECT fdid, fire_sprd, COUNT(*)
FROM joint_buildingfires
WHERE prop_use IN ('419','429','439','449','459','460','462','464','400')
    AND fire_sprd IN ('1','2','3','4','5')
    AND fdid=(%s) AND state=(%s)
GROUP BY fdid, fire_sprd
ORDER BY fdid, fire_sprd;
"""
```
In this query, the SELECT line is where the user specifies which parameters will be returned from the database. In the above case, the fire spread and FDID will be returned from the query. Please refer to NFIRS Fire Data Analysis
Guidelines and Issues (https://www.usfa.fema.gov/downloads/pdf/nfirs/nfirs_data_analysis_guidelines_issues.pdf) for a full listing of the other parameters that can be queried. The lines after the WHERE keyword establish the conditions that must be met for the queried fire incidents separated by the AND keyword. In the above query, the prop_use must be in the 400's series, which indicates residential dwellings (see NFIRS guidelines). The reported fire spread must be a reported fire spread for the incident (between 1-5, inclusive) and the fdid and state must match those specified by the user when the query is called. Recall when we generated the scores before, we did so with the following lines:

```python
#inputting the FDID and state of interest
fd = ('AW824', 'TX')

#Querying NFIRS
with PostgresBackend(dict(service='nfirs')) as backend:
    results = backend.get_firespread_counts(query=RESIDENTIAL_FIRES_BY_FDID_STATE, query_params=fd)
```
In those lines, we set "query_params" equal to a list of strings that correspond to the %s placeholders in the queries.py file. If we want to calculate scores based only on the year 2009, we can make a new query. NFIRS provides a full timestamp called "inc_date" in the format MMDDYYYY. If we are interested in just the year, we must use the extract method. The resulting query is shown below:

```python
RESIDENTIAL_FIRES_BY_FDID_STATE_AND_YEAR = """
SELECT fdid, fire_sprd, COUNT(*)
FROM joint_buildingfires
WHERE prop_use IN ('419','429','439','449','459','460','462','464','400')
    AND fire_sprd IN ('1','2','3','4','5')
    AND fdid=(%s) AND state=(%s)
    AND extract('year' from inc_date)=(%s)
GROUP BY fdid, fire_sprd
ORDER BY fdid, fire_sprd;
"""
```

Add the above query to queries.py. Now, because this query has three placeholder strings, we must set "query_params" equal to a list of three strings:
```python
#Querying NFIRS
with PostgresBackend(dict(service='nfirs')) as backend:
    results = backend.get_firespread_counts(query=RESIDENTIAL_FIRES_BY_FDID_STATE_AND_YEAR, query_params=fd+['2009'])
```

Note that making the original list "fd" contain three strings will cause errors in the lines pertaining to the building areas and response times. 

Also, if your query returns relatively few fire incidents, you'll get the following error:
```
raise NotEnoughRecords
```
This error can be easily fixed by changing the minimum required number of incidents in fire-risk/fire_risk/models/DIST/DIST.py:

```python
    minimum_number_of_records = 10
```
Remember to import your new query and then you can run your updated script, which will print the score for San Antonio based only on incidents from 2009:

```python
#Importing the DIST model
from fire_risk.models import DIST
#Importing postgres
from fire_risk.backends import PostgresBackend
#Importing the query 
from fire_risk.backends.queries import RESIDENTIAL_FIRES_BY_FDID_STATE
#Importing the new query
from fire_risk.backends.queries import RESIDENTIAL_FIRES_BY_FDID_STATE_AND_YEAR
#Importing building area data
from fire_risk.models.DIST.providers.ahs import ahs_building_areas
#Importing response time distributions
from fire_risk.models.DIST.providers.iaff import response_time_distributions
from fire_risk.utils import LogNormalDraw

#inputting the FDID and state of interest (ex. San Antonio)
fd = ['AW824', 'TX']

#Querying NFIRS
with PostgresBackend(dict(service='nfirs')) as backend:
    results = backend.get_firespread_counts(query=RESIDENTIAL_FIRES_BY_FDID_STATE_AND_YEAR, query_params=fd+['2009'])
    
#Get response time info
response_times = response_time_distributions.get('{0}-{1}'.format(*fd))
if response_times:
    results['arrival_time_draw'] = LogNormalDraw(*response_times, multiplier=60)
    print 'Added response time distribution for {}-{}'.format(*fd)

#Getting building draw info if it exists
bld_draw = ahs_building_areas(*fd)
if not bld_draw is None:
    results['building_area_draw'] = bld_draw
    print 'Added building area draw results for {}-{}'.format(*fd)

print DIST(floor_extent=False, **results).gibbs_sample()
```

In a similar fashion, custom queries can be created to conduct various other analyses of scores. 


