# fire-risk [![Build Status](https://travis-ci.org/FireCARES/fire-risk.svg?branch=master)](https://travis-ci.org/FireCARES/fire-risk)

Numerical Models for Developing Community Scale Risk Model

## Installation

Installing from source:

```bash
git clone https://github.com/FireCARES/fire-risk.git
cd fire-risk
pip install .
```

## Running the tests
After installing from source, the doctests and unit tests can be executed from the fire-risk root directory
using [pytest](http://pytest.org/):

```bash
py.test
```

### Running the Differential In Standard Time model
```python
from fire_risk.models.DIST import DIST

dist = DIST(floor_of_origin=34, beyond=12, object_of_origin=170, room_of_origin=190, building_of_origin=74, floor_extent=False)
dist.gibbs_sample()
```

### Running the Differential In Standard Time model when residential fire counts are stored in a Postgres database
```python
from fire_risk.models.DIST import DIST
from fire_risk.backends import PostgresBackend
from fire_risk.backends.queries import RESIDENTIAL_FIRES_BY_FDID_STATE

with PostgresBackend(dict(host='localhost')) as backend:
    # Get residential fire counts for Arlington, VA
    results = backend.get_firespread_counts(query=RESIDENTIAL_FIRES_BY_FDID_STATE, query_params=('01300', 'VA'))

dist = DIST(floor_extent=False, **results)

print dist.gibbs_sample()
```
