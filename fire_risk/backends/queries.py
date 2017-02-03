# To create the buildingfires table from the NFIRS database use this script:
# https://github.com/FireCARES/data/blob/master/sources/nfirs/scripts/building_fires.sql

ALL_BUILDING_FIRES = 'SELECT fire_sprd, COUNT(*) FROM buildingfires WHERE fire_sprd IN (\'1\',\'2\',\'3\',' \
                     '\'4\',\'5\') GROUP BY fire_sprd ORDER BY fire_sprd DESC;'

BUILDING_FIRES_BY_FDID_STATE = 'SELECT fire_sprd, COUNT(*) FROM buildingfires WHERE fire_sprd IN (\'1\',\'2\',' \
                                  '\'3\',\'4\',\'5\') AND fdid=(%s) AND state=(%s)  GROUP BY fire_sprd ORDER BY' \
                                  ' fire_sprd DESC;'


ALL_RESIDENTIAL_FIRES = """
SELECT fire_sprd, COUNT(*)
FROM buildingfires
WHERE prop_use IN ('419','429','439','449','459','460','462','464','400') AND fire_sprd IN ('1','2','3','4','5')
GROUP BY fire_sprd
ORDER BY fire_sprd;
"""


RESIDENTIAL_FIRES_BY_FDID_STATE = """
SELECT fdid, fire_sprd, COUNT(*)
FROM buildingfires
WHERE prop_use IN ('419','429','439','449','459','460','462','464','400')
    AND fire_sprd IN ('1','2','3','4','5')
    AND fdid=(%s) AND state=(%s)
GROUP BY fdid, fire_sprd
ORDER BY fdid, fire_sprd;
"""
