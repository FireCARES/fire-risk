# To create the buildingfires table from the NFIRS database use this script:
# https://github.com/FireCARES/data/blob/master/sources/nfirs/scripts/materialized_views/public.buildingfires.sql

ALL_BUILDING_FIRES = 'SELECT fire_sprd, COUNT(*) FROM joint_buildingfires WHERE fire_sprd IN (\'1\',\'2\',\'3\',' \
                     '\'4\',\'5\') GROUP BY fire_sprd ORDER BY fire_sprd DESC;'

BUILDING_FIRES_BY_FDID_STATE = 'SELECT fire_sprd, COUNT(*) FROM joint_buildingfires WHERE fire_sprd IN (\'1\',\'2\',' \
                                  '\'3\',\'4\',\'5\') AND fdid=(%s) AND state=(%s)  GROUP BY fire_sprd ORDER BY' \
                                  ' fire_sprd DESC;'


ALL_RESIDENTIAL_FIRES = """
SELECT fire_sprd, COUNT(*)
FROM joint_buildingfires
WHERE prop_use IN ('419','429','439','449','459','460','462','464','400') AND fire_sprd IN ('1','2','3','4','5')
GROUP BY fire_sprd
ORDER BY fire_sprd;
"""


RESIDENTIAL_FIRES_BY_FDID_STATE = """
SELECT fdid, fire_sprd, COUNT(*)
FROM joint_buildingfires
WHERE prop_use IN ('419','429','439','449','459','460','462','464','400')
    AND fire_sprd IN ('1','2','3','4','5')
    AND fdid=(%s) AND state=(%s)
GROUP BY fdid, fire_sprd
ORDER BY fdid, fire_sprd;
"""


RESIDENTIAL_FIRES_BY_FDID_STATE_HAZARD = """
SELECT *
    FROM crosstab(
      'select COALESCE(y.risk_category, ''N/A'') as risk_category, fire_sprd, count(*)
        FROM joint_buildingfires a left join (
          SELECT state,
            fdid,
            inc_date,
            inc_no,
            exp_no,
            geom,
            x.parcel_id,
            x.risk_category
          FROM (select * from joint_incidentaddress a
             left join parcel_risk_category_local b using (parcel_id)
             ) AS x
        ) AS y using (state, inc_date, exp_no, fdid, inc_no)
    where a.fdid='%s'
        and a.state='%s'
        and prop_use in (''419'',''429'',''439'',''449'', ''459'',''460'',''462'',''464'',''400'')
        and fire_sprd is not null and fire_sprd != ''''
    group by risk_category, fire_sprd
    order by risk_category, fire_sprd ASC')
    AS ct(risk_category text, "object_of_origin" bigint, "room_of_origin" bigint,
        "floor_of_origin" bigint, "building_of_origin" bigint, "beyond" bigint);
"""
