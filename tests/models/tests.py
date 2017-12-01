import unittest
import pytest
from mock import patch
from fire_risk import backends
from fire_risk.backends.queries import RESIDENTIAL_FIRES_BY_FDID_STATE
from fire_risk.models import DIST, DISTMediumHazard, DISTHighHazard, NotEnoughRecords
from fire_risk.models.DIST.providers.ahs import ahs_building_areas
from fire_risk.models.DIST.providers.iaff import response_time_distributions
from fire_risk.utils import LogNormalDraw


class TestDISTModel(unittest.TestCase):

    def test_dist_import(self):
        floor_extent = False

        results = {'floor_of_origin': 126896L,
                   'beyond': 108959L,
                   'object_of_origin': 383787L,
                   'room_of_origin': 507378L,
                   'building_of_origin': 529300L}

        dist = DIST(floor_extent=floor_extent, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 32.0, delta=4)

    def test_dist_high(self):
        # High hazard structure model testing (no AHS)
        floor_extent = False

        results = {'floor_of_origin': 20L,
                   'beyond': 9L,
                   'object_of_origin': 70L,
                   'room_of_origin': 97L,
                   'building_of_origin': 90L}

        dist = DISTHighHazard(floor_extent=floor_extent, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 7, delta=2)

    def test_dist_high_ahs(self):
        # High hazard structure model testing (with AHS)
        floor_extent = False

        results = {'floor_of_origin': 20L,
                   'beyond': 9L,
                   'object_of_origin': 70L,
                   'room_of_origin': 97L,
                   'building_of_origin': 90L,
                   'building_area_draw': ahs_building_areas('KA926', 'TX')}

        dist = DISTHighHazard(floor_extent=floor_extent, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 3, delta=2)

    def test_dist_high_ahs_with_response_times(self):
        # High hazard structure model testing (with AHS + response time distribution)
        floor_extent = False
        response_times = response_time_distributions['WB701-TX']

        results = {'floor_of_origin': 20L,
                   'beyond': 9L,
                   'object_of_origin': 70L,
                   'room_of_origin': 97L,
                   'building_of_origin': 90L,
                   'building_area_draw': ahs_building_areas('KA926', 'TX'),
                   'arrival_time_draw': LogNormalDraw(*response_times, multiplier=60)}

        dist = DISTHighHazard(floor_extent=floor_extent, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 22, delta=3)

    def test_dist_medium_ahs(self):
        # Medium hazard level (with AHS)
        floor_extent = False

        results = {'floor_of_origin': 243L,
                   'beyond': 91L,
                   'object_of_origin': 646L,
                   'room_of_origin': 1171L,
                   'building_of_origin': 785L,
                   'building_area_draw': ahs_building_areas('KA926', 'TX')}

        dist = DISTMediumHazard(floor_extent=floor_extent, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 10, delta=2)

    @patch('fire_risk.backends.psycopg2')
    def test_pg_backend(self, psycopg2):
        # Get fire spreads across ALL structure hazard levels

        mock_cur = psycopg2.connect.return_value.cursor.return_value
        res = [{'fdid': 'KA926', 'fire_sprd': '1', 'count': 1945L},
               {'fdid': 'KA926', 'fire_sprd': '2', 'count': 3987L},
               {'fdid': 'KA926', 'fire_sprd': '3', 'count': 853L},
               {'fdid': 'KA926', 'fire_sprd': '4', 'count': 3821L},
               {'fdid': 'KA926', 'fire_sprd': '5', 'count': 466L}]
        mock_cur.fetchall.return_value = res

        with backends.PostgresBackend(dict(service='nfirs')) as backend:
            results = backend.get_firespread_counts(query=RESIDENTIAL_FIRES_BY_FDID_STATE, query_params=('KA926', 'TX'))

        results['building_area_draw'] = ahs_building_areas('KA926', 'TX')

        dist = DIST(floor_extent=False, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 17, delta=2)

    @patch('fire_risk.backends.psycopg2')
    def test_pg_hazard_backend(self, psycopg2):
        # Get fire spread counts by hazard level for a specific department

        mock_cur = psycopg2.connect.return_value.cursor.return_value
        res = [{'risk_category': 'High',
                'object_of_origin': 70L,
                'room_of_origin': 97L,
                'floor_of_origin': 20L,
                'building_of_origin': 60L,
                'beyond': 9L
                },
               {'risk_category': 'Low',
                'object_of_origin': 659L,
                'room_of_origin': 1142L,
                'floor_of_origin': 228L,
                'building_of_origin': 1347L,
                'beyond': 151L
                },
               {'risk_category': 'Medium',
                'object_of_origin': 646L,
                'room_of_origin': 1171L,
                'floor_of_origin': 243L,
                'building_of_origin': 785L,
                'beyond': 91L
                },
               {'risk_category': 'N/A',
                'object_of_origin': 570L,
                'room_of_origin': 1577L,
                'floor_of_origin': 362L,
                'building_of_origin': 1629L,
                'beyond': 215L
                }]
        mock_cur.fetchall.return_value = res

        expected = {'High': 1,
                    'Medium': 8,
                    'Low': 20,
                    'N/A': 21}

        with backends.PostgresBackend(dict(service='nfirs')) as backend:
            results = backend.get_hazard_level_firespread_counts(query_params=('KA926', 'TX'))

        for k in results:
            res = results[k]
            res['building_area_draw'] = ahs_building_areas('KA926', 'TX')

            if k == 'High':
                dist = DISTHighHazard
            elif k == 'Medium':
                dist = DISTMediumHazard
            else:
                dist = DIST

            score = dist(floor_extent=False, **res).gibbs_sample()

            self.assertAlmostEqual(score, expected[k], delta=3)

    @patch('fire_risk.backends.psycopg2')
    def test_pg_hazard_response_time_distribution(self, psycopg2):
        fd = ('AW824', 'TX')
        mock_cur = psycopg2.connect.return_value.cursor.return_value
        res = [{'risk_category': 'High',
                'object_of_origin': 13L,
                'room_of_origin': 26L,
                'floor_of_origin': 4L,
                'building_of_origin': 13L,
                'beyond': 2L
                },
               {'risk_category': 'Low',
                'object_of_origin': 216L,
                'room_of_origin': 278L,
                'floor_of_origin': 94L,
                'building_of_origin': 258L,
                'beyond': 41L
                },
               {'risk_category': 'Medium',
                'object_of_origin': 141L,
                'room_of_origin': 138L,
                'floor_of_origin': 37L,
                'building_of_origin': 107L,
                'beyond': 12L
                },
               {'risk_category': 'N/A',
                'object_of_origin': 241L,
                'room_of_origin': 341L,
                'floor_of_origin': 89L,
                'building_of_origin': 313L,
                'beyond': 58L
                }]

        mock_cur.fetchall.return_value = res

        expected = {'High': None,
                    'Medium': 38,
                    'Low': 55,
                    'N/A': 57}

        with backends.PostgresBackend(dict(service='nfirs')) as backend:
            results = backend.get_hazard_level_firespread_counts(query_params=fd)

        for k in results:
            res = results[k]
            res['building_area_draw'] = ahs_building_areas(*fd)
            response_times = response_time_distributions.get('{0}-{1}'.format(*fd))
            if response_times:
                res['arrival_time_draw'] = LogNormalDraw(*response_times, multiplier=60)

            if k == 'High':
                dist = DISTHighHazard
            elif k == 'Medium':
                dist = DISTMediumHazard
            else:
                dist = DIST

            if expected[k] is not None:
                score = dist(floor_extent=False, **res).gibbs_sample()
                self.assertAlmostEqual(score, expected[k], delta=3)
            else:
                with pytest.raises(NotEnoughRecords):
                    dist(floor_extent=False, **res).gibbs_sample()



if __name__ == '__main__':
    unittest.main()
