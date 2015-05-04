import unittest

from fire_risk.models import DIST
from fire_risk.backends import PostgresBackend


class TestDISTModel(unittest.TestCase):

    def test_dist_import(self):
        floor_extent = False

        results = {'floor_of_origin': 126896L,
                   'beyond': 108959L,
                   'object_of_origin': 383787L,
                   'room_of_origin': 507378L,
                   'building_of_origin': 529300L}

        #with PostgresBackend(dict(host='localhost')) as backend:
        #    results = backend.get_firespread_counts()
        #print results

        dist = DIST(room_area_uniform_limits=(72, 380), building_area_uniform_limits=(1088, 9004),
                    alarm_time_uniform_limits=(90, 120), dispatch_time_uniform_limits=(40, 80),
                    turnout_time_uniform_limits=(60, 100), arrival_time_uniform_limits=(300, 420),
                    suppression_time_uniform_limits=(60, 180), floor_extent=floor_extent, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 32.0, delta=4)

if __name__ == '__main__':
    unittest.main()