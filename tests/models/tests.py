import unittest

from fire_risk.models.DIST import DIST
from fire_risk.backends import PostgresBackend
from fire_risk.backends.queries import RESIDENTIAL_FIRES_BY_FDID_STATE


class TestDISTModel(unittest.TestCase):

    def test_dist_import(self):
        floor_extent = False

        results = {'floor_of_origin': 126896L,
                   'beyond': 108959L,
                   'object_of_origin': 383787L,
                   'room_of_origin': 507378L,
                   'building_of_origin': 529300L}

        # from fire_risk.backends import PostgresBackend
        # with PostgresBackend(dict(host='localhost')) as backend:
        #     results = backend.get_firespread_counts()
        # print results
        dist = DIST(floor_extent=floor_extent, **results)

        self.assertAlmostEqual(dist.gibbs_sample(), 32.0, delta=4)

if __name__ == '__main__':
    unittest.main()
