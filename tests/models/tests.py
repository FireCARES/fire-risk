import unittest
from fire_risk.models.DIST import DIST


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

if __name__ == '__main__':
    unittest.main()
