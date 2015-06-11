import random
from pandas import DataFrame, melt


FDID_TO_AHS = {
    'WP801-TX': 'Austin-Round Rock, TX AHS Area',
    '31000-NY': 'New York, NY AHS Area',
    '07212-FL': 'Orlando, FL AHS Area',
    '28008-NY': 'Rochester, NY AHS Area',
    '23035-PA': 'Philadelphia, PA-NJ AHS Area',
    # 'Northern New Jersey, NJ AHS Area'
    '25001-MD': 'Baltimore, MD AHS Area',
    '03002-NV': 'Las Vegas-Paradise, NV AHS Area',
    '56290-KY': 'Louisville, KY-IN AHS Area',
    '01032-FL': 'Miami-Ft. Lauderdale-Hollywood, FL AHS Area',
    '76000-VA': 'Richmond, VA AHS Area',
    '17M15-WA': 'Seattle-Tacoma-Everett, WA AHS Area',
    '03072-FL': 'Tampa-St. Petersburg, FL AHS Area',
    'KA926-TX': 'Houston, TX AHS Area',
    'CS931-IL': 'Chicago, IL AHS Area',
    '11223-AZ': 'Tucson, AZ AHS Area',
    '02072-FD': 'Jacksonville, FL AHS Area',
    '55013-OK': 'Oklahoma City, OK AHS Area',
    '11001-DC': 'Washington-Arlington, DC-VA-MD-WV AHS Area',
    '01300-VA': 'Washington-Arlington, DC-VA-MD-WV AHS Area',
    '25035-MA': 'Boston, MA AHS Area',
    'AW824-TX': 'San Antonio, TX AHS Area',
    '19532-TN': 'Nashville-Davidson-Murfreesboro, TN AHS Area',
    '08207-MI': 'Detroit, MI AHS Area',
    '02140-CT': 'Hartford, CT AHS Area',
    '27218-MN': 'Minneapolis-St. Paul, MN-WI AHS Area'
}

AHS_DATA = {'New York, NY AHS Area': {
    'h01_995': 146.1,
    'h01_994': 340.5,
    'h01_997': 213,
    'h01_996': 122.7,
    'h01_991': 678.1,
    'h01_990': 697,
    'h01_993': 430.6,
    'h01_992': 677.1,
    'h01_998': 994.1,
    'h01_899': 417.7,
    'total': 4716.9
    },
    'Orlando, FL AHS Area': {
        'h01_995': 57.3,
        'h01_994': 120.3,
        'h01_997': 18.2,
        'h01_996': 44.9,
        'h01_991': 107.9,
        'h01_990': 56.5,
        'h01_993': 189.3,
        'h01_992': 248.6,
        'h01_998': 104.1,
        'h01_899': 8.1,
        'total': 955.2
    },
    'Rochester, NY AHS Area': {
        'h01_995': 23.9,
        'h01_994': 57.6,
        'h01_997': 7.8,
        'h01_996': 25,
        'h01_991': 38.4,
        'h01_990': 25.8,
        'h01_993': 91.5,
        'h01_992': 90.6,
        'h01_998': 85.3,
        'h01_899': 10.7,
        'total': 456.6
    },
    'Philadelphia, PA-NJ AHS Area': {
        'h01_995': 131.1,
        'h01_994': 225.1,
        'h01_997': 92.2,
        'h01_996': 128.2,
        'h01_991': 158.4,
        'h01_990': 126.4,
        'h01_993': 272.6,
        'h01_992': 342.4,
        'h01_998': 585.4,
        'h01_899': 76.6,
        'total': 2138.4
    },
    'Austin-Round Rock, TX AHS Area': {
        'h01_995': 47.5,
        'h01_994': 76.4,
        'h01_997': 18.7,
        'h01_996': 50.1,
        'h01_991': 95.6,
        'h01_990': 68.9,
        'h01_993': 122.2,
        'h01_992': 158.4,
        'h01_998': 82.6,
        'h01_899': 14.9,
        'total': 735.3
    },
    'Northern New Jersey, NJ AHS Area': {
        'h01_995': 132.8,
        'h01_994': 246.4,
        'h01_997': 101.3,
        'h01_996': 147.9,
        'h01_991': 239.4,
        'h01_990': 197.8,
        'h01_993': 344,
        'h01_992': 374.4,
        'h01_998': 631.2,
        'h01_899': 110.3,
        'total': 2525.5
    },
    'Baltimore, MD AHS Area': {
        'h01_995': 79.9,
        'h01_994': 132.8,
        'h01_997': 86.2,
        'h01_996': 83.1,
        'h01_991': 94.6,
        'h01_990': 63.6,
        'h01_993': 164.6,
        'h01_992': 238.5,
        'h01_998': 124.9,
        'h01_899': 74.9,
        'total': 1143.1
    },
    'Las Vegas-Paradise, NV AHS Area': {
        'h01_995': 49.4,
        'h01_994': 89.8,
        'h01_997': 17.7,
        'h01_996': 38.9,
        'h01_991': 88.4,
        'h01_990': 52.9,
        'h01_993': 184.5,
        'h01_992': 209.5,
        'h01_998': 101.1,
        'h01_899': 20.8,
        'total': 853
    },
    'Louisville, KY-IN AHS Area': {
        'h01_995': 30,
        'h01_994': 53.1,
        'h01_997': 24.8,
        'h01_996': 40.3,
        'h01_991': 60.8,
        'h01_990': 34.3,
        'h01_993': 77,
        'h01_992': 131.2,
        'h01_998': 106.5,
        'h01_899': 5.7,
        'total': 563.7
    },
    'Miami-Ft. Lauderdale-Hollywood, FL AHS Area': {
        'h01_995': 128.4,
        'h01_994': 241.2,
        'h01_997': 85,
        'h01_996': 101.7,
        'h01_991': 340.3,
        'h01_990': 218.9,
        'h01_993': 405.9,
        'h01_992': 661.9,
        'h01_998': 222.5,
        'h01_899': 66.8,
        'total': 2472.6
    },
    'Richmond, VA AHS Area': {
        'h01_995': 37.4,
        'h01_994': 68.4,
        'h01_997': 16.5,
        'h01_996': 35.7,
        'h01_991': 60.4,
        'h01_990': 27.8,
        'h01_993': 91.8,
        'h01_992': 110,
        'h01_998': 81.9,
        'h01_899': 8.3,
        'total': 538.2
    },
    'Seattle-Tacoma-Everett, WA AHS Area': {
        'h01_995': 111.6,
        'h01_994': 202.8,
        'h01_997': 41.6,
        'h01_996': 90.6,
        'h01_991': 205.3,
        'h01_990': 132.1,
        'h01_993': 254.9,
        'h01_992': 321.7,
        'h01_998': 79.9,
        'h01_899': 46.4,
        'total': 1486.9
    },
    'Tampa-St. Petersburg, FL AHS Area': {
        'h01_995': 67.9,
        'h01_994': 153.9,
        'h01_997': 24,
        'h01_996': 46.2,
        'h01_991': 179.9,
        'h01_990': 115.4,
        'h01_993': 260.9,
        'h01_992': 395.3,
        'h01_998': 74.1,
        'h01_899': 44.9,
        'total': 1362.5
    },
    'Houston, TX AHS Area': {
        'h01_995': 190.5,
        'h01_994': 322.9,
        'h01_997': 70.1,
        'h01_996': 158.6,
        'h01_991': 302.8,
        'h01_990': 217.7,
        'h01_993': 423.2,
        'h01_992': 431.7,
        'h01_998': 226.2,
        'h01_899': 48.3,
        'total': 2392
    },
    'Chicago, IL AHS Area': {
        'h01_995': 178.4,
        'h01_994': 321.7,
        'h01_997': 138.8,
        'h01_996': 207.4,
        'h01_991': 369.9,
        'h01_990': 245.1,
        'h01_993': 445.8,
        'h01_992': 632.5,
        'h01_998': 628.6,
        'h01_899': 113.7,
        'total': 3281.9
    },
    'Tucson, AZ AHS Area': {
        'h01_995': 21,
        'h01_994': 48.2,
        'h01_997': 7.2,
        'h01_996': 15.3,
        'h01_991': 49.4,
        'h01_990': 40.7,
        'h01_993': 88.8,
        'h01_992': 101.6,
        'h01_998': 54.6,
        'h01_899': 19.2,
        'total': 446
    },
    'Jacksonville, FL AHS Area': {
        'h01_995': 33.3,
        'h01_994': 74.9,
        'h01_997': 12.5,
        'h01_996': 27.9,
        'h01_991': 57.9,
        'h01_990': 27.9,
        'h01_993': 144.6,
        'h01_992': 161,
        'h01_998': 58,
        'h01_899': 8.3,
        'total': 606.3
    },
    'Oklahoma City, OK AHS Area': {
        'h01_995': 31,
        'h01_994': 65.2,
        'h01_997': 11.4,
        'h01_996': 26,
        'h01_991': 81.1,
        'h01_990': 39.2,
        'h01_993': 112.7,
        'h01_992': 150.2,
        'h01_998': 23.2,
        'h01_899': 6.9,
        'total': 546.9
    },
    'Washington-Arlington, DC-VA-MD-WV AHS Area': {
        'h01_995': 167.8,
        'h01_994': 280.4,
        'h01_997': 207.4,
        'h01_996': 215.2,
        'h01_991': 241.1,
        'h01_990': 166,
        'h01_993': 286,
        'h01_992': 375.3,
        'h01_998': 259.8,
        'h01_899': 56.4,
        'total': 2255.4
    },
    'Boston, MA AHS Area': {
        'h01_995': 58.4,
        'h01_994': 115.4,
        'h01_997': 57.1,
        'h01_996': 75.1,
        'h01_991': 118.9,
        'h01_990': 84.9,
        'h01_993': 141.8,
        'h01_992': 195.3,
        'h01_998': 278.2,
        'h01_899': 73.8,
        'total': 1198.9
    },
    'San Antonio, TX AHS Area': {
        'h01_995': 55.1,
        'h01_994': 101.1,
        'h01_997': 19.3,
        'h01_996': 37.3,
        'h01_991': 112.2,
        'h01_990': 83.9,
        'h01_993': 156.3,
        'h01_992': 214.1,
        'h01_998': 56.8,
        'h01_899': 20.3,
        'total': 856.4
    },
    'Nashville-Davidson-Murfreesboro, TN AHS Area': {
        'h01_995': 46.9,
        'h01_994': 77.8,
        'h01_997': 26.1,
        'h01_996': 51.6,
        'h01_991': 69.9,
        'h01_990': 40.7,
        'h01_993': 110.4,
        'h01_992': 185.6,
        'h01_998': 62.7,
        'h01_899': 9.8,
        'total': 681.5
    },
    'Detroit, MI AHS Area': {
        'h01_995': 104.8,
        'h01_994': 226.1,
        'h01_997': 70,
        'h01_996': 105.7,
        'h01_991': 240.8,
        'h01_990': 97.2,
        'h01_993': 285.8,
        'h01_992': 491.1,
        'h01_998': 352.7,
        'h01_899': 34.3,
        'total': 2008.5
    },
    'Hartford, CT AHS Area': {
        'h01_995': 27.1,
        'h01_994': 62.4,
        'h01_997': 18.2,
        'h01_996': 32.3,
        'h01_991': 53.8,
        'h01_990': 34.6,
        'h01_993': 75.9,
        'h01_992': 94.8,
        'h01_998': 50.9,
        'h01_899': 18,
        'total': 468
    },
    'Minneapolis-St. Paul, MN-WI AHS Area': {
        'h01_995': 107.3,
        'h01_994': 208,
        'h01_997': 59.9,
        'h01_996': 125.6,
        'h01_991': 142.7,
        'h01_990': 84.3,
        'h01_993': 249.9,
        'h01_992': 241.3,
        'h01_998': 125.8,
        'h01_899': 27.5,
        'total': 1372.3
    }
}


AHS_BOUNDS = [
    (500, 500),
    (500, 749),
    (750, 999),
    (1000, 1499),
    (1500, 1999),
    (2000, 2499),
    (2500, 2999),
    (3000, 3999),
    (4000, 9000),
    (500, 9000)
    ]


class AHSHousingData(DataFrame):
    """
    Convenience class for drawing values from AHS data.
    """

    def __init__(self, *args, **kwargs):
        super(AHSHousingData, self).__init__(*args, **kwargs)

        df = melt(self)
        df = df[df['variable'] != 'total']
        df['value'] = df['value'].astype(float)
        df['normalized'] = df['value'] / sum(df['value'])
        df['cumulative'] = df['normalized'].cumsum()
        df['bounds'] = AHS_BOUNDS
        df['count'] = 0

        self.draw_data = df

    def draw(self):
        val = random.uniform(0, 1)

        if val == 1:
            min, max = self.draw_data['bounds'].get_values()[-1]

        else:
            vals = self.draw_data[self.draw_data['cumulative'] < val]

            if vals.empty:
                min, max = self.draw_data['bounds'].get_values()[0]
            else:
                min, max = vals['bounds'].get_values()[-1]

        return random.uniform(min, max)


def ahs_building_areas(fdid, state):
    """
    Draws building sizes from AHS data if the FDID and state are in the FDID_TO_AHS mapping.

    >>> random.seed(1234)
    >>> ahs_building_areas()
    Traceback (most recent call last):
            ...
    TypeError: ahs_building_areas() takes exactly 2 arguments (0 given)

    >>> x = AHSHousingData(AHS_DATA['Austin-Round Rock, TX AHS Area'], index=['value'])
    >>> x.draw()
    6203.6629958767635
    >>> random.seed(1234)
    >>> ahs_building_areas('WP801', 'TX').draw()
    6203.6629958767635
    >>> ahs_building_areas('no', 'way')

    """

    try:
        data = AHS_DATA[FDID_TO_AHS['{0}-{1}'.format(fdid, state)]]
    except KeyError:
        return

    data = AHSHousingData(data, index=['value'])
    return data

if __name__ == '__main__':
    import doctest
    doctest.testmod()
