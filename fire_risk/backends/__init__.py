import psycopg2

from .queries import ALL_RESIDENTIAL_FIRES
from psycopg2.extras import DictCursor


class Backend(object):
    """
    Backend mixin that should be used to implement APIs to read data.
    """

    def connect(self):
        """
        Connect to the backend.
        """
        raise NotImplementedError

    def close_connection(self):
        """
        Close the connection to the backend.
        """
        raise NotImplementedError

    def query(self):
        """
        Query the backend.
        """
        raise NotImplementedError


class FileBackend(Backend):
    """
    Parse a set of NFIRS incident flat files for structure fires.

    Args:
        flatfiles (list): a list of file pathnames for files to be parsed.
    Returns:
        changes the values of the firespread_count attributes to calculated
        values
    """

    pass


class PostgresBackend(Backend):
    """
    The Postgres Backend.
    """

    def __init__(self, connection_params):
        self.connection_params = connection_params

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close_connection()

    def connect(self):
        self.connection = psycopg2.connect(**self.connection_params)
        return self.connection

    def get_cursor(self):
        return self.connection.cursor(cursor_factory=DictCursor)

    def close_connection(self):
        self.connection.close()

    def query(self, query, query_params=()):
        cursor = self.get_cursor()
        cursor.execute(query, query_params)
        return cursor

    def get_firespread_counts(self, query=ALL_RESIDENTIAL_FIRES, query_params=()):
        results = self.query(query=query, query_params=query_params).fetchall()
        counts = dict(object_of_origin=0, room_of_origin=0, floor_of_origin=0, building_of_origin=0, beyond=0)

        for result in results:
            if result['fire_sprd'] == '1':
                counts['object_of_origin'] += result['count']

            if result['fire_sprd'] == '2':
                counts['room_of_origin'] += result['count']

            if result['fire_sprd'] == '3':
                counts['floor_of_origin'] += result['count']

            if result['fire_sprd'] == '4':
                counts['building_of_origin'] += result['count']

            if result['fire_sprd'] == '5':
                counts['beyond'] += result['count']

        return counts

if __name__ == '__main__':
    import doctest
    doctest.testmod()
