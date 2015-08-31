import click
import pandas
from scipy.stats import lognorm

@click.command()
@click.argument('filename')
@click.option('--column', default='Total_Trav', help='Column to identify shape, location, and scale from.')
def response_time_dist(filename, column):
    """
    Returns the lognormal distribution fit of travel times.
    """
    dt = pandas.read_csv(filename)
    response = lognorm.fit(dt[column])
    click.echo(response)
    return response


if __name__ == '__main__':
    response_time_dist()
