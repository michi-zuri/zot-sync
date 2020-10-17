import click

from . import fetch as _fetch, schema as _schema

class Config(object):
    def __init__(self):
         self.verbose = False
         self.engine  = None

config = click.make_pass_decorator(Config, ensure=True)

@click.group()
@click.option('--verbose', '-v', is_flag=True)
@click.argument('db_conn_string', type=str)
@config
def cli(config, verbose, db_conn_string):
    config.verbose = verbose
    try:
        config.engine = _schema.entry(db_conn_string, config.verbose)
    except ValueError as e:
        click.echo('check db_conn_string')
        if config.verbose :
            click.echo(e)

@cli.command()
@click.option('--key', '-k', type=str, help='Zotero API key')
@click.option('--user', '-u', type=int, help='fetch this user library')
@click.option('--group', '-g', type=int, help='fetch this group library')
@click.option('--all', '-a', is_flag=True, help='fetch all libraries accessible by user or else just by API key')
@click.option('--skip', '-s', is_flag=True, help='skip user library')
@config
def fetch(config, user, group, key, all, skip):
    if user and all :
        click.echo('fetching all libraries accessible by provided user with id %i:' % user)
        _fetch.from_all_by_user(config.engine, user, key, skip, config.verbose)
    elif key and all :
        click.echo('fetching all libraries, explicitly accessible by the provided API key now:')
        _fetch.from_all_by_key(config.engine, key, skip, config.verbose)
    elif user :
        click.echo('fetching user library with id %i now:' % user)
        _fetch.from_zotero_user(config.engine, user, key, config.verbose)
    elif group :
        click.echo('fetching group library with id %i now:' % group)
        _fetch.from_zotero_group(config.engine, group, key, config.verbose)
    elif key :
        click.echo('fetching API key properties for you now:')
        _fetch.key_info(key)
    else :
        click.echo('DB connection seems fine, now you need to provide at least one option. Type ... fetch --help for more details.')
