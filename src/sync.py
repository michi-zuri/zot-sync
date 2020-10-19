import json

import click

from . import fetch     as _fetch
from . import schema    as _schema
from . import check     as _check

class Config(object):
    def __init__(self):
         self.verbose = False
         self.engine  = None

config = click.make_pass_decorator(Config, ensure=True)

@click.group()
@click.option('--verbose', '-v', is_flag=True)
@config
def cli(config, verbose):
    config.verbose = verbose

@cli.command(no_args_is_help=True)
@click.option('--db', '--database', '-d', type=str, help='e.g. postgres://user:pass@host:port/db', required=True)
@click.option('--user', '-u', type=int, help='fetch this user library')
@click.option('--group', '-g', type=int, help='fetch this group library')
@click.option('--key', '-k', type=str, help='use this Zotero API key')
@click.option('--all', '-a', is_flag=True, help='fetch all accessible (combine with -u or -k)')
@click.option('--skip', '-s', is_flag=True, help='skip user library (combine with -a or -k)')
@config
def fetch(config, db, user, group, key, all, skip):
    try:
        config.engine = _schema.entry(db, config.verbose)
    except ValueError as e:
        click.echo('check database (connection)')
        if config.verbose :
            click.echo(e)
    if (user and all) or (key and all):
        click.echo('fetching all accessible libraries:')
        if not user :
            user = _check.key_info(key)['userID']
        if not skip:
            _fetch.from_zotero_user(config.engine, user, key, config.verbose)
        else :
            click.echo('skipping user library as requested')
        _fetch.from_all_groups_by_user(config.engine, user, key, skip, config.verbose)
    elif user :
        click.echo('fetching user library with id %i now:' % user)
        _fetch.from_zotero_user(config.engine, user, key, config.verbose)
    elif group :
        click.echo('fetching group library with id %i now:' % group)
        _fetch.from_zotero_group(config.engine, group, key, config.verbose)
    elif key:
        click.echo('fetching all libraries, explicitly accessible by the provided API key now:')
        _fetch.from_all_by_key(config.engine, key, skip, config.verbose)
    else :
        click.echo("DB connection seems fine, now you need to provide at least one option. Type "+click.style('zot-sync fetch --help', bg='white', fg='black')+" for more details.")

@cli.command(no_args_is_help=True)
@click.option('--key', '-k', type=str, help='check Zotero API key properties')
@config
def check(config, key):
    if key :
        click.echo(json.dumps(_check.key_info(key), indent=4))
