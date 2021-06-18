import json
import click
from src import check as _check
from src import fetch as _fetch
from src import schema as _schema

class Config(object):
    def __init__(self):
         self.verbosity = False
         self.engine  = None

config = click.make_pass_decorator(Config, ensure=True)

@click.group()
@click.version_option()
@click.option('--verbose', '-v', is_flag=True)
@config
def run(config, verbose):
    config.verbosity = verbose
    if config.verbosity :
        click.echo('verbose output:')

@run.command(no_args_is_help=True)
@click.option('--db', '--database', '-d', type=str, help='e.g. postgres://user:pass@host:port/db', required=True)
@click.option('--user', '-u', type=int, help='fetch this user library')
@click.option('--group', '-g', type=int, help='fetch this group library')
@click.option('--key', '-k', type=str, help='use this Zotero API key')
@click.option('--all', '-a', is_flag=True, help='fetch all accessible (combine with -u or -k)')
@click.option('--skip', '-s', is_flag=True, help='skip user library (combine with -a or -k)')
@config
def fetch(config, db, user, group, key, all, skip):
    try:
        config.engine = _schema.entry(db, config.verbosity)
    except Exception as e:
        click.echo('check database (connection)')
        if config.verbosity :
            click.echo(e)
    if (user and all) or (key and all):
        click.echo('fetching all accessible libraries:')
        if not user :
            user = _check.key_info(key)['userID']
        if not skip:
            _fetch.from_zotero_user(config.engine, user, key, config.verbosity)
        else :
            click.echo('skipping user library as requested')
        _fetch.from_all_groups_by_user(config.engine, user, key, skip, config.verbosity)
    elif user :
        click.echo('fetching user library with id %i now:' % user)
        _fetch.from_zotero_user(config.engine, user, key, config.verbosity)
    elif group :
        click.echo('fetching group library with id %i now:' % group)
        _fetch.from_zotero_group(config.engine, group, key, config.verbosity)
    elif key:
        click.echo('fetching all libraries, explicitly accessible by the provided API key now:')
        _fetch.from_all_by_key(config.engine, key, skip, config.verbosity)
    else :
        click.echo("DB connection seems fine, now you need to provide at least one option. Type "+click.style('zot-sync fetch --help', bg='white', fg='black')+" for more details.")

@run.command(no_args_is_help=True)
@click.option('--key', '-k', type=str, help='check Zotero API key properties')
@click.option('--db', '--database', '-d', type=str, help='check database connection')
@config
def check(config, key, db):
    if db :
        try:
            config.engine = _schema.entry(db, config.verbosity)
        except Exception as e:
            click.echo('There was a problem. '+"\n")
            click.echo(repr(e))
    if key :
        try:
            click.echo(json.dumps(_check.key_info(key), indent=4))
        except Exception as e:
            click.echo('There was a problem, please check your key and your network connection.')
            click.echo(str(e))

if __name__ == '__main__':
    run()