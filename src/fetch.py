# https://www.python.org/dev/peps/pep-0263/ encoding: utf-8

import datetime, json, math, os

from sqlalchemy import text
from pyzotero.zotero import Zotero

from . import schema

def _duration(start_time) :
    return (datetime.datetime.now(datetime.timezone.utc)-start_time).total_seconds()

def _start_duration() :
    return datetime.datetime.now(datetime.timezone.utc)

def from_zotero_user(engine, user_id, api_key = None, verbose = False) :
    from_zotero_library(engine, user_id, 'user', api_key, verbose)

def from_zotero_group(engine, group_id, api_key = None, verbose = False):
    from_zotero_library(engine, group_id, 'group', api_key, verbose)

def from_all_by_user(engine, user_id, api_key, only_groups = False, verbose = False):
    try:
        _from_all_by_user(engine, user_id, api_key, only_groups, verbose)
    except Exception as e:
        print("\nsome error has occurred ¶")

def _from_all_by_user(engine, user_id, api_key, only_groups = False, verbose = False):
    z = Zotero(user_id, 'user', api_key)
    print('this feature is not implemented yet')

def from_all_by_key(engine, api_key, only_groups = False, verbose = False):
    try:
        _from_all_by_key(engine, api_key, only_groups, verbose)
    except Exception as e:
        print("\ninvalid API key starting with %s ¶" % api_key[:3])

def _from_all_by_key(engine, api_key, only_groups = False, verbose = False):
    z = Zotero('AnyLibrary', 'AnyType', api_key)
    key_info = z.key_info(limit=None)
    if 'user' in key_info['access'] and not only_groups :
        from_zotero_user(engine, key_info['userID'], api_key, verbose)
    else :
        return "not syncing user library for API key starting with %s" % api_key[:4]

    if 'groups' in key_info['access']:
        for group in key_info['access']['groups'] :
            if group == 'all' :
                from_all_by_user(engine, key_info['userID'], api_key, only_groups, verbose)
            from_zotero_group(engine, int(group), api_key, verbose)
    else :
        return "no groups to sync in this API key starting with %s" % api_key[:4]

def from_zotero_library(engine, library_id, library_type, api_key = None,  verbose = False):
    skip = False
    if not library_type[:1] == 'u' and not library_type[:1] == 'g' :
        print("invalid library_type %s" % library_type)
        skip = True
    if library_id > 999999999 :
        print("invalid group id %i" % library_id)
        skip = True
    if skip :
        print("Skipping library of type %s with id %i ¶\n" % (library_type, library_id))
        return
    if verbose :
        _from_zotero_library(engine, library_id, library_type, api_key, verbose)
    try:
        _from_zotero_library(engine, library_id, library_type, api_key, verbose)
    except Exception as e:
        library_type_id = "%s_%s_%i" % (str(e)[7:10], library_type[:1], library_id)
        print("\n%s ¶" % library_type_id)
        with engine.connect() as db:
            query = """
            INSERT INTO logs.zot_fetch (timestamp, library, name)
            VALUES ( DEFAULT, :lib, :error) RETURNING id,timestamp;
            """
            sync = db.execute(text(query), lib=library_type_id, error=str(e)).fetchone() # ( Int, datetime )
            print("Sync #%i was aborted at %s" % (sync[0], sync[1].strftime('%c')) )

def _from_zotero_library(engine, library_id, library_type, api_key = None, verbose = False):
    library_type_id = "zot_%s_%i" % (library_type[:1], library_id)

    # Every library gets a separate schema within the database
    item_type_schema = schema.for_library(engine, library_type_id, verbose)
    # returns dictionary of item table fields.

    # Setup the Zotero connection through pyzotero
    z = Zotero(library_id, library_type, api_key)
    check_access = z.items(limit=1, format="json", includeTrashed=1)
    library_name = check_access[0]['library']['name']

    print("\n%s %s ¶" % (library_type_id, library_name))

    # Start the engine and fetch items from the cloud!
    with engine.connect() as db:
        # Start sync timer and log attempt to sync.
        # Duration and latest version will be updated when finished.
        query = """
        INSERT INTO logs.zot_fetch (timestamp, library, name)
        VALUES ( DEFAULT, :lib, :name) RETURNING id,timestamp;
        """
        sync = db.execute(text(query), lib=library_type_id, name=library_name).fetchone() # ( Int, datetime )
        print("Sync #%i was started at %s" % (sync[0], sync[1].strftime('%c')) )

        # Get current local library version
        query = """
        SELECT version FROM logs.zot_fetch WHERE library='%s' AND duration IS NOT NULL ORDER BY timestamp DESC LIMIT 1;
        """ % library_type_id
        res_last_sync_version = db.execute(text(query)).fetchone() # ( Int, ) or None
        if res_last_sync_version :
            last_sync_version = res_last_sync_version[0]
            query = """
            SELECT COUNT(*) FROM %s.items WHERE NOT deleted ;
            """ % library_type_id
            local_count = db.execute(text(query)).fetchone() # ( Int, ) or None
            print("local mirror is at version %i and contains %i items" % (last_sync_version, local_count[0] ))
        else :
            last_sync_version = 0
            print("Starting initial sync of library %s" % library_type_id)

        # Get current remote library count and version
        z.top(limit=1, format='keys')
        remote_count    = int(z.request.headers.get('total-results', 0))
        library_version = int(z.request.headers.get('last-modified-version', 0))
        print("remote cloud is at version %i and contains %i items" % (library_version , remote_count))

        if last_sync_version < library_version :
            # Get list of local item keys and their versions
            query = """
            SELECT key,version FROM %s.items ;
            """ % library_type_id
            local_versions = dict(db.execute(text(query)).fetchall()) # { String: Int, }

            def _fetch_updates_and_inserts( start = 0 ) :
                start_round = _start_duration()
                inserts = 0
                update_list = z.top(limit=100, start=start, format='json', since=last_sync_version, includeTrashed=1)
                total_results = int(z.request.headers.get('Total-Results'))
                # Maybe there are only deletions to handle, so checking number of updates to handle
                if len(update_list) > 0 :
                    for item in update_list :
                        data = {}
                        for field,value in item['data'].items() :
                            data[field] = schema._typeset_for_db(field, value, item['data']['itemType'])
                            if field == 'version' :
                                update_string = '"version"=:version'
                                insert_field_string = '"key", "version"'
                                insert_value_string = ':key, :version'
                            elif field != 'key' :
                                update_string += ', "%s"=:%s' % (field, field)
                                insert_field_string += ', "%s"' % field
                                insert_value_string += ', :%s' % field
                        for field,value in item['meta'].items() :
                            data[field] = schema._typeset_for_db(field, value, item['data']['itemType'])
                            update_string += ', "%s"=:%s' % (field, field)
                            insert_field_string += ', "%s"' % field
                            insert_value_string += ', :%s' % field
                        item_type = item['data']['itemType']
                        if item['key'] in local_versions :
                            query = """
                            UPDATE %s."%s"
                            SET %s
                            WHERE key=:key ;
                            """ % (library_type_id, item_type, update_string)
                            db.execute(text(query), **data )
                        else :
                            query = """
                            INSERT INTO %s."%s" (%s)
                            VALUES ( %s ) ;
                            """ % (library_type_id, item_type, insert_field_string, insert_value_string)
                            db.execute(text(query),  **data )
                            inserts += 1
                    round_duration = _duration(start_round)
                    print( "Finished processing %i updates in %s seconds." % (len(update_list), str(round_duration)) )
                    if len(update_list) == 100 and start+100 < total_results :
                        print( "%i of %i updates done: fetching more updates now." % (start+100, total_results) )
                        inserts = inserts + _fetch_updates_and_inserts(start=start+100)
                    else :
                        print( "%i of %i updates have been processed." % ( total_results, total_results ) )
                else :
                    round_duration = _duration(start_round)
                    print( "Zero updates to process (it took %s seconds to figure that out)" % str(round_duration) )
                return inserts
            # fetch all updates in batches of 100 (includes updates to existing items and new items)
            inserts = _fetch_updates_and_inserts()

            def _fetch_deletions(since_version) :
                deletions = 0
                start_round = _start_duration()
                print( "Fetching list of deletions since last successful sync." )
                # Get list of deleted items from cloud
                delete_list = z.deleted(since=since_version)
                if len(delete_list['items']) > 0:
                    for item in delete_list['items'] :
                        if item in local_versions:
                            query = """
                            DELETE FROM %s.items WHERE key=:key ;
                            """ % library_type_id
                            db.execute(text(query), key=item)
                            deletions += 1
                        else:
                            print("Tried to DELETE item with key %s, but this item is not in local library..." % item )
                round_duration = _duration(start_round)
                print("Finished processing %i deletions in %s seconds" % ( len(delete_list['items']), str(round_duration) ) )
                return deletions

            # if this is not the initial sync, there's nothing to delete...
            if last_sync_version > 0:
                deletions = _fetch_deletions(last_sync_version)
                final_count = local_count[0] + inserts - deletions
            else :
                print("Initial sync has been successful. Next time atomic updates will be performed!")
        else :
            print("Nothing to sync, everything is up to date.")

        duration = _duration(sync[1])
        query = """
        UPDATE logs.zot_fetch
        SET duration=:duration, version=:version
        WHERE id=:id ;
        """
        db.execute(text(query), duration=math.ceil(duration), version=library_version, id=sync[0])
        # Closing connection to database ༺ with engine.connect() as db : ༻
    print("Syncing library %s took %s seconds\n" % (library_type_id, str(duration)))
