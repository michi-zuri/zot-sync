# https://www.python.org/dev/peps/pep-0263/ encoding: utf-8

import datetime, json, math, os

from sqlalchemy import create_engine, text
import requests

def entry(database, verbose = False):
    """ Prepare database for sync logging.
        Creates a schema, tables, views and columns as needed
    """
    fields = {}
    fields['id'] = 'serial PRIMARY KEY'
    fields['timestamp'] = 'timestamp with time zone DEFAULT now()'
    fields['version'] = 'integer'
    fields['library'] = 'varchar(15)'
    fields['name'] = 'varchar(1023)'
    fields['duration'] = 'integer'

    # Database connection setup with sqlalchemy
    engine = create_engine(database)

    with engine.connect() as db:

        query = """
CREATE SCHEMA IF NOT EXISTS logs ;"""
        db.execute(text(query))
        if verbose :
            print(query)

        query = """
CREATE TABLE IF NOT EXISTS logs.zot_fetch ();"""
        db.execute(text(query))
        if verbose :
            print(query)

        for field,type in fields.items() :
            query = """
ALTER TABLE logs.zot_fetch ADD COLUMN IF NOT EXISTS "%s" %s;
            """ % (field, type)
            db.execute(text(query))
            if verbose :
                print(query)

    # pass on engine for further connections
    return engine

def from_zotero(flush_cache = False, verbose = False) :
    schema_file = os.path.normpath(os.path.join(os.path.dirname(__file__), './schema.json'))
    try :
        with open(schema_file, 'r') as file :
            schema = json.load(file)
    except :
        schema = {}
    headers = schema.get('headers',{} )
    # flush cache
    if flush_cache :
        headers = { 'Accept-Encoding' : 'gzip' }
    updated_schema = None
    try:
        response = requests.get("https://api.zotero.org/schema", headers=headers)
    except:
        response = {}
    if verbose :
        print(response.status_code)
    if response.status_code == 200 :
        updated_schema = response.json()
        updated_schema['headers'] = {}
        updated_schema['headers']['Accept-Encoding'] = 'gzip'
        updated_schema['headers']['If-None-Match'] = response.headers['ETag']
        updated_schema['headers']['If-Modified-Since'] = response.headers['Last-Modified']
        unsorted_fields = {}
        for type in updated_schema['itemTypes'] :
            if type['itemType']=='note' :
                type['fields'] = [{ 'field' : 'note', 'baseField' : 'title' }
                , { 'field' : 'customJSON' }
                , { 'field' : 'parsedDate',     'baseField' : 'undefined' }
                , { 'field' : 'creatorSummary', 'baseField' : 'undefined' }
                , { 'field' : 'creators',       'baseField' : 'undefined' }
                ]
            for field in type['fields'] :
                key = field.get('baseField',field['field'])
                if key=='accessDate' :
                    unsorted_fields[key] = 'timestamp'
                elif key=='customJSON' :
                    unsorted_fields[key] = 'jsonb'
                else :
                    unsorted_fields[key] = 'varchar(65535)'
        sorted_fields = sorted(unsorted_fields.items())
        updated_schema['fields'] = dict(sorted_fields)
        with open(schema_file, 'w') as file:
            json.dump(updated_schema, file)
    if updated_schema :
        print( "The Zotero schema was updated to version last modified %s" % updated_schema['headers']['If-Modified-Since'] )
        return updated_schema
    elif schema :
        if verbose :
            print( "Using cached Zotero schema last modified %s" % schema['headers']['If-Modified-Since'] )
        return schema
    else :
        raise ValueError("Schema could not be loaded for the Zotero API.")

def _key_field(foreign = False, lib_schema = None ) :
    if foreign :
        field = { 'key': 'char(8) PRIMARY KEY REFERENCES {lib}.items.key(char(8)) ON DELETE CASCADE'.format( lib = lib_schema ) }
    else :
        field = { 'key': 'char(8) PRIMARY KEY' }
    return field

def _special_fields() :
    ''' Changes to these fields requires special attention when patched.
    '''
    fields = {}
    fields['itemType'] = 'varchar(20)'
    fields['deleted'] = 'boolean DEFAULT FALSE'
    fields['creators'] = 'jsonb'
    fields['tags'] = 'jsonb'
    fields['collections'] = 'jsonb'
    fields['relations'] = 'jsonb'
    return fields

def _system_fields() :
    ''' Changes to these fields are only made by the Zotero API server.
    '''
    fields = {}
    fields['version'] = 'integer'
    fields['numChildren'] = 'integer'
    fields['dateAdded'] = 'timestamp with time zone'
    fields['dateModified'] = 'timestamp with time zone'
    return fields

def _meta_fields() :
    ''' These meta fields are managed by the Zotero API server.
    '''
    fields = {}
    fields['createdByUser'] = 'jsonb'
    fields['lastModifiedByUser'] = 'jsonb'
    fields['parsedDate'] = 'varchar(127)'
    fields['creatorSummary'] = 'varchar(127)'
    return fields

def for_library(engine, library_type_id, verbose = False):
    """ Prepare database table to accomodate all possible fields for storage.
        Creates a schema, tables, views and columns as needed
    """
    schema = from_zotero(verbose)

    fields = _key_field()
    fields.update(_system_fields())
    fields.update(_meta_fields())
    fields.update(_special_fields())
    fields.update(schema['fields'])

    with engine.connect() as db:

        query = """
CREATE SCHEMA IF NOT EXISTS %s""" % library_type_id ;
        db.execute(text(query))
        if verbose :
            print(query)

        query = """
CREATE TABLE IF NOT EXISTS %s.items ();""" % library_type_id
        db.execute(text(query))
        if verbose :
            print(query)

        query = """
CREATE TABLE IF NOT EXISTS %s.meta ();""" % library_type_id
        db.execute(text(query))
        if verbose :
            print(query)

        for field,type in fields.items() :
            query = """
ALTER TABLE %s.items ADD COLUMN IF NOT EXISTS "%s" %s;
            """ % (library_type_id, field, type)
            db.execute(text(query))
            if verbose :
                print(query)

        for item_type in schema['itemTypes'] :
            view_fields = [
            '"%s"'% s for s in list(_key_field().keys()) ] + [
            '"%s"'% s for s in list(_system_fields().keys()) ] + [
            '"%s"'% s for s in list(_meta_fields().keys()) ] + [
            '"%s"'% s for s in list(_special_fields().keys()) ]

            for field in item_type['fields'] :
                view_name = field['field']
                base_name = field.get('baseField', None)
                if base_name=='undefined' :
                    view_fields.remove('"%s"' % view_name)
                    continue
                if base_name :
                    alias_name = '"%s" AS "%s"' % (base_name, view_name)
                else :
                    alias_name = '"%s"' % view_name
                view_fields.append(alias_name)
            field_string = ', '.join(view_fields)
            query = """
CREATE OR REPLACE VIEW %s."%s" AS
SELECT %s FROM %s.items WHERE "itemType" = :type ;
            """ % (library_type_id, item_type['itemType'], field_string, library_type_id)
            db.execute(text(query), type = item_type['itemType'])
            if verbose :
                print(query)

    return schema

def _typeset_for_db(field, value, item_type) :
    if type(value) != int and len(value)==0 :
        return None
    elif field in ('deleted') :
        return str(value)
    elif field in ('customJSON', 'creators', 'tags', 'collections', 'relations', 'createdByUser', 'lastModifiedByUser') :
        return json.dumps(value)
    else :
        return value

def _parse_JSON_from_note(value) :
    return json.dumps({'casetext' : json.dumps(value) })

'''
Todo: make table for local enrichments to data.
Column('pmid', Integer, comment='Pubmed identifier'),
Column('eid', Integer, comment='Scopus electronic identifier: the prefix `2-s2.0-` is omitted'),
Column('keywords', String(1000), comment='keywords suggested by the author(s)'),
Column('CAS', String(1000), comment='Chemical Abstracts Service identifier for chemical substances'),
Column('MeSH', String(1000), comment='Medical Subject Headings indexed by MEDLINE'),
Column('EMTREE', String(1000), comment='EMTREE indexed by EMBASE'),
'''

'''
Tables for Abstrackr predictor
    t_citations = Table("citations", metadata, autoload=True)
    t_labels = Table("labels", metadata, autoload=True)
    t_reviews = Table("projects", metadata, autoload=True)
    t_prediction_status = Table("predictionstatuses", metadata, autoload=True)
    t_predictions = Table("predictions", metadata, autoload=True)
    t_priorities = Table("priorities", metadata, autoload=True)
    #t_users = Table("user", metadata, autoload=True)
    #t_labeled_features = Table("labeledfeatures", metadata, autoload=True)
'''
