# https://www.python.org/dev/peps/pep-0263/ encoding: utf-8

from pyzotero.zotero import Zotero

def key_info(api_key, verbose = False):
    print('checking API key properties...')
    z = Zotero('AnyLibrary', 'AnyType', api_key)
    return z.key_info(limit=None)
