from setuptools import setup, find_packages

setup(
    name='zot-sync',
    version='0.1',
    py_modules=find_packages(),
    include_package_data=True,
    install_requires=[
        'Click',
        'SQLAlchemy',
        'psycopg2',
        'pyzotero',
    ],
    entry_points='''
        [console_scripts]
        zot-sync=src.sync:cli
    ''',
)

