import setuptools

setuptools.setup(
    name='zot-sync',
    version='0.3.1',
    description='Command-line interface to sync Zotero libraries to a local PostgreSQL database',
    author='Michael Paul Killian',
    author_email='rad@killian.email',
    url='https://github.com/michi-zuri/zot-sync',
    packages=setuptools.find_packages(),
    include_package_data=True,
    install_requires=[
        'Click',
        'SQLAlchemy',
        'psycopg2-binary',
        'pyzotero',
    ],
    entry_points='''
        [console_scripts]
        zot-sync=src.sync:cli
    ''',
)
