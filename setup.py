import setuptools

required = [
    'Click',
    'SQLAlchemy',
    'pyzotero',
]

# Please choose what works for you,
# choose psycopg2 source distribution if it works on your system (recommended) or
# opt for psycopg2-binary if you require a quick fix without guarantees (convenient).
# example: pip install zot-sync[recommended]
psycopg2 = {
    'recommended': ['psycopg2'],
    'alternative': ['psycopg2-binary']
}

setuptools.setup(
    name='zot-sync',
    version='0.3.2',
    description='Command-line interface to sync Zotero libraries to a local PostgreSQL database',
    author='Michael Paul Killian',
    author_email='rad@killian.email',
    url='https://github.com/michi-zuri/zot-sync',
    packages=setuptools.find_packages(),
    include_package_data=True,
    install_requires= required,
    extras_require= psycopg2,
    entry_points={
        'console_scripts': [
            'zot-sync=zot_sync.cli:run',
        ],
    },
)
