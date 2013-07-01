PROJECT = gcm

# options

PLT_APPS = crypto asn1 public_key ssl sasl

### We somehow need this dialyzer option. Without it, dialyzer crashes
### with the following error message:
### `undefined parse transform 'lager_transform'`.
DIALYZER_OPTS = -pa deps/lager/ebin
###

# dependencies

DEPS = cowboy jiffy bitcask ibrowse lager
dep_cowboy = https://github.com/extend/cowboy.git 0.8.6
dep_jiffy = https://github.com/davisp/jiffy.git 0.8.4
dep_bitcask = https://github.com/basho/bitcask.git 1.6.3
dep_ibrowse = https://github.com/cmullaparthi/ibrowse.git v4.0.2
dep_lager = https://github.com/basho/lager.git 2.0.0

# standard targets

include erlang.mk

check test: tests
