PROJECT = gcm

# options

PLT_APPS = crypto asn1 public_key ssl sasl

ERLC_OPTS = -pa $(DEPS_DIR)/lager/ebin +'{parse_transform, lager_transform}'
DIALYZER_OPTS = -pa $(DEPS_DIR)/lager/ebin

# dependencies

DEPS = lager cowboy jiffy bitcask ibrowse
dep_lager = https://github.com/basho/lager.git 2.0.0
dep_cowboy = https://github.com/extend/cowboy.git 0.8.6
dep_jiffy = https://github.com/davisp/jiffy.git 0.8.4
dep_bitcask = https://github.com/basho/bitcask.git 1.6.3
dep_ibrowse = https://github.com/cmullaparthi/ibrowse.git v4.0.2

# standard targets

include erlang.mk

check test: tests
