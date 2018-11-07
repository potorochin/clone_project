PROJECT = autorization

PROJECT_VERSION = 0.1.0



# deps
DEPS = cowboy jsx epgsql eredis erlsom #reloader
# deps urlseloader

dep_jsx = git git@git.ceb.loc:erlang/jsx.git v2.4.0
dep_epgsql = git git@git.ceb.loc:erlang/epgsql.git 2.0.0-1
dep_eredis = git git@git.ceb.loc:erlang/eredis.git v1.1.0
dep_erlsom = git git@git.ceb.loc:erlang/erlsom.git master
dep_cowboy = git git@git.ceb.loc:erlang/cowboy.git 2.4.0-ceb
#dep_reloader = git git@git.ceb.loc:erlang/reloader master


# Compiler options.
#ERLC_OPTS ?= -W1
#ERLC_OPTS += +'{parse_transform, lager_transform}' +'{lager_truncation_size, 1024}'

# git urls
RELX_URL = https://git.ceb.loc/erlang/relx/raw/v1.1.0-1/relx
PKG_FILE_URL ?= https://git.ceb.loc/erlang/erlang-mk/raw/master/packages.v2.tsv

include erlang.mk

.PHONY: debug

debug: app rel
	cp sys.config _rel/autorization/releases/1/
	./_rel/$(PROJECT)/bin/$(PROJECT) console

