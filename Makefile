PROJECT = base
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

ERLC_OPTS = +'{parse_transform, lager_transform}' +'{lager_truncation_size, 512000}' +'{lager_extra_sinks, [sdebug,sinfo,swarning,serror,log1,log2,log3,log4,log5]}'
DEPS = eper lager recon
LOCAL_DEPS = tools
include erlang.mk
purge:
	rm -rf deps/*/ebin/*.app
	rm -rf deps/*/.idea
	rm -rf deps/*/.erlang.mk
	rm -rf deps/*/test
	rm -rf deps/*/examples
	rm -rf deps/*/doc
	rm -rf deps/*/.git
	rm -rf deps/*/*.d
