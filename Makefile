PROJECT=owm_resolver
DESCRIPTION="OpenWeatherMap city id resolver"
HOMEPAGE="https://github.com/ddqd/owm_resolver"

BUILD_DIR=$(CURDIR)/rel/$(PROJECT)
PACKAGE_DIR=$(CURDIR)/build

BRANCH=$(shell git rev-parse --abbrev-ref HEAD)
ifeq ($(BRANCH),master)
	PACKAGE?=$(PROJECT)
else
	PACKAGE?=$(PROJECT)-$(BRANCH)
endif

RELEASE_EXECUTABLE = rel/$(PROJECT)/bin/$(PROJECT)

OVERLAY_VARS_PRODUCTION?=files/vars.config
OVERLAY_VARS_DEBUG?=files/vars-dev.config
VERSION?=$(shell git describe --always --tags | sed -e "s/-[^-]*$$//;s/-/./")

all: build

clean:
	rm -rf build
	rebar clean

test: clean
	rebar eunit skip_deps=true

deps:
	rebar get-deps

compile: deps
	rebar compile
	rebar xref skip_apps=goldrush

build: compile
	rebar generate overlay_vars=$(OVERLAY_VARS_PRODUCTION)

build_testing: clean compile 
	rebar generate overlay_vars=$(OVERLAY_VARS_DEBUG)
	chmod +x $(RELEASE_EXECUTABLE)


testing_console: compile
	rebar generate overlay_vars=$(OVERLAY_VARS_DEBUG)
	chmod +x $(RELEASE_EXECUTABLE)
	./$(RELEASE_EXECUTABLE) console

deb: clean build
	mkdir -p $(PACKAGE_DIR)/etc/init.d
	mkdir -p $(PACKAGE_DIR)/etc/$(PROJECT)
	mkdir -p $(PACKAGE_DIR)/usr/lib/$(PROJECT)/bin
	mkdir -p $(PACKAGE_DIR)/var/log/$(PROJECT)

	cp -R $(BUILD_DIR)/erts-*   $(PACKAGE_DIR)/usr/lib/$(PROJECT)/
	cp -R $(BUILD_DIR)/lib      $(PACKAGE_DIR)/usr/lib/$(PROJECT)/
	cp -R $(BUILD_DIR)/releases $(PACKAGE_DIR)/usr/lib/$(PROJECT)/

	install -p -m 0755 $(BUILD_DIR)/bin/$(PROJECT)		$(PACKAGE_DIR)/usr/lib/$(PROJECT)/bin/$(PROJECT)
	install -p -m 0755 $(BUILD_DIR)/bin/$(PROJECT)		$(PACKAGE_DIR)/etc/init.d/$(PROJECT)
	install -m644 $(BUILD_DIR)/etc/app.config      		$(PACKAGE_DIR)/etc/$(PROJECT)/app.config
	install -m644 $(BUILD_DIR)/etc/vm.args         		$(PACKAGE_DIR)/etc/$(PROJECT)/vm.args

	fpm -s dir -t deb -f -n $(PACKAGE) --vendor dema -v $(VERSION) \
		--after-install $(CURDIR)/rel/files/postinst \
		--after-remove  $(CURDIR)/rel/files/postrm \
		--config-files /etc/$(PROJECT)/app.config \
		--config-files /etc/$(PROJECT)/vm.args \
		--deb-pre-depends adduser \
		--description $(DESCRIPTION) \
		-a native --url $(HOMEPAGE) \
		-C $(PACKAGE_DIR) etc usr var
