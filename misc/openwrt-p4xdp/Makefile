include $(TOPDIR)/rules.mk

PKG_NAME:=p4xdp
PKG_RELEASE:=$(AUTORELEASE)

PKG_MAINTAINER:=mc36
PKG_LICENSE:=beer-ware

PKG_BUILD_DEPENDS:=bpf-headers

include $(INCLUDE_DIR)/package.mk
include $(INCLUDE_DIR)/bpf.mk

define Package/p4xdp
	SECTION:=net
	CATEGORY:=Network
	TITLE:=p4xdp
	DEPENDS:=+libbpf $(BPF_DEPENDS)
	URL:=http://www.freertr.org
	SUBMENU:=VPN
endef

define Package/p4xdp/description
	p4xdp
endef

define Package/p4xdp/config
	source "$(SOURCE)/Config.in"
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)/etc/init.d
	mkdir -p $(PKG_BUILD_DIR)/etc/config
	$(CP) ./initd $(PKG_BUILD_DIR)/etc/init.d/p4xdp
	$(CP) ./config $(PKG_BUILD_DIR)/etc/config/p4xdp
	$(CP) ./maker $(PKG_BUILD_DIR)/Makefile
	$(CP) ./start $(PKG_BUILD_DIR)/p4xdp.sh
	$(CP) ./native/* $(PKG_BUILD_DIR)/
	chmod +x $(PKG_BUILD_DIR)/etc/init.d/p4xdp
	chmod +x $(PKG_BUILD_DIR)/p4xdp.sh
endef

define Build/Configure
endef

define Build/Compile
	$(call CompileBPF,$(PKG_BUILD_DIR)/p4xdp_kern.c)
	$(call CompileBPF,$(PKG_BUILD_DIR)/p4xdp_pass.c)
	CFLAGS="$(TARGET_CFLAGS)" CPPFLAGS="$(TARGET_CPPFLAGS)" $(MAKE) -C $(PKG_BUILD_DIR) $(TARGET_CONFIGURE_OPTS)
endef

define Package/p4xdp/install
	$(INSTALL_DIR) $(1)/usr/sbin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/p4xdp_user.bin $(1)/usr/sbin/
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/p4xdp_kern.o $(1)/usr/sbin/
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/p4xdp_pass.o $(1)/usr/sbin/
	mv $(1)/usr/sbin/p4xdp_kern.o $(1)/usr/sbin/p4xdp_kern.bin
	mv $(1)/usr/sbin/p4xdp_pass.o $(1)/usr/sbin/p4xdp_pass.bin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/p4xdp.sh $(1)/usr/sbin/
	$(CP) $(PKG_BUILD_DIR)/etc $(1)/
endef

$(eval $(call BuildPackage,p4xdp))
