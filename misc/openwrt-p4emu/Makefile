include $(TOPDIR)/rules.mk

PKG_NAME:=p4emu
PKG_RELEASE:=$(AUTORELEASE)

PKG_MAINTAINER:=mc36
PKG_LICENSE:=beer-ware

include $(INCLUDE_DIR)/package.mk

define Package/p4emu
	SECTION:=net
	CATEGORY:=Network
	TITLE:=p4emu
	DEPENDS:=
	URL:=http://www.freertr.org
	SUBMENU:=VPN
endef

define Package/p4emu/description
	p4emu
endef

define Package/p4emu/config
	source "$(SOURCE)/Config.in"
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)/etc/init.d
	mkdir -p $(PKG_BUILD_DIR)/etc/config
	$(CP) ./initd $(PKG_BUILD_DIR)/etc/init.d/p4emu
	$(CP) ./config $(PKG_BUILD_DIR)/etc/config/p4emu
	$(CP) ./maker $(PKG_BUILD_DIR)/Makefile
	$(CP) ./start $(PKG_BUILD_DIR)/p4emu.sh
	$(CP) ./native/* $(PKG_BUILD_DIR)/
	chmod +x $(PKG_BUILD_DIR)/etc/init.d/p4emu
	chmod +x $(PKG_BUILD_DIR)/p4emu.sh
endef

define Build/Configure
endef

define Build/Compile
	CFLAGS="$(TARGET_CFLAGS)" CPPFLAGS="$(TARGET_CPPFLAGS)" $(MAKE) -C $(PKG_BUILD_DIR) $(TARGET_CONFIGURE_OPTS)
endef

define Package/p4emu/install
	$(INSTALL_DIR) $(1)/usr/sbin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/p4emu.bin $(1)/usr/sbin/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/p4emu.sh $(1)/usr/sbin/
	$(CP) $(PKG_BUILD_DIR)/etc $(1)/
endef

$(eval $(call BuildPackage,p4emu))
