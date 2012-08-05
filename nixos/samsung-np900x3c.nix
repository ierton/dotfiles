# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:

{
  require = [
      # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  hardware.firmware = [ "/root/firmware" ];

  boot.initrd.kernelModules = [
      # Specify all kernel modules that are necessary for mounting the root
      # filesystem.
      # "xfs" "ata_piix"
    ];

  boot.blacklistedKernelModules = [
    "fbcon"
    ];

  #boot.kernelPackages = pkgs.linuxPackages_3_5;

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  # Europe/Moscow
  time.timeZone = "Etc/GMT-4";

  networking = {
    hostName = "greyblade"; # Define your hostname.

    interfaceMonitor.enable = false;
    wireless.enable = false;
    useDHCP = false;
    wicd.enable = true;
  };

  # Add filesystem entries for each partition that you want to see
  # mounted at boot time.  This should include at least the root
  # filesystem.
  fileSystems = [
    { mountPoint = "/";
      device = "/dev/disk/by-label/ROOT";
    }
    { mountPoint = "/boot";
      device = "/dev/disk/by-label/BOOT";
    }
    { mountPoint = "/home";
      device = "/dev/disk/by-label/HOME";
    }
  ];

  # List swap partitions activated at boot time.
  swapDevices = [
    # { device = "/dev/disk/by-label/swap"; }
  ];

  powerManagement = {
    enable = true;
  };

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  security = {
    sudo.configFile =
      ''
        # Don't edit this file. Set nixos option security.sudo.configFile instead
        # env vars to keep for root and %wheel also if not explicitly set
        Defaults:root,%wheel env_keep+=LOCALE_ARCHIVE
        Defaults:root,%wheel env_keep+=NIX_PATH
        Defaults:root,%wheel env_keep+=TERMINFO_DIRS

        # "root" is allowed to do anything.
        root        ALL=(ALL) SETENV: ALL

        # Users in the "wheel" group can do anything.
        %wheel      ALL=(ALL) SETENV: NOPASSWD: ALL
      '';
  };

  services.ntp = {
    enable = true;
    servers = [ "server.local" "0.pool.ntp.org" "1.pool.ntp.org" "2.pool.ntp.org" ];
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us,ru";
    xkbOptions = "eurosign:e, grp:alt_space_toggle, ctrl:swapcaps, grp_led:caps, ctrl:nocaps";

    desktopManager.xfce.enable = true;
    videoDrivers = [ "intel" ];
    displayManager = {
      slim = {
        enable = true;
        defaultUser = "ierton";
      };
    };

    multitouch.enable = false;

    synaptics = {
      enable = true;
      accelFactor = "0.05";
      maxSpeed = "10";
      twoFingerScroll = true;
      additionalOptions =
        ''
        MatchProduct "ETPS"
        Option "FingerLow"                 "3"
        Option "FingerHigh"                "5"
        Option "FingerPress"               "30"
        Option "MaxTapTime"                "100"
        Option "MaxDoubleTapTime"          "150"
        Option "FastTaps"                  "1"
        Option "VertTwoFingerScroll"       "1"
        Option "HorizTwoFingerScroll"      "1"
        Option "TrackstickSpeed"           "0"
        Option "LTCornerButton"            "3"
        Option "LBCornerButton"            "2"
        Option "CoastingFriction"          "20"
        '';
      };
  };

  services.postfix = {
    enable = true;
    setSendmail = true;
    # Thanks to http://rs20.mine.nu/w/2011/07/gmail-as-relay-host-in-postfix/
    extraConfig = ''
      relayhost=[smtp.gmail.com]:587
      smtp_use_tls=yes
      smtp_tls_CAfile=/etc/ssl/certs/ca-bundle.crt
      smtp_sasl_auth_enable=yes
      smtp_sasl_password_maps=hash:/etc/postfix.local/sasl_passwd
      smtp_sasl_security_options=noanonymous
    '';
  };

  fonts = {
    enableFontDir = true;
    enableCoreFonts = true;
    enableGhostscriptFonts = true;
    extraFonts = with pkgs ; [
      liberation_ttf
      ttf_bitstream_vera
      dejavu_fonts
      terminus_font
      bakoma_ttf
      clearlyU
      cm_unicode
      andagii
      bakoma_ttf
    ];
  };

  environment.systemPackages = with pkgs ; [
    # Basic tools
    psmisc
    iptables
    dhcp
    nmap
    tcpdump
    pmutils
    acpitool
    cpufrequtils
    zip
    unzip
    unrar
    openssl
    cacert
    w3m
    wget
    screen
    fuse
    bashCompletion
    irssi
    mpg321
    catdoc
    graphviz
    tftp_hpa
    unetbootin
    rpm
    acpid

    # X11 apps
    xorg.xdpyinfo
    xorg.xinput
    gitAndTools.gitFull
    subversion
    ctags
    mc
    rxvt_unicode
    vimHugeX
    #chromeWrapper
    firefoxWrapper
    glxinfo
    feh
    xcompmgr
    zathura
    evince
    xneur
    MPlayer
    xlibs.xev
    xfontsel
    xlsfonts
    djvulibre
    ghostscript
    djview4
    #conky
    #dzen2
    #dmenu
    skype_linux
    tightvnc
    thunderbird
    wine
    xfce.xarchiver
    vlc
    easytag
    gqview
    libreoffice
    #gnome_mplayer
    #linuxPackages.virtualbox
    #linuxPackages.virtualboxGuestAdditions
    #impressive
    #pianobooster
    pidgin

    (pkgs.haskellPackages.ghcWithPackages (self : [
        self.haskellPlatform
        self.happstackServer
        self.happstackHamlet
        self.happstackUtil
        self.cabalInstall
    ]))
  ];


  nixpkgs.config = {
    #chrome.enableRealPlayer = true;
    chrome.jre = true;
    #firefox.enableRealPlayer = true;
    firefox.jre = true;
    #subversion.saslSupport = false; #true;
    #freetype.useEncumberedCode = false; # true;

    packageOverrides = pkgs: {
      libass = pkgs.libass.override {
        enca = null;
      };

      xorg = pkgs.xorg // { xf86videointel = pkgs.xorg.xf86videointel_2_17_0; } ;

      devenv = pkgs.myEnvFun {
        name = "dev";
        buildInputs = with pkgs; [
          autoconf
          automake
          gettext
          intltool
          libtool
          pkgconfig
          perl
          curl
          sqlite
          cmake
          qt4
          python
          glew
          mesa
          freetype
          fontconfig
          ncurses
          zlib
          xlibs.xproto
          xlibs.libX11
          xlibs.libXt
          xlibs.libXft
          xlibs.libXext
          xlibs.libSM
          xlibs.libICE
          xlibs.xextproto
          xlibs.libXrender
          xlibs.renderproto
          xlibs.libxkbfile
          xlibs.kbproto
          xlibs.libXrandr
          xlibs.randrproto
        ];
      };
    };
  };

}
