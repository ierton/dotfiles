# Edit this configuration file which defines what would be installed on the
# system.  To Help while choosing option value, you can watch at the manual
# page of configuration.nix or at the last chapter of the manual available
# on the virtual console 8 (Alt+F8).

{config, pkgs, ...}:

{
  require = [
    # Include the configuration for part of your system which have been
    # detected automatically.
    /etc/nixos/hardware-configuration.nix
  ];

  #boot.kernelPackages = pkgs.linuxPackages_3_0;

  boot.blacklistedKernelModules = [ 
    "wimax"
    "i2400m"
    "i2400m_usb"
    ];

  boot.initrd.kernelModules = [
    # Specify all kernel modules that are necessary for mounting the root
    # file system.
    "uhci_hcd" "ehci_hcd" "ahci"
  ];

  boot.kernelModules = [
    "acpi-cpufreq"
  ];

  boot.loader.grub = {
    # Use grub 2 as boot loader.
    enable = true;
    version = 2;
    configurationLimit = 10;

    # Define on which hard drive you want to install Grub.
    device = "/dev/sda";

    extraEntries = ''
      menuentry "Gentoo Linux 3.0.0" {
        linux /boot/vmlinuz-3.0.0-gentoo-hm20
      }
      '';
  };

  # Europe/Moscow
  time.timeZone = "Etc/GMT-4";

  networking = {
    hostName = "pokemon";
    interfaceMonitor.enable = false;
    enableWLAN = false;
    useDHCP = false;
    wicd.enable = true;
  };

  security = {
    sudo.configFile = "root ALL=(ALL) SETENV: ALL\n%wheel ALL=(ALL) SETENV: NOPASSWD: ALL\n";
  };

  fileSystems = [
    { mountPoint = "/";
      device = "/dev/sda7";
    }

    { mountPoint = "/boot";
      device = "/dev/sda2";
    }

    { mountPoint = "/home";
      device = "/dev/sda6";
    }

    { mountPoint = "/mnt/gentoo";
      device = "/dev/sda5";
    }
  ];

  swapDevices = [
    # List swap partitions that are mounted at boot time.
    { device = "/dev/sda1"; }
  ];

  powerManagement = {
    enable = true;
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  services.nixosManual.showManual = false;

  # Add an OpenSSH daemon.
  services.openssh.enable = true;

  # Add CUPS to print documents.
  services.printing.enable = true;

  services.acpid = {
    powerEventCommands = ''
      ${pkgs.upstart}/sbin/poweroff
    '';
    lidEventCommands = ''
      LID="/proc/acpi/button/lid/LID/state"
      state=`cat $LID | ${pkgs.gawk}/bin/awk '{print $2}'`
      case "$state" in
        *open*) ;;
        *close*) ${pkgs.pmutils}/sbin/pm-suspend ;;
        *) logger -t lid-handler "Failed to detect lid state ($state)" ;;
      esac
    '';
  };

  services.ntp = {
    enable = true;
    servers = [ "server.local" "0.pool.ntp.org" "1.pool.ntp.org" "2.pool.ntp.org" ];
  };

  # Add XServer (default if you have used a graphical iso)
  services.xserver = {
    enable = true;
    layout = "us,ru(winkeys)";
    xkbOptions = "eurosign:e, grp:alt_space_toggle, ctrl:swapcaps, grp_led:caps, ctrl:nocaps";
    windowManager.xmonad.enable = true;
    windowManager.default = "xmonad";
    exportConfiguration = true;
    # multitouch.enable = true; Doesn't work
    startOpenSSHAgent = true;
    synaptics = {
      enable = true;
      twoFingerScroll = false;
      additionalOptions = ''
        Option "LBCornerButton" "2"
        Option "LTCornerButton" "3"
        '';
    };

    displayManager = {
      #job.logsXsession = true; Doesn't work
      slim = {
        enable = true;
        defaultUser = "ierton";
      };
    };
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

  environment.pathsToLink = ["/"];

  environment.systemPackages = with pkgs ; [
    gcc gnumake
    pkgconfig autoconf automake m4
    flex bison 
    vimHugeX
    rxvt_unicode
    wget
    screen
    gitAndTools.gitFull
    subversion
    ctags
    mc
    psmisc
    iptables
    nmap
    tcpdump
    pmutils
    haskellPackages.ghc
    haskellPackages.cabalInstall
    acpid
    acpitool
    cpufrequtils
    cmake
    intltool
    gettext
    zip unzip unrar

    #haskellPackages.xmobar
    #haskellPackages.xmonad
    #haskellPackages.xmonadContrib
    #haskellPackages.xmonadExtras

    # GUIs
    chromeWrapper
    glxinfo
    feh
    trayer
    xcompmgr
    zathura
    xneur
    MPlayer
    vimprobable2
    unclutter
    trayer
    xorg.xdpyinfo
    xfontsel
    xlsfonts
    djvulibre
    ghostscript
    djview4
    #evince
    conky #Limited
    dzen2
    hicolor_icon_theme
    oxygen_gtk
    lxappearance
    skype_linux

    # xmonad stuff
    freetype fontconfig xlibs.xproto xlibs.libX11 xlibs.libXt
    xlibs.libXft xlibs.libXext xlibs.libSM xlibs.libICE
    xlibs.xextproto xlibs.libXrender xlibs.renderproto 
    xlibs.libxkbfile xlibs.kbproto xlibs.libXrandr 
    xlibs.randrproto
    glew mesa
  ];

  nixpkgs.config = {
    chrome.enableRealPlayer = true;
    chrome.jre = true;
    subversion.saslSupport = true;
  };
}

# vim: expandtab : tabstop=2 : autoindent :
