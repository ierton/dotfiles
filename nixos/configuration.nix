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

  # boot.kernelPackages = pkgs.linuxPackages_3_0;

  boot.blacklistedKernelModules = [ "wimax" "i2400m" "i2400m_usb" ];

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

  # Add an OpenSSH daemon.
  services.openssh.enable = true;

  # Add CUPS to print documents.
  services.printing.enable = true;

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
      twoFingerScroll = true;
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
      terminus_font
      dejavu_fonts
      bakoma_ttf
      clearlyU
      cm_unicode
      andagii
    ];
  };

  services.nixosManual.showManual = false;

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
    ctags
    mc
    psmisc
    iptables
    nmap
    tcpdump
    haskellPackages.ghc
    haskellPackages.cabalInstall

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

    # xmonad stuff
    freetype fontconfig xlibs.xproto xlibs.libX11 xlibs.libXt
    xlibs.libXft xlibs.libXext xlibs.libSM xlibs.libICE
    xlibs.xextproto xlibs.libXrender xlibs.renderproto 
    xlibs.libxkbfile xlibs.kbproto
  ];

  nixpkgs.config = {
    chrome.enableRealPlayer = true;
  };
}

# vim: expandtab : tabstop=2 :
