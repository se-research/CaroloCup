#####################################################################################

      IDS - Imaging Development Systems GmbH

        uEye Linux SDK installation

#####################################################################################

      (c) 2014, Imaging Development Systems GmbH


------------------
     SUMMARY
------------------

 1 - REQUIREMENTS
 2 - INSTALLED FILE STRUCTURE
 3 - USER/GROUP
 4 - QUICK START
 5 - THE DEMONSTRATION PROGRAM
 6 - EVENT HANDLING UNDER LINUX
 7 - CURRENT API STATUS
 8 - UEYE LINUX DRIVER FAQ
 9 - COMPATIBILITY LIST
10 - KNOWN BUGS/ISSUES
11 - FAQ

-------------------------------------------------------------------------------------
For those who want to proceed immediately: Installation instructions can be found 
at "4 - QUICK START".
-------------------------------------------------------------------------------------

1 - REQUIREMENTS
----------------

SOFTWARE REQUIREMENTS

 - Linux kernel min. 2.6.11 (recommended Linux Kernel min. 3.4 for USB3-Cameras)
 - Linux kernel 3.3 or greater recommended for uEye XS
 - libc/glibc, the standard C Library (min. v2.11)
 - POSIX threads library (POSIX threads enabled libc)
 - bash (Bourne again shell) or sh to run the script
 - libcap v2
 - libgomp v2 (for OpenMP-Support)
 - udev min. v105.
 - libpng (libpng.so or libpng12.so.0) for support of saving images in png format.
 - libjpeg (libjpeg.so.62) for support of saving images in jpeg format and JPEG-Mode of XS.

HARDWARE REQUIREMENTS
 - uEye camera
   - for GigE uEye variant: GigE uEye camera with persistent IP configuration
   - for USB uEye variant: USB uEye camera

OTHER REQUIREMENTS
 - You must be root to install
 - You must be root to control the daemon
 - The cameras MUST have a persistent IP configuration for GigE variant
 - If a firewall is active on your system, ensure that UDP ports 50000 to 50003 are 
    not filtered if you installed the GigE variant
 - It is recommended to use a system-wide, fully static interface configuration for
    the network interfaces that GigE uEye cameras are connected to.
 - If you want to have a graphical environment by using tools like the camera manager
    or the demo application, you need to install the Qt framework (min. v4.5.2)

IMPORTANT NOTES
 - Installation of uEye Linux 4.40 will break USB driver installations prior to 4.40.

-------------------------------------------------------------------------------------
2 - INSTALLED FILE STRUCTURE
-------------------------------------------------------------------------------------

The created files will be installed in following directories (the presence of the 
binaries depends on whether you have installed the USB or GigE package):

/usr/lib/libueye_api.so.4.40        32bit uEye shared library *
/usr/lib/libueye_api64.so.4.40      64bit uEye shared library *
/usr/include/ueye.h                 Development header file +
/usr/local/share/ueye/ueyeethd      GigE uEye daemon binaries and configuration
/usr/local/share/ueye/ueyeusbd      USB uEye daemon binaries and configuration
/usr/local/share/ueye/bin           uEye utilities
/var/run/ueyed                      Runtime directory
/etc/init.d/ueyeethdrc              GigE uEye daemon runlevel control script
/etc/init.d/ueyeusbdrc              USB uEye daemon runlevel control script
/usr/src/ids                        IDS demo sources
/usr/share/doc/ids                  IDS SDK readme.txt
/usr/share/doc/ids/ueye_manual      IDS SDK user manual

*: after ldconfig has finished, a symbolic link libueye_api.so should exist to provide 
   linkage against libueye_api.so. Note that only the version for the current target 
   architecture will be installed, e.g. the 32bit library will only be installed on a 
   32bit system and the 64bit library will only be installed on a 64bit system.
+: for compatibility with older versions, a symbolic link /usr/include/uEye.h will be 
   created. It is strongly recommended to use the lower case file name in new projects.

Additionally, the uEye SDK installer provides the following tools - unless not indicated 
otherwise, the tools will be installed to '/usr/local/share/ueye/bin':

ueyesetid                           Camera ID configuration tool
ueyesetip                           Camera IP address configuration tool
setSEstarter                        GigE uEye SE starter firmware upload tool
ueyecameramanager                   camera manager
ueyedemo                            demo application
report.sh                           Report generator tool

For the binary tools except setSEstarter, a symbolic link will be created in /usr/bin 
to provide direct command line access.

-------------------------------------------------------------------------------------
3 - USER/GROUP
-------------------------------------------------------------------------------------

The installer will create a system group 'ueye' and a system user 'ueyed' to run the
daemon with. Currently there are no access control restrictions, but be aware that
this might change in the future.

-------------------------------------------------------------------------------------
4 - QUICK START
-------------------------------------------------------------------------------------

1 - copy all the files into a directory on the hard disk
    (you need write access to decompress). For the following 
    steps, you need to be root!

2 - go to the directory you copied the files into and
    run the setup program script by typing (replace the
    wildcard with the actual version identifier of the 
    installer file)
    $> sh ./ueyesdk-setup*.run

3 - if any problems occurred, the common problems section
    below or the generated report.log file may include a
    hint to solve them (see installer output).
    If not, contact your local distributors support and
    submit the report.log file (see installer output).

    Note regarding GigE uEye: By default, the installer configures any 
    ethernet network interface named eth* - if you use another network 
    interface to connect your cameras, insert the interface name at the
    interface configuration parameter in "[Parameters]-Interfaces"
    of /usr/local/share/ueye/ueyeethd/ueyeethd.conf. You must be root to
    edit the configuration file.
    As of Camera Manager version 1.5.2, the GigE uEye daemon network interfaces may
    be configured with the Camera Manager if a graphical environment is available.
    NOTE: ueyeethd must be stopped BEFORE manually editing the configuration file!

4 - if installation succeeded, one may start the uEye daemon by typing
    '/etc/init.d/ueyeethdrc start' (you need to be root)
    To stop the uEye daemon, type
    '/etc/init.d/ueyeethdrc stop' (you need to be root)
    For USB uEye, replace 'ueyeethdrc' with 'ueyeusbdrc'.
    Alternatively, if one has a working graphical environment, the daemons may be
    controlled via the uEye Camera Manager (started as root)

5 - use GigE uEye SE with ueyeethd
    If the GigE uEye SE firmware version does not match the driver 
    version, a firmware upgrade might be required.
    
    The uEye Camera Manager provides firmware upgrade capability. Simply select the 
    camera in the camera list and click "Upload starter firmware". Select the proper
    firmware file (if you want to use the firmware files delivered with the driver,
    navigate to /usr/local/share/ueye/fw if required) and click "Open".

    The firmware upgrade may take a while.

    If you do not have a graphical environment with Qt installed and want to upgrade 
    the firmware from a console, follow these steps:
    - find the device ID by running 'ueyesetid -d' 
    - As root, type 
      '/usr/local/share/ueye/bin/setSEstarter <id> /usr/local/share/ueye/fw/<SE_RE_V_VV_VV.fw>'
      and replace <id> with the camera's device ID (as printed in column 'Device ID') and <SE_RE_V_VV_VV.fw>
      with the name of the SE_RE_*.fw file located in directory /usr/local/share/ueye/fw/.

    DO NOT TRY TO USE ANOTHER FIRMWARE THAN THE ONE SPECIFIED ABOVE!

6 - set camera ID
    To set the camera ID, one may use the Camera Manager or the 'ueyesetid' tool.

7 - set camera IP
    To set the camera persistent IP address, one may use the Camera Manager or
    the 'ueyesetip' tool.  

8 - to uninstall ueyeethd, run (as root)
    '/usr/local/share/ueye/bin/ueyed_install-eth uninstall'
    to uninstall ueyeusbd, run (as root)
    '/usr/local/share/ueye/bin/ueyed_install-usb uninstall'
    
    NOTE: Once uninstalled, any configuration file will be lost. Consider backing up 
    the respective configuration files!
    
9 - if the uEye daemon hangs and can not be stopped with '/etc/init.d/ueyeethdrc stop', 
    run (as root)
    '/etc/init.d/ueyeethdrc force-stop'
    Note that it is normal behaviour that ueyeethd refuses to terminate if there are 
    applications connected. For USB uEye, replace 'ueyeethdrc' with 'ueyeusbdrc'.

-------------------------------------------------------------------------------------
5 - THE DEMONSTRATION PROGRAM
-------------------------------------------------------------------------------------

The demonstration program shows how to use the uEye API in order to make following 
actions:
 - Board detection and initialization
 - Setup memory for capturing
 - Setup the camera video mode
 - Modify the image parameters
 - Make snapshots
 - Use Events
 - Display the video in a QT based application

The demo application sources were installed to /usr/src/ids/ueyedemo - refer
to its README.TXT for detailed information.

-------------------------------------------------------------------------------------
6 - EVENT HANDLING UNDER LINUX
-------------------------------------------------------------------------------------

Contrary to the Windows event implementation where event handles may be registered
directly with the library, Linux does not use event handle registration. Events 
may be waited on via calls to is_WaitEvent() with the respective event name given 
as parameter. Handle arguments may be NULL. To use an event, it must be initialized 
with is_InitEvent(). If the event is no longer needed, it must be deinitialized with 
is_ExitEvent().

-------------------------------------------------------------------------------------
7 - CURRENT API STATUS
-------------------------------------------------------------------------------------

The current API version shipped with this setup package is 4.40.0017.
The functional range is described in the SDK documentation and is much the same as in 
the MS Windows version with the following exceptions:

7.1 - Functions currently not available for Linux
-------------------------------------------------

There are a few functions (mainly gui related ones) that are not accessible in the 
Linux version of this uEye SDK release. These are in detail:

User Interface: (currently not supported)
---------------
  IDSEXP   is_RenderBitmap           (HIDS hf, INT nMemID, HWND hwnd, INT nMode);
  IDSEXP   is_GetDC                  (HIDS hf, HDC* phDC);
  IDSEXP   is_ReleaseDC              (HIDS hf, HDC hDC);
  IDSEXP   is_UpdateDisplay          (HIDS hf);
  IDSEXP   is_SetDisplayMode         (HIDS hf, INT Mode);
  IDSEXP   is_SetDisplayPos          (HIDS hf, INT x, INT y);
  IDSEXP   is_SetHwnd                (HIDS hf, HWND hwnd);
  IDSEXP   is_SetUpdateMode          (HIDS hf, INT mode);
  IDSEXP   is_GetColorDepth          (HIDS hf, INT* pnCol, INT* pnColMode);
  IDSEXP   is_SetOptimalCameraTiming (HIDS hf, INT Mode, INT TimeoutFineTuning, INT *PClk, double *Framerate);

DirectDraw: (not supported)

-------------------------------------------------------------------------------------
8 - UEYE LINUX DRIVER FAQ
-------------------------------------------------------------------------------------

8.1 - Camera open issues
------------------------

Q: I've successfully installed the uEye driver but can neither open a camera with 
   the ueye demonstration app, nor do I see cameras in the graphical camera manager.
A: Make sure the ueye daemon is running. To start it, run (as root):
    /etc/init.d/ueyeethdrc start
   AND/OR
    /etc/init.d/ueyeusbdrc start

Q: I get an error from is_InitCamera if I try to reopen a device that I just closed
   by calling is_ExitCamera.
A: In the current implementation is_ExitCamera returns before the internal close device 
   procedure completes. is_InitCamera will fail if you call it on the same device before 
   the device is completely closed by the library.


8.2 - No camera shown in camera list
------------------------------------
Q: I have successfully installed the GigE uEye Linux SDK. The demo application built 
   successfully but I can't open a camera although one is connected and has a 
   persistent IP address. The camera list of either camera manager or uEye demo don't 
   show GigE uEye cameras. 
A: Ensure that any firewall application installed on your system is either turned off 
   for your camera interface, or that UDP ports 50000 to 50003 are not filtered by 
   the firewall. This issue is currently known for openSuSE and Fedora distributions. 

Q: The daemon is running but shows no cameras although they're connected!
A1: Check if you have given the interfaces the daemon shall listen on via the 
    appropriate configuration parameter - check [Parameters]-Interfaces in
    /usr/local/share/ueye/ueyeethd/ueyeethd.conf. Do not forget to stop the daemon 
    before and start it again after having applied changes to the configuration file. 
    It is recommended to use the graphical Camera Manager if a graphical desktop 
    is available.

A2: Check if you are using a network manager application which activates network 
    interfaces after user login. As the ueyeethd is started before a graphical desktop 
    environment, it possibly didn't find all its network interfaces configured properly. 
    Simply stop the daemon after network activation and start it again, then the cameras 
    should appear in the camera list. Note that we strongly recommend to use a fully 
    static network configuration for the interfaces with uEye cameras connected to. 


8.3 - Information required by IDS support
-----------------------------------------

Q: What information can I supply if I contact the vendor support?
A: Please run (as root) 
    /usr/local/share/ueye/bin/report.sh 
   and send the output to support@ids-imaging.de. If there is no output visible (which
   may occur on certain distributions), please write the output to a file and send it
   afterwards:
    /usr/local/share/ueye/bin/report.sh -o /tmp/ueye_report.log


8.4 - 64bit support
-------------------

Q: What about 64bit support?
A: There are two different installers providing 32bit and 64bit installation. Check the 
   download section!


8.5 - Uninstall the SDK
-----------------------

Q: How can I uninstall the software?
A: To uninstall the GigE uEye daemon, run (as root): 
    /usr/local/share/ueye/bin/ueyed_install-eth uninstall
   To uninstall the USB uEye daemon, run (as root):
    /usr/local/share/ueye/bin/ueyed_install-usb uninstall
   You should always use the install script that came with the previous installation to 
   ensure proper system cleanup. Do not attempt to remove the uEye SDK manually.


8.6 - Installation failure
--------------------------

Q: I use Gentoo Linux and the installer fails when creating the uEye user/group. What 
   can I do?
A: This is a known issue, as Gentoo seems to have a symbolic link 'adduser' pointing to 
   'useradd' - the installer will attempt to use incompatible command line arguments.
   Run the installer with the --keep option (as root). This will prevent the installer 
   from deleting the installation files, which will be kept in the subdirectory 
   'ueyed-setup' of the current working directory. One may edit the installer script 
   'ueyed_install' in that subdirectory, correct the useradd section and run the 
   installation manually by executing './ueyed_install install'.

Q: I have an embedded Linux platform like gumstix which has tinylogin installed.
   The uEye installer fails when it tries to run groupadd, useradd etc. What can I do?
A: Proceed as follows:
     1. Unpack the .tar archive at /. Do not execute the installer as described in the
        setup guide.
     2. Make sure the following files are owned by root
          a) /usr/local/share/ueye
          b) /var/run/ueyed
     3. Make sure /etc/init.d/ueye*rc is executable.
     4. To automatically start the uEye drivers add the runlevel control scripts to the
        runlevel control sequence configuration.
   Alternatively, edit the installer script to suit your needs. See the previous Q/A.
   

8.7 - daemon stop issues
-------------------------

Q: I'm running SuSE Linux and cannot stop a uEye daemon via 
    YaST -> System -> Runlevel control applet because
   of insufficient user permissions.
A: The uEye daemon rejects termination requests if there are clients connected. Check 
   if you have a camera manager or another application running which uses libueye_api.so. 
   If you know what you are doing, you may run (as root) 
    '/etc/init.d/ueyeethdrc force-stop' (or ueyeusbdrc, respectively, for USB variant)


8.8 - 32bit compatibility mode on 64bit Linux
----------------------------------------------

Q: I'm running Ubuntu 9.10 'Karmic Koala' 64bit Linux and want to use my legacy 32bit 
   application with the uEye Linux SDK on 64bit Linux but my application complains 
   about missing 32bit libueye_api.so upon startup. How can I fix this?
A: Currently, there is no support for 32bit compatibility mode on 64bit Linux, therefore 
   old applications have to be rebuilt for use on 64bit systems.


8.9 - Daemon and library configuration
---------------------------------------

Q: How can I retain my current daemon and library configuration when uninstalling and 
   reinstalling the uEye Linux SDK?
A: The GigE uEye daemon (and USB uEye daemon, respectively) store persistent 
   configuration in a configuration file located at 
   /usr/local/share/ueye/ueyeethd/ueyeethd.conf (and .../ueyeusbd/ueyeusbd.conf, respectively). 
   The uEye library stores persistent configuration in two configuration files located at
   /usr/local/share/ueye/libueye_api/machine.conf and at
   /home/USER/.ueyeconf/libueye_api/user.conf.
   The library's user.conf file is not necessarily created. If present it is not automatically
   removed by the uninstaller.
   To retain the configuration across an uninstall/install cycle, create a backup copy 
   of these files at a safe place (e.g. your home directory or your desktop directory). 
   After successfully re-installing the daemon, copy the file back to the previous 
   location, but note that the respective daemon _must_ be stopped before.
 

8.10 - Older SDK installations prior to 3.50
--------------------------------------------

Q: I have an older SDK version installed on my Linux system. How can I remove it?
A: The latest kernel driver version has an uninstall script that can be called with
   sh /usr/bin/ueye_usb_uninstall.sh. If the script is not available, perform the 
   following steps (as root):
   - terminate any application using uEye cameras
   - unplug any uEye camera
   - rm -Rf /usr/src/ids /usr/share/doc/ueye
   - rmmod ueye_usb-driver
   - rm /lib/modules/$(uname -r)/kernel/drivers/media/video/ueye_usb-driver.ko
   - depmod
   - rm -f /etc/udev/rules.d/20-ueye.rules*
   - rm -f /bin/ueye_demo
   - rm -f /bin/ueyesetid
   - rm -Rf /usr/src/ids/ueye
   - rm -Rf /usr/share/doc/ueye
   - rm -f /usr/lib/libueye_api.so.*
   - ldconfig
   
   Check if the driver is really uninstalled by plugging a uEye camera. If the 
   camera LED does not switch to green after a few seconds and you do not see
   any uEye related message in your system log, the camera was successfully removed.
   Note that the commands shown above must be used carefully!

8.11 - Video for Linux
----------------------

Q: When working with uEye cameras and Linux, can one work with the cameras via 
   /dev/video0?
A: /dev/video* files are usually V4L or V4L2 device files. The uEye SDK neither 
   supports V4L nor V4L2.


8.12 - Cameramanager Icon  not shown in systray
----------------------

Q: If I press "Hide to Tray" in the cameramanager then the Systray don't display the Icon.
A: You use Ubuntu in a version newer or equal to 11.04. They use Unity as displaysystem and you must
   edit a whitelist to display the trayicon in the systray. To do this run the following command on
   an virt. Console (e.g. gnome-terminal).

   To insert the Icon to the systray:
   gsettings set com.canonical.Unity.Panel systray-whitelist "$(gsettings get com.canonical.Unity.Panel systray-whitelist | sed -e "s/]/, 'ueyecameramanager']/g")"
  
   To remove the Icon from the systray:
   gsettings set com.canonical.Unity.Panel systray-whitelist "$(gsettings get com.canonical.Unity.Panel systray-whitelist | sed -e "s/, 'ueyecameramanager'//g")"
   
   After this you must restart Unity by pressing ALT + F2 and then run the command unity --replace 
   or restart the System.


to be continued...

-------------------------------------------------------------------------------------
9 - COMPATIBILITY LIST
-------------------------------------------------------------------------------------

The Linux operation system is one of the most widely ported, running on a huge amount 
of architectures and provided over many distributions. Although we are trying to be 
compatible among the whole range of kernel versions and distributions, there is no 
guarantee that the uEye driver is working on a specific untested combination.

Below is a list of tested and succeeded platforms and distributions for the CURRENT SDK release:
    Distribution            Architecture /Processor     uEye setup
    /etc/<distr>-release    'uname -i' / 'uname -m'           
    Debian 6.0.2.1          i386 / i686                 ueyesdk-setup-<version>-i686.gz.run             
    Debian 6.0.2.1          amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run              
    Debian 7                i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Debian 7                amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run
    Ubuntu 12.04 LTS        i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 12.04 LTS        amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run    
    Ubuntu 13.04            i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 13.04            amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run

    NOTE USB3: These distributions were tested with a Kernel version >= 3.2.
               However we recommend for a better USB3 support a kernel version >= 3.4.

Below is a list of tested and succeeded platforms and distributions for FORMER SDK versions:
    Distribution            Architecture /Processor     uEye setup
    /etc/<distr>-release    'uname -i' / 'uname -m'
    Debian 4.0              unknown / i686              ueyesdk-setup-<version>-i686-oldlibc-lipcap1.gz.run
    Debian 5.0              i386 / i686                 ueyesdk-setup-<version>-i686.gz.run             
    Debian 5.0              amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run  
    Fedora 12               i386 / i686                 ueyesdk-setup-<version>-i686.gz.run             
    OpenSuSE 11.0           i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 8.10             unknown / i686              ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 9.04             unknown / i686              ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 9.10             unknown / i686              ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 9.10 64bit       unknown / amd64             ueyesdk-setup-<version>-amd64.gz.run
    Ubuntu 10.04.3 LTS      i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 10.04.3 LTS      amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run    
    Ubuntu 11.04            i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 11.04            amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run
    Ubuntu 11.10            i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 11.10            amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run
    Ubuntu 12.10            i386 / i686                 ueyesdk-setup-<version>-i686.gz.run
    Ubuntu 12.10            amd64 / amd64               ueyesdk-setup-<version>-amd64.gz.run

    NOTE: These distributions are not supported by IDS for the current SDK release

-------------------------------------------------------------------------------------
10 - KNOWN BUGS/ISSUES
-------------------------------------------------------------------------------------

Please consider this section before calling for support as it lists known bugs and 
anomalies that might occur in this release. 

- Firewall blocks proper ueyeethd operation: Some distributions (e.g. openSuSE, Fedora) 
  have a firewall installed by default. This firewall blocks GigE uEye messages required 
  for device discovery. Ensure that the firewall won't block UDP ports 50000 to 50003 
  for proper ueyeethd operation.

- Energy saving issues: On some systems with activated energy saving, the daemons may 
  hang once the system has entered an energy saving state.
  
- GigE uEye interface control: Network interface deactivation/activation may lead to a 
  segfault when performed while a camera is opened.
  
- USB3 issues: Sometimes the USB-Daemon cannot receive Data from the USB-Camera which is
  connected on a USB3-Port. After that occur, unlink the USB-Camera and reconnect it.
