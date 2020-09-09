This is a bugfix update. It fixes some failing tests on macos and changes the maintainer's email address. I recently learned that my old address (sebastian.henz@ufz.de) is going to get deactivated in a few months. To ensure long-term maintainability of the package I changed the address to one that will remain active for the foreseeable future (many years). I have sent a confirmation from the old address to cran-submissions@R-project.org.

## Test environments
* local Windows 10 installation, R 4.0.2
* macOS 10.13.6 High Sierra on R-hub, R 4.0.2
* Ubuntu Linux 16.04 LTS on R-hub, R 3.6.1
* Windows Server 2008 R2 SP1 on R-hub, R-devel
* Fedora Linux on R-hub, R-devel
* Ubuntu 20.04 LTS on GitLab-CI, R 4.0.2
* Ubuntu 20.04 LTS on GitLab-CI, R-devel
* Debian GNU/Linux 10 on GitLab-CI, R 3.6.3
* Debian GNU/Linux 9 on GitLab-CI, R 3.5.1

## R CMD check results
There were no errors or warnings.
There were 3 notes:

* checking for future file timestamps ... NOTE
  unable to verify current time

I believe this is not caused by my package but instead by how R checks 
timestamps, which seems to be broken at the moment.

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Sebastian Henz <bastihz.dev@posteo.de>'

I changed the maintainer e-mail address, see comment above.

* checking CRAN incoming feasibility ... NOTE
  ...
  Message: libcurl error code 35:
    Maintainer: 'Sebastian Henz <bastihz.dev@posteo.de>'
         	    schannel: next InitializeSecurityContext failed: 
         	    SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs 
         	    when a fatal SSL/TLS alert is received (e.g. handshake failed).

This note is from R-hub for Windows Server 2008. I don't know what to do about that because the new email address is definitely valid and working.

## Downstream dependencies
There are no downstream dependencies yet.
