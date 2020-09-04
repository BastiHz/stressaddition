## Change in the maintainer's email address
I recently learned that my old address (sebastian.henz@ufz.de) is probably 
going to get deactivated in a few months. To ensure long-term maintainability 
of the package I changed the address to one that will remain active for the 
foreseeable future. I have sent a confirmation from the old address to
CRAN-submissions@R-project.org.

## Test environments
* local Windows 10 installation, R 4.0.2
* Ubuntu Linux 16.04 LTS on R-hub, R-release
* Windows Server 2008 R2 SP1 on R-hub, R-devel
* Ubuntu 20.04 LTS on GitLab-CI, R 4.0.2
* Ubuntu 20.04 LTS on GitLab-CI, R devel (2020-08-18 r79041)
* Debian GNU/Linux 10 on GitLab-CI, R 3.6.3
* Debian GNU/Linux 9 on GitLab-CI, R 3.5.1

## R CMD check results
There were no errors or warnings.
One note came up: "unable to verify current time". I believe this is 
not caused by my package but instead by how R checks timestamps.

## Downstream dependencies
There are no downstream dependencies yet.
