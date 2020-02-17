<!--
Please fill out this template fully! Remember that the master branch must 
always be stable. So take great care in documenting and testing your changes. 

For info on how this package uses versioning see https://r-pkgs.org/description.html#version
-->

### description
<!--
Describe or list the changes here.
Comment on any notes that come up during devtools::check().
-->

### system details
* RStudio Version:
* R Version:
* OS Version:

### checklist
<!-- Please check `[x]` the applicable boxes. -->
* [ ] This merge includes bug fixes or changes in the functionality of the package (whether they are user-visible or not).
    * [ ]  update documentation including examples (if necessary)
    * [ ]  update NEWS.md
    * [ ]  update version in DESCRIPTION
    * [ ]  update date in DESCRIPTION
* [ ]  This merge introduces new features.
    * [ ]  new features are documented
    * [ ]  new features have tests
* [ ]  no remaining TODO, FIXME, or debug prints anywhere in the source files
* [ ]  `devtools::document()`
* [ ]  `devtools::check()` without errors or warnings

<!--
If this merge introduces a new version (major, minor or patch) to the 
master branch do these steps after the branch has been successfully merged:
- git tag
- build binary and source package
- gitlab release including the builds
-->
