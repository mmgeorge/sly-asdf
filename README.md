## Sly-asdf

SLY contrib that adds support for editing ASDF systems. Ported from [slime](https://github.com/slime/slime/blob/master/contrib/slime-asdf.el) with minor code changes to add better support for `package-inferred-system`. 

## Installation 
Can be installed from MELPA or manually. 

Installing manually will require the contrib be registered in SLY's `sly-contrib` variable. 
```
(add-to-list 'sly-contribs 'sly-asdf 'append)
```
