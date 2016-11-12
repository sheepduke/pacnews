# pacnews
`pacnews` is a archlinux news reader, like 'eselect news' from Gentoo system.

## Description
Upgrading in archlinux can be dangerous if some important news is missed. Thus this software is created to fetch and read news from archlinux website.

Every time `pacnews` synchronizes with archlinux server, it downloads latest news to local directory. It will **not** synchronize automatically.

This software is implemented in Common Lisp, tested on SBCL.

## Usage
`pacnews <ACTION> [OPTION]`

## Actions

### sync
Synchronize with archlinux server and download all the latest news. Local news file is default to `$HOME/.pacnews/news`. This file is changed only after all the news is fetched. 

Please note that same news will **not** be downloaded twice.

### list [--all]
List local news according to given option.
By default, only _unread_ news is listed.
If `--all` is provided, all the news is listed.

### read [--all | N]
Print the content of corresponding news and mark it as _read_.
By default, only _unread_ news is listed.
If a number _N_ is provided, news with index _N_ will be processed.
If `--all` is given, all the news is listed and marked.

### unread [--all | N]
Mark news as unread.
By default, only _read_ news is marked.
If a number _N_ is given, news with index _N_ will be processed.
If `--all` is provided, all the news is marked.

### help
Print help message.

### version
Print version information.
