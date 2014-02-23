#!/bin/bash
grep "[}{_]\|define" /usr/include/rpm/rpmtag.h |\
 sed 's:/\*.*\*/::g' |\
 grep -E '\}|[0-9]|\{|=' |\
 sed -e 's/typedef enum \([^ ]*\)/get_\1() -> [/;' \
-e ' s/}.*/}}/;' \
-e ' s/^ /{/;' \
-e ' s/,/},/;' \
-e ' s/=/,/' | \
sed 's/}}/}]/' #| sed 's/+/ + /; s/\([^ ]\)}/\1 }/' 
