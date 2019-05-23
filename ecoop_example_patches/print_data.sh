grep -ce "diff" ./*/project.patch
grep -cve "^\s*$" ./*/*.cocci
grep -ce "^[-+]\s" ./*/project.patch
