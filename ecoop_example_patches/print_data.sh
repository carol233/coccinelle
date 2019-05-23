echo "Number of files"
grep -ce "diff" ./*/project.patch
echo "Number of additions and deletions"
grep -ce "^[-+]\s" ./*/project.patch
echo "Size of Semantic Patches"
grep -cve "^\s*$" ./*/*.cocci
