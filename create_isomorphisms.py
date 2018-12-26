import re
import subprocess
import sys

cmd = """grep -r  -o -E -h '(([A-Za-z]+\.)*([A-Z]\w+))(\.\w+)?' {}  --include='*java'"""

template = """
Expression
@ {} @
@@
{} => {}
"""


if len(sys.argv) > 1:
    path = sys.argv[1]
else:
    path = '.'

output = subprocess.check_output(cmd.format(path), shell = True)
parsed = output.decode('ascii').split('\n')
filtered = [qualified_identifier for qualified_identifier in parsed if len(qualified_identifier.split('.')) > 1]

for qualified_identifier in filtered:
    ident = qualified_identifier.split('.')[-1]
    iso_name = '_'.join(qualified_identifier.split('.'))
    iso = template.format(iso_name, ident, qualified_identifier)

    print(iso)
    
