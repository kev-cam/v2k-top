<html>
#include "color-htm.c"

<h1>Scripting</h1>

Most scripting is done in csh or perl. Csh is used mostly as a wrapper script for launching executables after identifying the platform and setting up dynamic loading paths - I prefer csh to [ba]sh because it handles paths with whitespace better. Perl is used for most data/text processing.
<p>
V2k has a built in CSH interpreter, use "v2k -shell" to invoke it (and type "help" for more information). The "set" command can be used to set or show v2k specific values. "v2k" and built-in commands are interpreted directly by the shell, other commands will run in seperate processes.
<p>
You can use commands like "xterm -e gdb $0 $$ &" to launch a debugger from the CSH interpreter which will attach to the running program (this usually works better than trying to start v2k under the debugger when using dynamic libraries).

