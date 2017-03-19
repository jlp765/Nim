==============================================
  Embedded Nim Debugger (ENDB) User Guide
==============================================

:Author: Andreas Rumpf
:Version: |nimversion|

.. contents::

**WARNING**: ENDB is not maintained anymore! Please help if you're interested
in this tool.

Nim comes with a platform independent debugger -
the Embedded Nim Debugger (ENDB). The debugger is
*embedded* into your executable if it has been
compiled with the ``--debugger:on`` command line option (for
C or C++ code generation).

*Endb* is NOT a low level debugger like gdb.
It cannot display and evaluate arbitrary memory locations.  It is a
high-level debugger, and knows about the variables (and their type
information that have been defined in the Nim source code), and the Nim
compiler registers Nim variables with Endb.
Endb then uses the compiler-supplied information to display the
values of variables as the code is debugged.


Example compilation command:

``nim c -r --debugger:endb myfile.nim``

or

``nim cpp -r --debugger:endb myfile.nim``

This also defines the conditional symbol ``ENDB`` for you.

Note: You must not compile your program with the ``--app:gui``
command line option, because then there would be no console
available for the debugger.

If you start your program the debugger will immediately show
the prompt
``endb| >>``
on the console . You can now enter a command.
The next sections deal with the possible commands.
As usual with all commands in Nim, underscores and case do not matter.
In this document, optional components of a command are listed
in brackets ``[...]``.

Currently, the print command ``p`` and ``o`` are not expression parsers,
but enables the display of the representation of a variable (eg, the
whole of an array, sequence or object, but does not allow referencing
elements of a complex variable).
However, by placing a **watchpoint pragma** in the source code, displaying
a specific element of a complex variable is possible (like displaying
the fifth element of an array, rather than the whole array).


General Commands
================

``h``, ``help``
    Display a quick reference of the possible commands.

``q``, ``quit``
    Quit the debugger and the program.

<ENTER>
    (Without any typed command) repeat the previous debugger command.
    If there is no previous command, ``step_into`` is assumed.

Execution Commands
==================

``s``, ``step``
    Single step, stepping into routine calls.

``ss``, ``sys``
    System step, stepping into lib files (but not lib/system/ files)
    ``ss`` to keep at system level; ``f``, ``s`` or ``n`` to
    return to the upper level.

``n``, ``next``
    Single step, without stepping into routine calls.

``f``, ``skipcurrent``
    Continue execution until the current routine finishes.

``c``, ``continue``, ``r``, ``run``
    Continue execution until the next breakpoint.

``i``, ``ignore``
    Continue execution, ignore all breakpoints. This effectively quits
    the debugger and runs the program until it finishes.

Breakpoint Commands
===================

``b``, ``break`` [fromline [toline]] [file]
    Set a new breakpoint for the given file
    and line numbers. If no file is given, the current execution point's
    filename is used. If the filename has no extension, ``.nim`` is
    appended for your convenience.
    If no line numbers are given, the current execution point's
    line is used. If both ``fromline`` and ``toline`` are given the
    breakpoint contains a line number range. Some examples if it is still
    unclear:

    * ``b 12 15 thallo`` creates a breakpoint that
      will be triggered if the instruction pointer reaches one of the
      lines 12-15 in the file ``thallo.nim``.
    * ``b 12 thallo`` creates a breakpoint that
      will be triggered if the instruction pointer reaches the
      line 12 in the file ``thallo.nim``.
    * ``b 12`` creates a breakpoint that
      will be triggered if the instruction pointer reaches the
      line 12 in the current file.
    * ``b`` creates a breakpoint that
      will be triggered if the instruction pointer reaches the
      current line in the current file again.

``bp``, ``breakpoints``
    Display the entire breakpoint list.

``t``, ``toggle``  line_number [file]
    Enable/Disable a breakpoint at ``line number``. It remains disabled
    until you turn it on again with the ``enable`` command.
    The optional ``file`` parameter sets the breakpoint in another
    source file (one of the import modules of the file being debugged)

Often it happens when debugging that you keep retyping the breakpoints again
and again because they are lost when you restart your program. This is not
necessary: A special pragma has been defined for this:


The ``breakpoint`` pragma
-------------------------

The ``breakpoint`` pragma is syntactically a statement. It can be used
to mark the *following line* as a breakpoint:

.. code-block:: Nim
  echo "1"
  {.breakpoint: "before_echo_2".}
  echo "2"

which will cause the debugger to stop showing the line

.. code-block:: Nim
  endb| >> c
  endb| myfile.nim(2) myfile -> {.breakpoint: "before_echo_2".}

The name of the breakpoint here is ``before_echo_2``. Of course the
breakpoint's name is optional - the compiler will generate one for you
if you leave it out.

Code for the ``breakpoint`` pragma is only generated if the debugger
is turned on, so you don't need to remove it from your source code after
debugging.


The ``watchpoint`` pragma
-------------------------

The ``watchpoint`` pragma is syntactically a statement. It can be used
to mark a location as a watchpoint, that will display changes in a watched
variable to the console as the debugger continues to run (``c`` command),
or as you step through code.

For example:

.. code-block:: Nim
  var a: array [0..20, int]

  for i in 0 .. 20:
    {.watchpoint: a[3].}
    a[i] = i

and will display on the console:

.. code-block:: Nim
  endb| >> c
  endb| WATCHPOINT(4) tst2.a[3]: int = 0
  endb| WATCHPOINT(4) tst2.a[3]: int = 3

ENDB then writes a stack trace whenever the content of the location ``a[3]``
changes. The current implementation only tracks a hash value of the location's
contents and so locations that are not word sized may encounter false
negatives in very rare cases.

WATCHPOINT(4) is a watchpoint on line 4 of the source code.
If you are watching the same variable at may locations, the line number of each
watchpoint is easily identified.

Note: the position of the watchpoint in this example needs to be within the scope of
the loop.  Placing the watchpoint before the `for` loop, would not trigger the
watchpoint during looping.

Code for the ``watchpoint`` pragma is only generated if the debugger
is turned on, so you don't need to remove it from your source code after
debugging.

Data Display Commands
=====================

Variables are displayed three ways, depending on whether the variable
- type information is available,
- has been initialized, and
- expanding of the variable is enabled (display the variable's
value)

The display of information is in the form:

``endb| filename(lineNr):     varname: typename = value``


``e``, ``expand``
    Toggle displaying data values (for local, global, stack frames).
    By default, Expand is ``off``.  Usually, the display of values
    for a large sequence or array will be superfluous.

``g``, ``globals`` ``[file]``
    Display [optionally to file] all the global variables that are
    available for inspection.

``l``, ``locals`` ``[file]``
    Display [optionally to file] the available local variables in
    the current stack frame.
    If the variable has been registered
    before the current execution point, then type information is
    displayed.  If exppansion is enabled, the representation of the
    value of the variable is displayed.

``v``, ``variables`` ``[file]``
    Display [optionally to file] variables that have been registered
    with the debugger (defined) in getting to the current execution point.

``p``, ``print`` <exp>
    Print the expression <exp>. Note that ENDB has no full-blown expression
    evaluator built-in. So expressions are limited:

    * To display global variables prefix their names with their
      owning module: ``nim1.globalVar``
    * To display local variables or parameters just type in
      their name: ``localVar``. If you want to inspect variables that are not
      in the current stack frame, use the ``up`` or ``down`` command.

    Unfortunately, only inspecting variables is possible at the moment. Maybe
    a future version will implement a full-blown Nim expression evaluator,
    but this is not easy to do and would bloat the debugger's code.

    Since displaying the whole data structures is often not needed and
    painfully slow, the debugger uses a *maximal display depth* concept for
    displaying.

    You can alter the maximal display depth with the ``maxdisplay``
    command.

``md``, ``maxdisplay`` <natural>
    Sets the maximal display depth to the given integer value in the range 0 to 9.
    A value of 0 means there is no maximal display depth. Default is 4.
    This helps to limit displaying the representation of nested objects.
    When the ``md`` is reached, [...] will be displayed in place of further
    elements of an complex object.

``o``, ``out`` file <exp>
    Evaluate the expression <exp> and store its string representation into a
    file (the name of the file including extension). If the file does not exist,
    it will be created, otherwise it will be opened for appending.

``w``, ``where``
    Display the current execution point.

``bt``, ``backtrace``
    Display the entire call stack (but not its content).
    The call stack is the nested calling of procs from other procs.
    Endb is not able to change the current execution point.  While
    you can change what is displayed of the call stack, the next
    line of code to be run will remain unchanged.

``u``, ``up``
    Go up in the call stack (display earlier procs)

``d``, ``down``
    Go down in the call stack.



Known Issues
============

Code that sits at the global level (not within a proc) is not contained in a
normal stack frame.  This mainly impacts variables, which appear as global
variables, not as local variables.

It is possible to inspect a variable using Endb BEFORE it has been initialized.
For string types (et. al.), this can result in
``SIGSEGV: Illegal storage access. (Attempt to read from nil?)``
failure, causing the debugger to crash.

The compiler registers with Endb the line number where a variable is defined,
and Endb displays ``<undefined>`` as the value of a variable, if the variable
is inspected before it has been initialized (before that line number has been
reached).

However,  if you come across an instance where inspecting an uninitialized
variable crashes Endb, then please report this as a bug, and provide a
minimal Nim file for testing.


The internals of Endb
=====================

If you are wanting to make imporvements to Endb, then the following will be
helpful for understanding how Endb works.

- view the files compiled with ``--debugger:endb``
 nimcache\myfile.c    (myfile.nim compiled to a C file )
 nimcache\myfile.cpp  (same file compiled to a C++ file)

 and look for the procs
  - ``endb()``,
  - ``dbgRegisterWatchpoint()``,
  - ``dbgRegisterVariable()``,
  - ``dbgRegisterGlobal()``,
  - ``dbgRegisterBreakpoint()``,
  - ``dbgRegisterFilename()``

 These callbacks to Endb provide the information that Endb uses to display
 meaningful information about the source code as the debugger is run.

- view the Endb source files
 - lib/system/endb.nim
 - lib/system/debugger.nim
 - lib/system/repr.nim

and note the corresponding compiler procs

- view the compiler source files (do you really want to do this?)
 - compiler/cgen.nim
 - compiler/ccgstmts.nim

 and definitions in
 - compiler/ast.nim
 - compiler/cgendata.nim
 - compiler/ccgtypes.nim

looking for things like ``#dbgRegister`` and ``#endb``.
