# gtkada_canvas
This is an evolution of the GtkAda Canvas package.
==================================================

The Ada package for canvas that comes with GtkAda is monolithic, menaing that nearly all
concepts are defined in such package.

Instead, we have split the original package in several smaller ones.

In addition, the following improvements have been implemented:
- waypoints do no disappear when entities are moved
- many internal parameters are now visible through functions
- links are now Orthogonal, Straight, Line, (Bezier) Spline and (Orthogonal) Rounded.


Installation
------------
This is an Ada library. Follow the instructions at the end of the gpr.


Usage
-----
Just insert in your gpr file the following: with "gtkada_canvas";


Bug reports
-----------
Please send questions and bug reports to the author. Of course, any help/contributions are
welcome.
