------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
-- Modified by JLF (fdesp87@gmail.com) to separate the various concepts.    --
-- See gtkada.canvas_view                                                   --
------------------------------------------------------------------------------

with Glib;    use Glib;

package Gtkada.Style_Pkg.Cubic_Bezier is

   function Bezier_Cubic (T : Gdouble;
                          P0, P1, P2, P3 : Point) return Point;
   --  Bezier Cubic Polynmial

   function Dt_Bezier_Cubic (T : Gdouble;
                             P0, P1, P2, P3 : Point) return Point;
   --  First Derivative of Bezier Cubic Polynomial

   function D2t_Bezier_Cubic (T : Gdouble;
                              P0, P1, P2, P3 : Point) return Point;
   --  Second Derivative of Bezier Cubic Polynomial

   function Cubic_Bezier_Spline (Knots  : Point_Array) return Point_Array;
   --  Compute the cubic bezier spline following the algorithm of
   --  https://www.particleincell.com/2012/bezier-splines/
   --  K : the nodes of the spline
   --  Result: an array of points in this way:
   --  K (K'First), control point1, control point 2,
   --  K (K'First + 1), control point1, control point 2,
   --  K (K'Last)
   --  This spline is a C2 function.
   --  Otherwise the chord in first and last segment will be Offset.
   --  Program_Error is raised if Knots'Length < 3.
end Gtkada.Style_Pkg.Cubic_Bezier;
