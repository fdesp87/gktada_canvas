------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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
------------------------------------------------------------------------------
-- Modified by JLF (fdesp87@gmail.com) to separate the various concepts.    --
-- See gtkada.canvas_view                                                   --
------------------------------------------------------------------------------

with Gtkada.Style_Pkg;                        use Gtkada.Style_Pkg;

with Gtkada.Canvas_Defs;                  use Gtkada.Canvas_Defs;
with Gtkada.Canvas_View_Pkg;              use Gtkada.Canvas_View_Pkg;
with Gtkada.Canvas_Container_Item_Pkg;    use Gtkada.Canvas_Container_Item_Pkg;

package Gtkada.Canvas_Rect_Item_Pkg is

   type Rect_Item_Record is new Container_Item_Record with private;
   type Rect_Item is access all Rect_Item_Record'Class;
   --  A predefined type object which displays itself as a rectangle or a
   --  rectangle with rounded corners.

   function Gtk_New_Rect
     (Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
      Radius        : Model_Coordinate := 0.0)
      return Rect_Item;

   procedure Initialize_Rect
     (Self          : not null access Rect_Item_Record'Class;
      Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
      Radius        : Model_Coordinate := 0.0);
   --  Create a new rectangle item.
   --  Specifying the size should rather be done with a call to
   --  Set_Size, which provides more flexibility with regards to the units
   --  used to describe the size.

   overriding procedure Draw
     (Self    : not null access Rect_Item_Record;
      Context : Draw_Context);

   overriding procedure Draw_Outline
     (Self    : not null access Rect_Item_Record;
      Style   : Gtkada.Style_Pkg.Drawing_Style;
      Context : Draw_Context);

private

   type Rect_Item_Record is new Container_Item_Record with record
      Radius   : Model_Coordinate;
   end record;

end Gtkada.Canvas_Rect_Item_Pkg;
