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

with Gtkada.Style_Pkg;

with Gtkada.Canvas_Defs;                use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Container_Item_Pkg;  use Gtkada.Canvas_Container_Item_Pkg;
with Gtkada.Canvas_View_Pkg;            use Gtkada.Canvas_View_Pkg;

package Gtkada.Canvas_Ellipse_Item_Pkg is

   type Ellipse_Item_Record is new Container_Item_Record with private;
   type Ellipse_Item is access all Ellipse_Item_Record'Class;
   --  A predefined object that displays itself as a circle or an ellipse
   --  inscribed in a given rectangle.

   function Gtk_New_Ellipse
     (Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Ellipse_Item;

   procedure Initialize_Ellipse
     (Self          : not null access Ellipse_Item_Record'Class;
      Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
   --  Create a new ellipse item.
   --  If either Width or Height are negative, they will be computed based on
   --  the children's requested size (if there are no children, a default size
   --  is used).
   --  The ellipse is inscribed in the rectangle given by the item's position
   --  and the size passed in argument to this function.

   overriding procedure Draw
     (Self    : not null access Ellipse_Item_Record;
      Context : Draw_Context);

   overriding function Contains
     (Self    : not null access Ellipse_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;

private

   type Ellipse_Item_Record is new Container_Item_Record with null record;

end Gtkada.Canvas_Ellipse_Item_Pkg;
