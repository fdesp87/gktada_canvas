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

with Gtkada.Canvas_Defs;               use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Container_Item_Pkg; use Gtkada.Canvas_Container_Item_Pkg;
with Gtkada.Canvas_View_Pkg;           use Gtkada.Canvas_View_Pkg;
with Gtkada.Canvas_Model_Pkg;          use Gtkada.Canvas_Model_Pkg;

package Gtkada.Canvas_Polyline_Item_Pkg is

   type Polyline_Item_Record is new Container_Item_Record with private;
   type Polyline_Item is access all Polyline_Item_Record'Class;
   --  A predefine object that displays itself as a set of joined lines.
   --  This object can optionally contain children, and the polyline can thus
   --  be used to draw a polygon around those items

   function Gtk_New_Polyline
     (Style    : Gtkada.Style_Pkg.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False)
      return Polyline_Item;

   procedure Initialize_Polyline
     (Self     : not null access Polyline_Item_Record'Class;
      Style    : Gtkada.Style_Pkg.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False);
   --  Create a new polyline item.
   --  If Relative is true, then each point is relative to the previous one
   --  (i.e. its coordinates are the previous points's coordinate plus the
   --  offset given in points). The first point is of course in item
   --  coordinates.

   overriding procedure Draw
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context);

   overriding procedure Destroy
     (Self     : not null access Polyline_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   overriding procedure Size_Request
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context);

   overriding function Contains
     (Self    : not null access Polyline_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;

   overriding function Clip_Line
     (Self   : not null access Polyline_Item_Record;
      P1, P2 : Item_Point) return Item_Point;

private

   type Polyline_Item_Record is new Container_Item_Record with record
      Points   : Item_Point_Array_Access;
      Close    : Boolean;
      Relative : Boolean;
   end record;

end Gtkada.Canvas_Polyline_Item_Pkg;
