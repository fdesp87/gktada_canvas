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

with Gtk.Widget;
with Gtkada.Style_Pkg;                    use Gtkada.Style_Pkg;
with Glib;                            use Glib;

with Gtkada.Canvas_Defs;              use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Abstract_Item_Pkg; use Gtkada.Canvas_Abstract_Item_Pkg;
with Gtkada.Canvas_View_Pkg;          use Gtkada.Canvas_View_Pkg;

package Gtkada.Canvas_Item_Pkg is

   type Canvas_Item_Record is abstract new Abstract_Item_Record with private;
   type Canvas_Item is access all Canvas_Item_Record'Class;
   --  An implementation of the Abstract_Item interface, which handles a
   --  number of the operations automatically. For instance, it will store the
   --  position of the item and its bounding box.
   --  It is easier to derive from this type when you want to create your own
   --  items, unless you want complete control of the data storage.

   overriding function Is_Link
     (Self : not null access Canvas_Item_Record) return Boolean is (False);

   overriding function Parent
     (Self : not null access Canvas_Item_Record)
      return Abstract_Item is (null);

   overriding function Is_Invisible
     (Self : not null access Canvas_Item_Record)
      return Boolean is (False);

   function Inner_Most_Item
     (Self           : not null access Canvas_Item_Record;
      Dummy_At_Point : Model_Point;
      Dummy_Context  : Draw_Context)
      return Abstract_Item is (Self);

   overriding function Position
     (Self : not null access Canvas_Item_Record) return Gtkada.Style_Pkg.Point;

   overriding function Contains
     (Self    : not null access Canvas_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;

   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Item_Record;
      Anchor : Anchor_Attachment)
      return Item_Point;

   overriding function Clip_Line
     (Self   : not null access Canvas_Item_Record;
      P1, P2 : Item_Point) return Item_Point;

   overriding function Edit_Widget
     (Self  : not null access Canvas_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget;

   overriding procedure Draw_As_Selected
     (Self    : not null access Canvas_Item_Record;
      Context : Draw_Context);

   overriding procedure Draw_Outline
     (Self    : not null access Canvas_Item_Record;
      Style   : Gtkada.Style_Pkg.Drawing_Style;
      Context : Draw_Context);

   overriding procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Item_Record;
      Threshold : Gdouble);

   overriding function Get_Visibility_Threshold
     (Self : not null access Canvas_Item_Record) return Gdouble;

   overriding procedure Set_Position
     (Self     : not null access Canvas_Item_Record;
      Pos      : Gtkada.Style_Pkg.Point);
   --  Sets the position of the item within its parent (or within the canvas
   --  view if Self has no parent).

private

   type Canvas_Item_Record is abstract new Abstract_Item_Record with record
      Position : Gtkada.Style_Pkg.Point := No_Position;
      --  Position within its parent or the canvas view.

      Visibility_Threshold : Gdouble := 0.0;
      --  See Set_Visibility_Threshold.
   end record;

end Gtkada.Canvas_Item_Pkg;
