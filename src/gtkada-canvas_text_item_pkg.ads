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
with Glib;                                use Glib;
with GNAT.Strings;                        use GNAT.Strings;

with Gtkada.Canvas_Defs;                  use Gtkada.Canvas_Defs;
with Gtkada.Canvas_View_Pkg;              use Gtkada.Canvas_View_Pkg;
with Gtkada.Canvas_Container_Item_Pkg;    use Gtkada.Canvas_Container_Item_Pkg;
with Gtkada.Canvas_Model_Pkg;             use Gtkada.Canvas_Model_Pkg;

package Gtkada.Canvas_Text_Item_Pkg is

   type Text_Item_Record is new Container_Item_Record with private;
   type Text_Item is access all Text_Item_Record'Class;
   --  A predefined object that displays itself as text.

   function Gtk_New_Text
     (Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Text_Item;

   procedure Initialize_Text
     (Self     : not null access Text_Item_Record'Class;
      Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
   --  Create a new text item
   --
   --  Directed indicates whether the text should be followed (or preceded)
   --  by a directional arrow. This is used when displaying text along links,
   --  to help users read the meaning of the label.

   procedure Set_Directed
     (Self     : not null access Text_Item_Record;
      Directed : Text_Arrow_Direction := No_Text_Arrow);
   --  Change the direction of the arrow.
   --  In particular, this is done automatically when the text is used on a
   --  link.

   procedure Set_Text
     (Self : not null access Text_Item_Record;
      Text : String);

   function Get_Text
     (Self : not null access Text_Item_Record) return String;
   --  Change the text displayed in the item.
   --  This does not force a refresh of the item, and it is likely that you
   --  will need to call the Model's Refresh_Layout method to properly
   --  recompute sizes of items and link paths.

   overriding procedure Draw
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context);

   overriding procedure Destroy
     (Self     : not null access Text_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   overriding procedure Size_Request
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context);

   function Direction (Self : not null access Text_Item_Record)
                       return Text_Arrow_Direction;

   procedure Set_Direction (Self : not null access Text_Item_Record;
                            Dir  : Text_Arrow_Direction);

private

   type Text_Item_Record is new Container_Item_Record with record
      Text     : GNAT.Strings.String_Access;
      Directed : Text_Arrow_Direction;
   end record;

end Gtkada.Canvas_Text_Item_Pkg;
