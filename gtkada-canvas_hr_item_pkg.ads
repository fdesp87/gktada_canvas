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
with GNAT.Strings;                        use GNAT.Strings;

with Gtkada.Canvas_Defs;                  use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Container_Item_Pkg;    use Gtkada.Canvas_Container_Item_Pkg;
with Gtkada.Canvas_View_Pkg;              use Gtkada.Canvas_View_Pkg;
with Gtkada.Canvas_Model_Pkg;             use Gtkada.Canvas_Model_Pkg;

package Gtkada.Canvas_Hr_Item_Pkg is

   type Hr_Item_Record is new Container_Item_Record with private;
   type Hr_Item is access all Hr_Item_Record'Class;
   --  A predefined object that displays itself as a horizontal line with
   --  optional text in the middle. This thus looks like:
   --              ---- text ----

   function Gtk_New_Hr
     (Style   : Gtkada.Style_Pkg.Drawing_Style;
      Text    : String := "")
      return Hr_Item;

   procedure Initialize_Hr
     (Self    : not null access Hr_Item_Record'Class;
      Style   : Gtkada.Style_Pkg.Drawing_Style;
      Text    : String := "");
   --  Create a new horizontal rule

   overriding procedure Draw
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context);

   overriding procedure Destroy
     (Self     : not null access Hr_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   overriding procedure Size_Request
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context);

private

   type Hr_Item_Record is new Container_Item_Record with record
      Text     : GNAT.Strings.String_Access;
      Requested_Width, Requested_Height : Model_Coordinate;

      Space    : Model_Coordinate := 4.0;
      --  Space between text and lines
   end record;

end Gtkada.Canvas_Hr_Item_Pkg;
