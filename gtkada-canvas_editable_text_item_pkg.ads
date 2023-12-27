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

with Glib;
with Gtkada.Style_Pkg;
with Gtk.Widget;

with Gtkada.Canvas_Defs;                         use Gtkada.Canvas_Defs;
with Gtkada.Canvas_View_Pkg;                     use Gtkada.Canvas_View_Pkg;
with Gtkada.Canvas_Text_Item_Pkg;                use Gtkada.Canvas_Text_Item_Pkg;

package Gtkada.Canvas_Editable_Text_Item_Pkg is

   type Editable_Text_Item_Record is new Text_Item_Record with private;
   type Editable_Text_Item is access all Editable_Text_Item_Record'Class;
   --  A special text item that can be double-clicked on to be editing in
   --  place (provided the Gtkada.Canvas_View.Views.On_Item_Event_Edit
   --  callback was added to the view).

   function Gtk_New_Editable_Text
     (Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
      return Editable_Text_Item;

   procedure Initialize_Editable_Text
     (Self     : not null access Editable_Text_Item_Record'Class;
      Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow);
   --  Create a new text item

   procedure Set_Editable
      (Self   : not null access Editable_Text_Item_Record'Class;
       Editable : Boolean);
   function Is_Editable
      (Self   : not null access Editable_Text_Item_Record'Class)
      return Boolean;
   --  Sets whether Self can be edited interactively by double-clicking
   --  on it. You should also call
   --  Gtkada.Canvas_View.Views.Cancel_Inline_Editing in case some editing
   --  was taking place.

   procedure On_Edited
     (Self     : not null access Editable_Text_Item_Record;
      Old_Text : String) is null;
   --  Called after the text has been edited

   overriding function Edit_Widget
     (Self  : not null access Editable_Text_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget;

private

   type Editable_Text_Item_Record is new Text_Item_Record with record
      Editable : Boolean := True;
   end record;

end Gtkada.Canvas_Editable_Text_Item_Pkg;
