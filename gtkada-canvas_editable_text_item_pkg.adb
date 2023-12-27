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

with Glib;                               use Glib;
with Glib.Object;                        use Glib.Object;
with Gdk.Event;                          use Gdk.Event;
with Gdk.Types;                          use Gdk.Types;
with Gdk.Types.Keysyms;                  use Gdk.Types.Keysyms;
with Gtk.Widget;                         use Gtk.Widget;
with Gtk.Accel_Group;                    use Gtk.Accel_Group;
with Gtk.Text_Buffer;                    use Gtk.Text_Buffer;
with Gtk.Text_Iter;                      use Gtk.Text_Iter;
with Gtk.Text_View;                      use Gtk.Text_View;
with Gtk.Enums;                          use Gtk.Enums;
with Pango.Font;                         use Pango.Font;

with Gtkada.Canvas_View_Pkg.Views;       use Gtkada.Canvas_View_Pkg.Views;

package body Gtkada.Canvas_Editable_Text_Item_Pkg is

   ---------------------------
   -- Gtk_New_Editable_Text --
   ---------------------------

   function Gtk_New_Editable_Text
     (Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
      return Editable_Text_Item
   is
      Result : constant Editable_Text_Item := new Editable_Text_Item_Record;
   begin
      Initialize_Editable_Text (Result, Style, Text, Directed);
      return Result;
   end Gtk_New_Editable_Text;

   ------------------------------
   -- Initialize_Editable_Text --
   ------------------------------

   procedure Initialize_Editable_Text
     (Self     : not null access Editable_Text_Item_Record'Class;
      Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
   is
   begin
      Initialize_Text (Self, Style, Text, Directed);
   end Initialize_Editable_Text;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Self   : not null access Editable_Text_Item_Record'Class;
      Editable : Boolean) is
   begin
      Self.Editable := Editable;
   end Set_Editable;

   -----------------
   -- Is_Editable --
   -----------------

   function Is_Editable
     (Self   : not null access Editable_Text_Item_Record'Class)
     return Boolean is
   begin
      return Self.Editable;
   end Is_Editable;

   ----------------------------
   -- On_Text_Edit_Key_Press --
   ----------------------------

   function On_Text_Edit_Key_Press
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Key)
      return Boolean;
   --  Called when the user is inline-editing a text widget, to properly close
   --  the editor.

   function On_Text_Edit_Key_Press
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Key)
     return Boolean
   is
      Self : constant Canvas_View := Canvas_View (View);
      Text : Gtk_Text_View;
      From, To : Gtk_Text_Iter;
   begin
      --  <return> closes the editing, but <shift-return> just
      --  inserts a newline
      if Event.Keyval = GDK_Return
         and then (Event.State and Get_Default_Mod_Mask) = 0
      then
         declare
            Old : constant String := Editable_Text_Item
              (Self.The_Inline_Edit_Item).Get_Text;
         begin
            Text := Gtk_Text_View (Self.Get_Child);
            Text.Get_Buffer.Get_Start_Iter (From);
            Text.Get_Buffer.Get_End_Iter (To);

            Editable_Text_Item (Self.The_Inline_Edit_Item).Set_Text
              (Text.Get_Buffer.Get_Text
                 (Start   => From,
                  The_End => To));

            Editable_Text_Item (Self.The_Inline_Edit_Item).On_Edited (Old);

            Cancel_Inline_Editing (Self);
            Self.Model.Refresh_Layout;
         end;

         return True;

      elsif Event.Keyval = GDK_Escape then
         Cancel_Inline_Editing (Self);
         return True;
      end if;

      return False;
   end On_Text_Edit_Key_Press;

   -----------------
   -- Edit_Widget --
   -----------------

   overriding function Edit_Widget
     (Self  : not null access Editable_Text_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Text   : Gtk_Text_View;
      Font   : Pango_Font_Description;
      Start, Finish : Gtk_Text_Iter;
      Buffer : Gtk_Text_Buffer;
   begin
      if Self.Editable then
         Gtk_New (Buffer);
         Gtk_New (Text, Buffer);

         Font := Copy (Self.Get_Style.Get_Font.Name);
         Set_Size (Font, Gint (Gdouble (Get_Size (Font)) * View.Get_Scale));
         Text.Override_Font (Font);
         Free (Font);

         Text.Override_Color
            (Gtk_State_Flag_Normal or Gtk_State_Flag_Active,
             Self.Get_Style.Get_Font.Color);

         Buffer.Set_Text (Self.Get_Text);   --  not compute_text
         Text.On_Key_Press_Event (On_Text_Edit_Key_Press'Access, View);

         --  Select the whole text so that users can more easily replace
         --  it
         Buffer.Get_Start_Iter (Start);
         Buffer.Get_End_Iter (Finish);
         Buffer.Select_Range (Start, Finish);

         return Gtk_Widget (Text);
      else
         return null;
      end if;
   end Edit_Widget;

end Gtkada.Canvas_Editable_Text_Item_Pkg;
