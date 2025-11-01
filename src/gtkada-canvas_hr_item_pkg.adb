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

with Glib;                   use Glib;
with Pango.Layout;           use Pango.Layout;

package body Gtkada.Canvas_Hr_Item_Pkg is
   use Gtkada.Style_Pkg;

   ----------------
   -- Gtk_New_Hr --
   ----------------

   function Gtk_New_Hr
     (Style   : Gtkada.Style_Pkg.Drawing_Style;
      Text    : String := "")
     return Hr_Item
   is
      R : constant Hr_Item := new Hr_Item_Record;
   begin
      Initialize_Hr (R, Style, Text);
      return R;
   end Gtk_New_Hr;

   -------------------
   -- Initialize_Hr --
   -------------------

   procedure Initialize_Hr
     (Self    : not null access Hr_Item_Record'Class;
      Style   : Gtkada.Style_Pkg.Drawing_Style;
      Text    : String := "") is
   begin
      Self.Set_Style (Style);
      Self.Text := new String'(Text);
   end Initialize_Hr;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context)
   is
      H : constant Model_Coordinate := Self.Get_Height / 2.0;
      W : Model_Coordinate;
   begin
      if Self.Text.all = "" then
         Self.Get_Style.Draw_Polyline (Context.Cr, ((0.0, H), (Self.Get_Width, H)));
      else
         W := (Self.Get_Width - Self.Requested_Width) / 2.0;
         Self.Get_Style.Draw_Polyline (Context.Cr, ((0.0, H), (W, H)));

         W := W + Self.Space;
         if Context.Layout /= null then
            Self.Get_Style.Draw_Text
              (Context.Cr, Context.Layout,
               (W, (Self.Get_Height - Self.Requested_Height) / 2.0),
               Self.Text.all,
               Max_Width  => Self.Get_Width,
               Max_Height => Self.Get_Height);
         end if;

         W := W + Self.Requested_Width + Self.Space;
         Self.Get_Style.Draw_Polyline (Context.Cr, ((W, H), (Self.Get_Width, H)));
      end if;
   end Draw;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : not null access Hr_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Free (Self.Text);
      Container_Item_Record (Self.all).Destroy (In_Model);
   end Destroy;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context)
   is
   begin
      if Context.Layout /= null and then Self.Text.all /= "" then
         Self.Get_Style.Measure_Text
           (Context.Layout,
            Self.Text.all,
            Width  => Self.Requested_Width,
            Height => Self.Requested_Height);

         --  Some space to show the lines
         Self.Set_Size_Request
           (Self.Requested_Width
             + 2.0 * Self.Space
             + 8.0,   --  lines should be at least 4 pixels each side
            Self.Requested_Height);
      else
         Self.Set_Size_Request (8.0, 1.0);
      end if;
   end Size_Request;

end Gtkada.Canvas_Hr_Item_Pkg;
