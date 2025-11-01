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

with Pango.Layout;            use Pango.Layout;

package body Gtkada.Canvas_Text_Item_Pkg is

   ------------------
   -- Gtk_New_Text --
   ------------------

   function Gtk_New_Text
     (Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Text_Item
   is
      R : constant Text_Item := new Text_Item_Record;
   begin
      Initialize_Text (R, Style, Text, Directed, Width, Height);
      return R;
   end Gtk_New_Text;

   ---------------------
   -- Initialize_Text --
   ---------------------

   procedure Initialize_Text
     (Self     : not null access Text_Item_Record'Class;
      Style    : Gtkada.Style_Pkg.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double) is
   begin
      Set_Style (Self, Style);
      Self.Text  := new String'(Text);
      Self.Directed := Directed;
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Text;

   ------------------
   -- Set_Directed --
   ------------------

   procedure Set_Directed
     (Self     : not null access Text_Item_Record;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
   is
   begin
      Self.Directed := Directed;
   end Set_Directed;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Self : not null access Text_Item_Record;
      Text : String)
   is
   begin
      Free (Self.Text);
      Self.Text := new String'(Text);
   end Set_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Self : not null access Text_Item_Record) return String is
   begin
      return Self.Text.all;
   end Get_Text;


   ------------------
   -- Compute_Text --
   ------------------
   --  internal
   function Compute_Text
     (Self : not null access Text_Item_Record'Class)
      return String;
   --  Return the text to display for Self, including the directional arrow

   function Compute_Text
     (Self : not null access Text_Item_Record'Class)
      return String
   is
   begin
      case Self.Directed is
         when No_Text_Arrow =>
            return Self.Text.all;

         when Up_Text_Arrow =>
            return Self.Text.all & " "
              & Character'Val (16#E2#)   --  UTF8 for  \u25b2
              & Character'Val (16#96#)
              & Character'Val (16#B2#);

         when Down_Text_Arrow =>
            return Self.Text.all & " "
              & Character'Val (16#E2#)   --  UTF8 for  \u25bc
              & Character'Val (16#96#)
              & Character'Val (16#BC#);

         when Left_Text_Arrow =>
            return Character'Val (16#E2#)   --  UTF8 for  \u25c0
              & Character'Val (16#97#)
              & Character'Val (16#80#)
              & " " & Self.Text.all;

         when Right_Text_Arrow =>
            return Self.Text.all & " "
              & Character'Val (16#E2#)   --  UTF8 for  \u25b6
              & Character'Val (16#96#)
              & Character'Val (16#B6#);
      end case;
   end Compute_Text;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context)
   is
      Text : constant String := Compute_Text (Self);
   begin
      Resize_Fill_Pattern (Self);
      Self.Get_Style.Draw_Rect (Context.Cr, (0.0, 0.0),
                                Self.Get_Width, Self.Get_Height);

      if Context.Layout /= null then
         Self.Get_Style.Draw_Text
           (Context.Cr, Context.Layout, (0.0, 0.0), Text,
            Max_Width  => Self.Get_Width,
            Max_Height => Self.Get_Height);
      end if;
   end Draw;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : not null access Text_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Free (Self.Text);
      Container_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self  : not null access Text_Item_Record;
      Context : Draw_Context)
   is
      Text : constant String := Compute_Text (Self);
      W, H : Model_Coordinate;
   begin
      if Context.Layout /= null then
         Self.Get_Style.Measure_Text (Context.Layout, Text, Width => W, Height => H);
         Set_Size_Request (Self, W, H);
      else
         Set_Size_Request (Self, 0.0, 0.0);
      end if;
   end Size_Request;

   ---------------
   -- Direction --
   ---------------

   function Direction (Self : not null access Text_Item_Record)
                       return Text_Arrow_Direction is
   begin
      return Self.Directed;
   end Direction;

   -------------------
   -- Set_Direction --
   -------------------

   procedure Set_Direction (Self : not null access Text_Item_Record;
                            Dir  : Text_Arrow_Direction) is
   begin
      Self.Directed := Dir;
   end Set_Direction;

end Gtkada.Canvas_Text_Item_Pkg;
