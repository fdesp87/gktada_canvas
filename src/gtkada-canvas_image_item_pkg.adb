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

with Glib;                    use Glib;

package body Gtkada.Canvas_Image_Item_Pkg is
   use Gtkada.Style_Pkg;

   -------------------
   -- Gtk_New_Image --
   -------------------

   function Gtk_New_Image
     (Style  : Gtkada.Style_Pkg.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item
   is
      R : constant Image_Item := new Image_Item_Record;
   begin
      Initialize_Image (R, Style, Image, Allow_Rescale, Width, Height);
      return R;
   end Gtk_New_Image;

   -------------------
   -- Gtk_New_Image --
   -------------------

   function Gtk_New_Image
     (Style  : Gtkada.Style_Pkg.Drawing_Style;
      Icon_Name : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item
   is
      R : constant Image_Item := new Image_Item_Record;
   begin
      Initialize_Image (R, Style, Icon_Name, Allow_Rescale, Width, Height);
      return R;
   end Gtk_New_Image;

   ----------------------
   -- Initialize_Image --
   ----------------------

   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style_Pkg.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double) is
   begin
      Self.Set_Style (Style);
      Self.Image := Gdk_Pixbuf (Image);
      Self.Allow_Rescale := Allow_Rescale;
      Ref (Self.Image);
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Image;

   ----------------------
   -- Initialize_Image --
   ----------------------

   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style_Pkg.Drawing_Style;
      Icon_Name  : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double) is
   begin
      Self.Set_Style (Style);
      Self.Icon_Name := new String'(Icon_Name);
      Self.Allow_Rescale := Allow_Rescale;
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Image;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Image_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      if Self.Image /= null then
         Unref (Self.Image);
         Self.Image := null;
      end if;
      Free (Self.Icon_Name);
      Container_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context)
   is
      P : Gdk_Pixbuf;
      W, H  : Gdouble;
   begin
      Self.Get_Style.Draw_Rect (Context.Cr, (0.0, 0.0),
                                Self.Get_Width, Self.Get_Height);

      if Self.Icon_Name /= null then
         W := Gdouble'Min (Self.Get_Width, Self.Get_Height);
         Draw_Pixbuf_With_Scale
            (Context.Cr,
             Self.Icon_Name.all,
             Size => Gint (W),
             X => (Self.Get_Width - W) / 2.0,
             Y => (Self.Get_Height - W) / 2.0,
             Widget => Context.View);

      else
         W := Gdouble (Get_Width (Self.Image)) - Self.Get_Width;
         H := Gdouble (Get_Height (Self.Image)) - Self.Get_Height;

         if Self.Allow_Rescale and then
            (abs (W) >= 2.0 or else abs (H) >= 2.0)
         then
            P := Scale_Simple
              (Self.Image,
               Dest_Width  => Gint (Self.Get_Width),
               Dest_Height => Gint (Self.Get_Height));
            Draw_Pixbuf_With_Scale
              (Context.Cr, P, 0.0, 0.0, Context.View);
            Unref (P);
         else
            Draw_Pixbuf_With_Scale
              (Context.Cr, Self.Image, W / 2.0, H / 2.0, Context.View);
         end if;
      end if;
   end Draw;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context)
   is
   begin
      Container_Item_Record (Self.all).Size_Request (Context);  --  inherited
      if Self.Image /= null then
         Self.Set_Size_Request
           (Model_Coordinate'Max
              (Self.Get_Width, Gdouble (Get_Width (Self.Image))),
            Model_Coordinate'Max
              (Self.Get_Height, Gdouble (Get_Height (Self.Image))));
      end if;
   end Size_Request;


end Gtkada.Canvas_Image_Item_Pkg;
