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
with Gdk.RGBA;                           use Gdk.RGBA;
with Gdk.Cairo;                          use Gdk.Cairo;
with Cairo;                              use Cairo;

package body Gtkada.Canvas_Rect_Item_Pkg is

   ------------------
   -- Gtk_New_Rect --
   ------------------

   function Gtk_New_Rect
     (Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
      Radius        : Model_Coordinate := 0.0)
      return Rect_Item
   is
      R : constant Rect_Item := new Rect_Item_Record;
   begin
      Initialize_Rect (R, Style, Width, Height, Radius);
      return R;
   end Gtk_New_Rect;

   ---------------------
   -- Initialize_Rect --
   ---------------------

   procedure Initialize_Rect
     (Self          : not null access Rect_Item_Record'Class;
      Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
      Radius        : Model_Coordinate := 0.0)
   is
   begin
      Set_Style (Self, Style);
      Self.Radius        := Radius;
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Rect;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Rect_Item_Record;
      Context : Draw_Context)
   is
      Stroke : constant Gdk_RGBA := Self.Get_Style.Get_Stroke;
      Fill   : constant Cairo_Pattern := Self.Get_Style.Get_Fill;
      Shadow : constant Shadow_Style := Self.Get_Style.Get_Shadow;
   begin
      if Shadow /= No_Shadow then
         Save (Context.Cr);
         Translate (Context.Cr, Shadow.X_Offset, Shadow.Y_Offset);

         if Self.Get_Style.Path_Rect
           (Context.Cr, (0.0, 0.0),
            Self.Get_Width, Self.Get_Height, Radius => Self.Radius)
         then
            Set_Source_RGBA (Context.Cr, Shadow.Color);
            Cairo.Fill (Context.Cr);
         end if;

         Restore (Context.Cr);
      end if;

      Resize_Fill_Pattern (Self);

      --  We need to clip the contents of the rectangle. Also, we
      --  stroke only after drawing the children so that they do
      --  no hide the stroke.

      if Self.Get_Style.Path_Rect
        (Context.Cr, (0.0, 0.0), Self.Get_Width, Self.Get_Height,
         Radius => Self.Radius)
      then
         --  Only clip if the item is visible, otherwise let nested items do
         --  their own clipping. This ensures that nested items with shadows
         --  are properly rendered

         if not Self.Is_Invisible then
            Clip_Preserve (Context.Cr);
         end if;

         if not Self.The_Children.Is_Empty then
            Self.Get_Style.Set_Stroke (Null_RGBA);
         end if;

         Self.Get_Style.Finish_Path (Context.Cr);
         Self.Get_Style.Set_Stroke (Stroke);
      end if;

      Self.Draw_Children (Context);

      if not Self.The_Children.Is_Empty
        and then Stroke /= Null_RGBA
        and then Self.Get_Style.Path_Rect
        (Context.Cr, (0.0, 0.0), Self.Get_Width, Self.Get_Height,
         Radius => Self.Radius)
      then
         Self.Get_Style.Set_Fill (Null_Pattern);
         Self.Get_Style.Finish_Path (Context.Cr);
         Self.Get_Style.Set_Fill (Fill);
      end if;
   end Draw;

   ------------------
   -- Draw_Outline --
   ------------------

   overriding procedure Draw_Outline
     (Self    : not null access Rect_Item_Record;
      Style   : Gtkada.Style_Pkg.Drawing_Style;
      Context : Draw_Context)
   is
      Margin_Pixels : constant View_Coordinate := 2.0;
      Margin        : Model_Coordinate;
   begin
      if Context.View /= null then
         Margin := Margin_Pixels / Context.View.The_Scale;
         Style.Draw_Rect
           (Context.Cr, (-Margin, -Margin),
            Self.Get_Width + 2.0 * Margin, Self.Get_Height + 2.0 * Margin,
            Radius => Self.Radius);
      end if;
   end Draw_Outline;

end Gtkada.Canvas_Rect_Item_Pkg;
