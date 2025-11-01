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
with Gdk.Cairo;                          use Gdk.Cairo;
with Cairo;                              use Cairo;

with Gtkada.Canvas_Item_Pkg;             use Gtkada.Canvas_Item_Pkg;

package body Gtkada.Canvas_Ellipse_Item_Pkg is
   use Gtkada.Style_Pkg;

   ---------------------
   -- Gtk_New_Ellipse --
   ---------------------

   function Gtk_New_Ellipse
     (Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Ellipse_Item
   is
      R : constant Ellipse_Item := new Ellipse_Item_Record;
   begin
      Initialize_Ellipse (R, Style, Width, Height);
      return R;
   end Gtk_New_Ellipse;

   ------------------------
   -- Initialize_Ellipse --
   ------------------------

   procedure Initialize_Ellipse
     (Self          : not null access Ellipse_Item_Record'Class;
      Style         : Gtkada.Style_Pkg.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
   is
   begin
      Self.Set_Style (Style);
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Ellipse;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Ellipse_Item_Record;
      Context : Draw_Context)
   is
      Shadow : constant Shadow_Style := Self.Get_Style.Get_Shadow;
   begin
      if Shadow /= No_Shadow then
         Save (Context.Cr);
         Translate (Context.Cr, Shadow.X_Offset, Shadow.Y_Offset);

         if Self.Get_Style.Path_Ellipse
           (Context.Cr, (0.0, 0.0), Self.Get_Width, Self.Get_Height)
         then
            Set_Source_RGBA (Context.Cr, Shadow.Color);
            Cairo.Fill (Context.Cr);
         end if;

         Restore (Context.Cr);
      end if;

      Resize_Fill_Pattern (Self);
      Self.Get_Style.Draw_Ellipse
        (Context.Cr, (0.0, 0.0), Self.Get_Width, Self.Get_Height);
      Self.Draw_Children (Context);
   end Draw;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Ellipse_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      pragma Unreferenced (Context);
      Box   : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
      Center : Item_Point;
      Dx, Dy : Item_Coordinate;
   begin
      if Point_In_Rect (Box, Point) then
         Center := (Box.Width / 2.0, Box.Height / 2.0);

         --  This doesn't test the whole rectangle, only the topleft corner
         Dx := Point.X - Center.X;
         Dy := Point.Y - Center.Y;
         return Dx * Dx / (Center.X * Center.X)
           + Dy * Dy / (Center.Y * Center.Y) <= 1.0;
      end if;
      return False;
   end Contains;
end Gtkada.Canvas_Ellipse_Item_Pkg;
