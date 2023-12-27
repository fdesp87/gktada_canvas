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

with Ada.Unchecked_Deallocation;
with Glib;                   use Glib;
with Gdk.Cairo;              use Gdk.Cairo;
with Cairo;                  use Cairo;

with Gtkada.Canvas_Item_Pkg;        use Gtkada.Canvas_Item_Pkg;
with Gtkada.Canvas_Link_Pkg.Links;  use Gtkada.Canvas_Link_Pkg.Links;

package body Gtkada.Canvas_Polyline_Item_Pkg is
   use Gtkada.Style_Pkg;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Gtkada.Style_Pkg.Point_Array, Gtkada.Style_Pkg.Point_Array_Access);

   ----------------------
   -- Gtk_New_Polyline --
   ----------------------

   function Gtk_New_Polyline
     (Style    : Gtkada.Style_Pkg.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False)
      return Polyline_Item
   is
      R   : constant Polyline_Item := new Polyline_Item_Record;
   begin
      Initialize_Polyline (R, Style, Points, Close, Relative);
      return R;
   end Gtk_New_Polyline;

   -------------------------
   -- Initialize_Polyline --
   -------------------------

   procedure Initialize_Polyline
     (Self     : not null access Polyline_Item_Record'Class;
      Style    : Gtkada.Style_Pkg.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False) is
   begin
      Self.Set_Style (Style);
      Self.Close    := Close;
      Self.Relative := Relative;
      Self.Points   := new Item_Point_Array'(Points);
   end Initialize_Polyline;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context)
   is
      Shadow : constant Shadow_Style := Self.Get_Style.Get_Shadow;
   begin
      if Shadow /= No_Shadow then
         Save (Context.Cr);
         Translate (Context.Cr, Shadow.X_Offset, Shadow.Y_Offset);

         if Self.Get_Style.Path_Polyline
           (Context.Cr, Self.Points.all,
            Close    => Self.Close,
            Relative => Self.Relative)
         then
            Set_Source_RGBA (Context.Cr, Shadow.Color);
            Cairo.Fill (Context.Cr);
         end if;

         Restore (Context.Cr);
      end if;

      Resize_Fill_Pattern (Self);
      Self.Get_Style.Draw_Polyline
        (Context.Cr, Self.Points.all,
         Close    => Self.Close,
         Relative => Self.Relative);
      Draw_Children (Self, Context);
   end Draw;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : not null access Polyline_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
   begin
      Unchecked_Free (Self.Points);
      Container_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context)
   is
      B : constant Item_Rectangle := Compute_Bounding_Box
        (Self.Points.all, Relative => Self.Relative);
   begin
      Container_Item_Record (Self.all).Size_Request (Context);  --  inherited
      Self.Set_Size_Request
        (Model_Coordinate'Max (Self.Get_Width, B.Width + B.X),
         Model_Coordinate'Max (Self.Get_Height, B.Height + B.Y));
   end Size_Request;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Polyline_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      Box   : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
   begin
      if Point_In_Rect (Box, Point) then
         if Self.Get_Style.Path_Polyline
           (Context.Cr,
            Points   => Self.Points.all,
            Close    => Self.Close,
            Relative => Self.Relative)
         then
            return In_Fill (Context.Cr, Point.X, Point.Y);
         end if;
      end if;
      return False;
   end Contains;

   ---------------
   -- Clip_Line --
   ---------------

   overriding function Clip_Line
     (Self   : not null access Polyline_Item_Record;
      P1, P2 : Item_Point) return Item_Point
   is
      Inter : Item_Point;
      Current, Next : Item_Point;
   begin
      if Self.Points /= null then
         Current := Self.Points (Self.Points'First);

         for P in Self.Points'First + 1 .. Self.Points'Last loop
            if Self.Relative then
               Next := (Current.X + Self.Points (P).X,
                        Current.Y + Self.Points (P).Y);
            else
               Next := Self.Points (P);
            end if;

            Inter := Intersection (P1, P2, Current, Next);
            if Inter.X /= Gdouble'First then
               return Inter;
            end if;

            Current := Next;
         end loop;

         if Self.Close then
            Inter := Intersection
              (P1, P2, Current, Self.Points (Self.Points'First));
            if Inter.X /= Gdouble'First then
               return Inter;
            end if;
         end if;
      end if;

      return P1;
   end Clip_Line;

end Gtkada.Canvas_Polyline_Item_Pkg;
