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

package body Gtkada.Canvas_Defs is

   -------------------
   -- Point_In_Rect --
   -------------------

   function Point_In_Rect
     (Rect  : Model_Rectangle;
      P     : Model_Point) return Boolean
   is
      X : constant Model_Coordinate := P.X - Rect.X;
      Y : constant Model_Coordinate := P.Y - Rect.Y;
   begin
      return 0.0 <= X and then X <= Rect.Width
        and then 0.0 <= Y and then Y <= Rect.Height;
   end Point_In_Rect;

   -------------------
   -- Point_In_Rect --
   -------------------

   function Point_In_Rect
     (Rect : Item_Rectangle; P : Item_Point) return Boolean
   is
      X : constant Item_Coordinate := P.X - Rect.X;
      Y : constant Item_Coordinate := P.Y - Rect.Y;
   begin
      return 0.0 <= X and then X <= Rect.Width
        and then 0.0 <= Y and then Y <= Rect.Height;
   end Point_In_Rect;


   ----------------
   -- Intersects --
   ----------------

   function Intersects (Rect1, Rect2 : Model_Rectangle) return Boolean is
   begin
      return not
        (Rect1.X > Rect2.X + Rect2.Width            --  R1 on the right of R2
         or else Rect2.X > Rect1.X + Rect1.Width    --  R2 on the right of R1
         or else Rect1.Y > Rect2.Y + Rect2.Height   --  R1 below R2
         or else Rect2.Y > Rect1.Y + Rect1.Height); --  R1 above R2
   end Intersects;

   ----------------
   -- Intersects --
   ----------------

   function Intersects (Rect1, Rect2 : Item_Rectangle) return Boolean is
   begin
      return not
        (Rect1.X > Rect2.X + Rect2.Width            --  R1 on the right of R2
         or else Rect2.X > Rect1.X + Rect1.Width    --  R2 on the right of R1
         or else Rect1.Y > Rect2.Y + Rect2.Height   --  R1 below R2
         or else Rect2.Y > Rect1.Y + Rect1.Height); --  R1 above R2
   end Intersects;

   ------------------
   -- Intersection --
   ------------------
   --  Algorithm adapted from [GGII - xlines.c].
   --  Copied from gnatcoll-geometry.adb

   function Intersection
     (P11, P12, P21, P22 : Item_Point) return Item_Point
   is
      type Line is record
         A, B, C : Item_Coordinate;
      end record;

      function To_Line (P1, P2 : Item_Point) return Line;
      --  Compute the parameters for the line
      --     Ax + By = C

      function To_Line (P1, P2 : Item_Point) return Line is
         A : constant Item_Coordinate := P2.Y - P1.Y;
         B : constant Item_Coordinate := P1.X - P2.X;
      begin
         return (A => A,
                 B => B,
                 C => A * P1.X + B * P1.Y);
      end To_Line;

      L1 : constant Line := To_Line (P11, P12);
      R3 : constant Item_Coordinate := L1.A * P21.X + L1.B * P21.Y - L1.C;
      R4 : constant Item_Coordinate := L1.A * P22.X + L1.B * P22.Y - L1.C;
      L2 : constant Line := To_Line (P21, P22);
      R1 : constant Item_Coordinate := L2.A * P11.X + L2.B * P11.Y - L2.C;
      R2 : constant Item_Coordinate := L2.A * P12.X + L2.B * P12.Y - L2.C;

      Denom : Item_Coordinate;

   begin
      --  Check signs of R3 and R4. If both points 3 and 4 lie on same side
      --  of line 1, the line segments do not intersect

      if (R3 > 0.0 and then R4 > 0.0)
        or else (R3 < 0.0 and then R4 < 0.0)
      then
         return (Gdouble'First, Gdouble'First);
      end if;

      --  Check signs of r1 and r2. If both points lie on same side of
      --  second line segment, the line segments do not intersect

      if (R1 > 0.0 and then R2 > 0.0)
        or else (R1 < 0.0 and then R2 < 0.0)
      then
         return (Gdouble'First, Gdouble'First);
      end if;

      --  Line segments intersect, compute intersection point
      Denom := L1.A * L2.B - L2.A * L1.B;
      if Denom = 0.0 then
         if abs (L1.A * P21.X + L1.B * P21.Y - L1.C) < 0.00001 then
            --  colinears
            return P11;
         else
            return (Gdouble'First, Gdouble'First);
         end if;
      end if;

      return
        (X => (L2.B * L1.C - L1.B * L2.C) / Denom,
         Y => (L1.A * L2.C - L2.A * L1.C) / Denom);
   end Intersection;

   -----------
   -- Union --
   -----------

   procedure Union
     (Rect1 : in out Model_Rectangle;
      Rect2 : Model_Rectangle)
   is
      Right : constant Model_Coordinate :=
        Model_Coordinate'Max (Rect1.X + Rect1.Width, Rect2.X + Rect2.Width);
      Bottom : constant Model_Coordinate :=
        Model_Coordinate'Max (Rect1.Y + Rect1.Height, Rect2.Y + Rect2.Height);
   begin
      Rect1.X := Model_Coordinate'Min (Rect1.X, Rect2.X);
      Rect1.Width := Right - Rect1.X;

      Rect1.Y := Model_Coordinate'Min (Rect1.Y, Rect2.Y);
      Rect1.Height := Bottom - Rect1.Y;
   end Union;

   ---------------------
   -- Size_From_Value --
   ---------------------

   function Size_From_Value (Value : Model_Coordinate) return Size is
   begin
      if Value = Fit_Size_As_Double then
         return Fit_Size;
      elsif Value = Auto_Size_As_Double then
         return Auto_Size;
      else
         return (Unit_Pixels, Value);
      end if;
   end Size_From_Value;

end Gtkada.Canvas_Defs;
