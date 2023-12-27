
package body Gtkada.Style_Pkg.Cubic_Bezier is

   ----------------------------
   -- Bezier Cubic Polynmial --
   ----------------------------

   function Bezier_Cubic (T : Gdouble; P0, P1, P2, P3 : Point)
                          return Point is
      A, B, C, D : Gdouble;
      P   : Point;
   begin
      if T not in 0.0 .. 1.0 then
         raise Program_Error;
      end if;

      A := (1.0 - T) * (1.0 - T) * (1.0 - T);
      B := 3.0 * (1.0 - T) * (1.0 - T) * T;
      C := 3.0 * (1.0 - T) * T * T;
      D := T * T * T;

      P.X := A * P0.X + B * P1.X + C * P2.X + D * P3.X;
      P.Y := A * P0.Y + B * P1.Y + C * P2.Y + D * P3.Y;
      return P;
   end Bezier_Cubic;

   -------------------------------------------------
   -- First Derivative of Bezier Cubic Polynomial --
   -------------------------------------------------

   function Dt_Bezier_Cubic (T : Glib.Gdouble; P0, P1, P2, P3 : Point) return Point
   is
      A, B, C, D : Gdouble;
      P : Point;
   begin
      if T not in 0.0 .. 1.0 then
         raise Program_Error;
      end if;

      A := -3.0 * (1.0 - T) * (1.0 - T);
      B := 3.0 * (1.0 - 4.0 * T + 3.0 * T * T);
      C := 3.0 * (2.0 * T - 3.0 * T * T);
      D := 3.0 * T * T;

      P.X := A * P0.X + B * P1.X + C * P2.X + D * P3.X;
      P.Y := A * P0.Y + B * P1.Y + C * P2.Y + D * P3.Y;
      return P;
   end Dt_Bezier_Cubic;

   --------------------------------------------------
   -- Second Derivative of Bezier Cubic Polynomial --
   --------------------------------------------------

   function D2t_Bezier_Cubic (T : Gdouble; P0, P1, P2, P3 : Point) return Point is
      A, B, C, D : Gdouble;
      P : Point;
   begin
      A := 6.0 * (1.0 - T);
      B := 3.0 * (-4.0 + 6.0 * T);
      C := 3.0 * (2.0 - 6.0 * T);
      D := 6.0 * T;

      P.X := A * P0.X + B * P1.X + C * P2.X + D * P3.X;
      P.Y := A * P0.Y + B * P1.Y + C * P2.Y + D * P3.Y;
      return P;
   end D2t_Bezier_Cubic;

   ---------------------------------------------------
   -- Solve Tridiagonal System of Linear Equations  --
   ---------------------------------------------------

   function Solve (Rhs : Gdouble_Array) return Gdouble_Array;
   function Solve (Rhs : Gdouble_Array) return Gdouble_Array is
      N   : constant Integer := Rhs'Length;
      X   : Gdouble_Array (0 .. N - 1);
      Tmp : Gdouble_Array (0 .. N - 1);
      B   : Gdouble := 2.0;
   begin
      X (0) := Rhs (0) / B;
      for I in 1 .. N - 1 loop
         Tmp (I) := 1.0 / B;
         B := (if I < N - 1 then 4.0 else 3.5) - Tmp (I);
         X (I) := (Rhs (I) - X (I - 1)) / B;
      end loop;
      for I in 1 .. N - 1 loop
         X (N - I - 1) := X (N - I - 1) - Tmp (N - I) * X (N - I);
      end loop;
      return X;
   end Solve;

   --------------------------
   -- Cubic Bezier Spline  --
   --------------------------

   function Cubic_Bezier_Spline (Knots  : Point_Array)
                                 return Point_Array is
      N : constant Integer := Knots'Length - 1;  --  num. of segments
   begin
      if N <= 1 then
         raise Program_Error with "At least 3 nodes are required";
      end if;
      declare
         P1  : Point_Array (0 .. N - 1);
         P2  : Point_Array (0 .. N - 1);
         Rhs : Gdouble_Array (0 .. N - 1);
         X   : Gdouble_Array (0 .. N - 1);
         Y   : Gdouble_Array (0 .. N - 1);
      begin
         for I in 1 .. N - 2 loop
            Rhs (I) := (4.0 * Knots (Knots'First + I).X
                        + 2.0 * Knots (Knots'First + I + 1).X);
         end loop;
         Rhs (0) := Knots (Knots'First).X + 2.0 * Knots (Knots'First + 1).X;
         Rhs (N - 1) := (8.0 * Knots (Knots'First + N - 1).X
                         + Knots (Knots'First + N).X) / 2.0;
         X := Solve (Rhs);

         for I in 1 .. N - 2 loop
            Rhs (I) := (4.0 * Knots (Knots'First + I).Y
                        + 2.0 * Knots (Knots'First + I + 1).Y);
         end loop;
         Rhs (0) := Knots (Knots'First).Y + 2.0 * Knots (Knots'First + 1).Y;
         Rhs (N - 1) := (8.0 * Knots (Knots'First + N - 1).Y
                         + Knots (Knots'First + N).Y) / 2.0;
         Y := Solve (Rhs);

         for I in 0 .. N - 1 loop
            P1 (I) := (X (I), Y (I));
         end loop;
         for I in 0 .. N - 1 loop
            if I < N - 1 then
               P2 (I) := 2.0 * Knots (Knots'First + I + 1) - P1 (I + 1);
            else
               P2 (N - 1) := (Knots (Knots'First + N) + P1 (N - 1)) / 2.0;
            end if;
         end loop;

         --  copy and return
         declare
            WP : Point_Array (0 .. N * 3);
         begin
            for Segment in 0 .. N - 1 loop
               WP (Segment * 3) := Knots (Knots'First + Segment);
            end loop;
            WP (WP'Last) := Knots (Knots'Last);

            for Segment in 0 .. N - 1 loop
               WP (Segment * 3 + 1) := P1 (Segment);
               WP (Segment * 3 + 2) := P2 (Segment);
            end loop;
            return WP;
         end;
      end;
   end Cubic_Bezier_Spline;
end Gtkada.Style_Pkg.Cubic_Bezier;
