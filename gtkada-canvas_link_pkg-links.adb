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
with Ada.Numerics.Generic_Elementary_Functions;
with Cairo; use Cairo;

with Gtkada.Canvas_View_Pkg.Astar;   use Gtkada.Canvas_View_Pkg.Astar;
with Gtkada.Canvas_View_Pkg.Objects; use Gtkada.Canvas_View_Pkg.Objects;
with Gtkada.Style_Pkg;               use Gtkada.Style_Pkg;
with Gtkada.Canvas_Rect_Item_Pkg;    use Gtkada.Canvas_Rect_Item_Pkg;

with Gtkada.Canvas_Text_Item_Pkg; use Gtkada.Canvas_Text_Item_Pkg;

with Gtkada.Style_Pkg.Cubic_Bezier; use Gtkada.Style_Pkg.Cubic_Bezier;
with Gtkada.Canvas_Link_Pkg.Best_Route_Pkg;
use Gtkada.Canvas_Link_Pkg.Best_Route_Pkg;

package body Gtkada.Canvas_Link_Pkg.Links is

   package Gdouble_Elementary_Functions is new Ada.Numerics
     .Generic_Elementary_Functions
     (Gdouble);
   use Gdouble_Elementary_Functions;

   type End_Info is record
      Box : Model_Rectangle;
      --  The bounding box for the end element of the link

      Toplevel : Model_Rectangle;
      --  The bounding box for the toplevel element that contains the source
      --  element.

      P : Model_Point;
      --  Anchor points for each end of the link, on the toplevel box
   end record;

   type Anchors is record
      From, To : End_Info;
   end record;
   --  Various dimensions computed for the layout of links.
   --  It contains the start and end points for the link, as well as the
   --  bounding boxes.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Item_Point_Array, Item_Point_Array_Access);

   procedure Sort (Arr : in out Gdouble_Array);
   --  Sort the array

   function Compute_Anchors
     (Self : not null access Canvas_Link_Record'Class) return Anchors;
   --  Compute the start and end point of the link, depending on the two
   --  objects it links. This makes sure that the link does not start in the
   --  middle of the box.

   procedure Compute_Labels
     (Self : not null access Canvas_Link_Record'Class; Context : Draw_Context;
      Dim  : Anchors);
   --  Compute the position for the end labels and middle label for the link

   type Layout_Matrix is record
      X, Y : Gdouble_Array (1 .. 7);
      Dim  : Anchors;
   end record;
   --  Mapping from integer to double: the double values indicate positions
   --  in the canvas itself. The integer coordinates are used for the A*
   --  algorithm, since we are working on a non-uniform grid.

   function Manhattan_Dist
     (Self : Layout_Matrix; From, To : Coordinate) return Integer;
   function Manhattan_Cost
     (Self : Layout_Matrix; Parent, From, To : Coordinate) return Integer;
   function Next_Point is new Manhattan_Next_Point (Layout_Matrix);
   --  Functions for the A* algorithm.

   function Relative_To_Item
     (From : Model_Point; Wp : Item_Point_Array) return Model_Point;
   function Relative_To_Array
     (From : Item_Point; Wp : Item_Point_Array) return Item_Point_Array;
   --  Compute the points after applying all the relative coordinates
   --  from Wp

   procedure Compute_Side
     (Link   :     not null access Canvas_Link_Record'Class;
      Anchor :     Anchor_Attachment; Info : End_Info; Margin : Margins;
      P, P2  : out Item_Point);
   --  Compute where a link should point to (for one of its ends).
   --  (P, P2) is the first segment of an orthogonal link exiting from the
   --  box. P2 is aligned on a grid of size B, at a distance Margin from P,
   --  so that we can use Astar

   procedure Orthogonal_Waypoints
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context;
      Min_Margin_From : Gdouble; Max_Margin_From : Gdouble;
      Min_Margin_To   : Gdouble; Max_Margin_To : Gdouble);
   --  Min_Margin: we do not want lines to be displayed too close to an item,
   --  Min_Margin: we do not want lines to be displayed too close to an item,
   --  this is the minimal distance.
   --
   --  Max_Margin: On the other hand, we do not want the link to be too far
   --  either, so that for instance if Item1 is linked to both Item2 and Item3,
   --  the two link will share the middle line even when the items are not
   --  aligned:
   --
   --           Item1
   --             |
   --       ------+--------
   --       |             |
   --      Item2          |
   --                   Item3
   --
   --  If the position of the horizontal line only depends on the distance
   --  between Item1 and either Item2 or Item3, it would only be shared
   --  if Item2 and Item3 are at the same y coordinate.

   --------------------
   -- Manhattan_Dist --
   --------------------

   function Manhattan_Dist
     (Self : Layout_Matrix; From, To : Coordinate) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return abs (From.X - To.X) + abs (From.Y - To.Y);
   end Manhattan_Dist;

   --------------------
   -- Manhattan_Cost --
   --------------------

   function Manhattan_Cost
     (Self : Layout_Matrix; Parent, From, To : Coordinate) return Integer
   is
   begin
      if To.X not in Self.X'Range or else To.Y not in Self.Y'Range

         --  If the target is not a valid destination

        or else abs (Self.X (To.X) - Gdouble'Last) < 0.01
        or else abs (Self.Y (To.Y) - Gdouble'Last) < 0.01
      then
         return Not_Traversable;

      elsif Point_In_Rect
          (Self.Dim.From.Toplevel, (Self.X (To.X), Self.Y (To.Y)))
        or else Point_In_Rect
          (Self.Dim.To.Toplevel, (Self.X (To.X), Self.Y (To.Y)))
      then
         return 1_000_000; --  very expensive, but not impossible if we have to

      else
         --  A bend is costly

         if (Parent.X = From.X and then From.X /= To.X)
           or else (Parent.Y = From.Y and then From.Y /= To.Y)
         then
            return 100_000;
         else
            return 1;
         end if;
      end if;
   end Manhattan_Cost;

   function Astar_Find is new Find_Path
     (User_Data  => Layout_Matrix, Heuristic_Cost => Manhattan_Cost,
      Next_Point => Next_Point, Heuristic_Dist => Manhattan_Dist);
   --  An A* algorithm to find the shortest path between two points,
   --  while avoiding overlapping with two boxes specified in the
   --  Layout_Matrix.

   ----------------------
   -- Relative_To_Item --
   ----------------------

   function Relative_To_Item
     (From : Model_Point; Wp : Item_Point_Array) return Model_Point
   is
      C : Model_Point := From;
   begin
      for W in Wp'Range loop
         C := (C.X + Wp (W).X, C.Y + Wp (W).Y);
      end loop;
      return C;
   end Relative_To_Item;

   -----------------------
   -- Relative_To_Array --
   -----------------------

   function Relative_To_Array
     (From : Item_Point; Wp : Item_Point_Array) return Item_Point_Array
   is
      Result : Item_Point_Array (Wp'Range);
   begin
      Result (Result'First) :=
        (From.X + Wp (Wp'First).X, From.Y + Wp (Wp'First).Y);
      for P in Wp'First + 1 .. Wp'Last loop
         Result (P) :=
           (Result (P - 1).X + Wp (P).X, Result (P - 1).Y + Wp (P).Y);
      end loop;
      return Result;
   end Relative_To_Array;

   ---------------------
   -- Compute_Anchors --
   ---------------------

   function Compute_Anchors
     (Self : not null access Canvas_Link_Record'Class) return Anchors
   is
      S      : constant Abstract_Item := Self.From;
      ST     : Abstract_Item          := Canvas_View_Pkg.Objects.Toplevel (S);
      D      : constant Abstract_Item := Self.To;
      DT     : Abstract_Item          := Canvas_View_Pkg.Objects.Toplevel (D);
      P      : Model_Point;
      Item   : Abstract_Item;
      Result : Anchors;
      L      : Gdouble;
   begin
      if ST = null then
         ST := S;
      end if;

      if DT = null then
         DT := D;
      end if;

      Result :=
        (From =>
           (P   => S.Item_To_Model (S.Link_Anchor_Point (Self.Anchor_From)),
            Box => S.Model_Bounding_Box, Toplevel => ST.Model_Bounding_Box),
         To =>
           (P   => D.Item_To_Model (D.Link_Anchor_Point (Self.Anchor_To)),
            Box => D.Model_Bounding_Box, Toplevel => DT.Model_Bounding_Box));

      --  Clip the line to the side of the toplevel boxes.

      if Self.Anchor_From.Toplevel_Side = Auto then

         --  Find the topmost non-invisible parent
         Item := S;
         ST   := Item;
         while Item /= null loop
            if not Item.Is_Invisible then
               ST := Item;
            end if;
            Item := Item.Parent;
         end loop;
         Result.From.Toplevel := ST.Model_Bounding_Box;

         if Self.Waypoints = null then
            P := Result.To.P;
         elsif Self.Relative_Waypoints then
            P := Result.From.P;
            P :=
              Self.Item_To_Model
                ((P.X + Self.Waypoints (Self.Waypoints'First).X,
                  P.Y + Self.Waypoints (Self.Waypoints'First).Y));
         else
            P := Self.Item_To_Model (Self.Waypoints (Self.Waypoints'First));
         end if;

         Result.From.P :=
           ST.Item_To_Model
             (ST.Clip_Line
                (ST.Model_To_Item (Result.From.P), ST.Model_To_Item (P)));

         if Self.Anchor_From.Distance > 0.0 then
            L :=
              Sqrt
                ((Result.From.P.X - P.X) * (Result.From.P.X - P.X) +
                 (Result.From.P.Y - P.Y) * (Result.From.P.Y - P.Y));

            L := Self.Anchor_From.Distance / L;

            Result.From.P :=
              (Result.From.P.X + L * (P.X - Result.From.P.X),
               Result.From.P.Y + L * (P.Y - Result.From.P.Y));
         end if;
      end if;

      if Self.Anchor_To.Toplevel_Side = Auto then

         --  Find the topmost non-invisible parent
         Item := D;
         DT   := Item;
         while Item /= null loop
            if not Item.Is_Invisible then
               DT := Item;
            end if;
            Item := Item.Parent;
         end loop;
         Result.To.Toplevel := DT.Model_Bounding_Box;

         if Self.Waypoints = null then
            P := Result.From.P;
         elsif Self.Relative_Waypoints then
            P := Relative_To_Item (Result.From.P, Self.Waypoints.all);
         else
            P := Self.Item_To_Model (Self.Waypoints (Self.Waypoints'Last));
         end if;

         Result.To.P :=
           DT.Item_To_Model
             (DT.Clip_Line
                (DT.Model_To_Item (Result.To.P), DT.Model_To_Item (P)));

         if Self.Anchor_To.Distance > 0.0 then
            L :=
              Sqrt
                ((Result.To.P.X - P.X) * (Result.To.P.X - P.X) +
                 (Result.To.P.Y - P.Y) * (Result.To.P.Y - P.Y));

            L := Self.Anchor_To.Distance / L;

            Result.To.P :=
              (Result.To.P.X + L * (P.X - Result.To.P.X),
               Result.To.P.Y + L * (P.Y - Result.To.P.Y));
         end if;
      end if;

      return Result;
   end Compute_Anchors;

   --------------------------
   -- Compute_Bounding_Box --
   --------------------------

   function Compute_Bounding_Box
     (Points : Item_Point_Array; Relative : Boolean := False)
      return Item_Rectangle
   is
      Max_X   : Gdouble    := Points (Points'First).X;
      Max_Y   : Gdouble    := Points (Points'First).Y;
      X       : Gdouble    := Max_X;
      Y       : Gdouble    := Max_Y;
      Current : Item_Point := Points (Points'First);
   begin
      for P1 in Points'First + 1 .. Points'Last loop
         if Relative then
            Current := (Current.X + Points (P1).X, Current.Y + Points (P1).Y);
         else
            Current := Points (P1);
         end if;

         X     := Gdouble'Min (X, Current.X);
         Y     := Gdouble'Min (Y, Current.Y);
         Max_X := Gdouble'Max (Max_X, Current.X);
         Max_Y := Gdouble'Max (Max_Y, Current.Y);
      end loop;

      return (X => X, Y => Y, Width => Max_X - X, Height => Max_Y - Y);
   end Compute_Bounding_Box;

   --------------------
   -- Compute_Labels --
   --------------------

   procedure Compute_Labels
     (Self : not null access Canvas_Link_Record'Class; Context : Draw_Context;
      Dim  : Anchors)
   is
      P : constant Item_Point_Array_Access := Self.Points;

      procedure Place_End_Label
        (Label    : Container_Item; P1, P2 : Item_Point;
         Toplevel : Model_Rectangle);
      procedure Place_End_Label
        (Label    : Container_Item; P1, P2 : Item_Point;
         Toplevel : Model_Rectangle)
      is
         Margin : constant Gdouble := 3.0;   --  extra margin around the box
         Deltax              : constant Gdouble := P2.X - P1.X;
         Deltay              : constant Gdouble := P2.Y - P1.Y;
         D                   : Gdouble          := 1.0;
         X1, X2, Y1, Y2, Tmp : Gdouble;
      begin
         Label.Size_Request (Context);

         X1 := (Toplevel.X + Margin + Toplevel.Width - P1.X) / Deltax;
         X2 := (Toplevel.X - Margin - P1.X - Label.Get_Width) / Deltax;
         Y1 := (Toplevel.Y + Margin + Toplevel.Height - P1.Y) / Deltay;
         Y2 := (Toplevel.Y - Margin - P1.Y - Label.Get_Height) / Deltay;

         if Deltax < 0.0 then
            Tmp := X1;
            X1  := X2;
            X2  := Tmp;
         end if;

         if Deltay < 0.0 then
            Tmp := Y1;
            Y1  := Y2;
            Y2  := Y1;
         end if;

         if 0.0 <= X1 and then X1 <= 1.0 then
            D := X1;
         end if;

         if 0.0 <= X2 and then X2 <= 1.0 then
            D := Gdouble'Min (X2, D);
         end if;

         if 0.0 <= Y1 and then Y1 <= 1.0 then
            D := Gdouble'Min (Y1, D);
         end if;

         if 0.0 <= Y2 and then Y2 <= 1.0 then
            D := Gdouble'Min (Y2, D);
         end if;

         Store_Computed_Position
           (Label, (X => P1.X + D * Deltax, Y => P1.Y + D * Deltay));

         --  A small additional offset: the text is now outside of the box, we
         --  also want it outside of the link.

         if abs (Deltax) < abs (Deltay) then
            --  link is mostly vertical, so offset is horizontal
            Store_Computed_Position
              (Label, (Label.Get_Computed_Position.X + 4.0, Gdouble'First));
         else
            Store_Computed_Position
              (Label, (Gdouble'First, Label.Get_Computed_Position.Y + 4.0));
         end if;

         Label.Size_Allocate;
      end Place_End_Label;

      Deltax, Deltay : Gdouble;
      Idx            : Integer;
   begin
      --  Sets text.x and text.y to a position appropriate to show the label
      --  so that it doesn't intersect the from and to boxes. We are moving
      --  the text box along the line deltax, deltay. Basically, we are looking
      --  for d such that the two bounding boxes for the text and "from" do no
      --  intersect.
      --
      --     0 <= d <= 1    (position along the line)
      --     x + d * deltax > from.x + from.w
      --     || from.x > x + d * deltax + label.w
      --     || y + d * deltay > from.y + from.h
      --     || from.y > y + d * deltay + label.h
      --
      --  Which provides:
      --     0 <= d <= 1
      --     && (d * deltax < from.x - x - w
      --         || d * deltax > from.x + from.w - x
      --         || d * deltay < from.y - y -h
      --         || d * deltay > from.y + from.h - y)
      --
      --  We take the lowest d that satisfies any of the criteria, to keep the
      --  text closer to the box.

      if Self.Label_From /= null then
         Place_End_Label
           (Self.Label_From, P (P'First), P (P'First + 1), Dim.From.Toplevel);
      end if;

      if Self.Label_To /= null then
         Place_End_Label
           (Self.Label_To, P (P'Last), P (P'Last - 1), Dim.To.Toplevel);
      end if;

      if Self.Label /= null then
         if Self.Label.all in Text_Item_Record'Class
           and then Text_Item (Self.Label).Direction /= No_Text_Arrow
         then
            Deltax := P (P'First).X - P (P'Last).X;
            Deltay := P (P'First).Y - P (P'Last).Y;

            if abs (Deltax) > abs (Deltay) then
               if Deltax > 0.0 then
                  Set_Direction (Text_Item (Self.Label), Left_Text_Arrow);
               else
                  Set_Direction (Text_Item (Self.Label), Right_Text_Arrow);
               end if;

            else
               if Deltay > 0.0 then
                  Set_Direction (Text_Item (Self.Label), Up_Text_Arrow);
               else
                  Set_Direction (Text_Item (Self.Label), Down_Text_Arrow);
               end if;
            end if;
         end if;

         Self.Label.Size_Request (Context);

         --  Place the label in the middle of the middle segment.

         Idx := P'First + P'Length / 2 - 1;
         Store_Computed_Position
           (Self.Label,
            (X =>
               (P (Idx).X + P (Idx + 1).X) / 2.0 - Self.Label.Get_Width / 2.0,
             Y =>
               (P (Idx).Y + P (Idx + 1).Y) / 2.0 -
               Self.Label.Get_Height / 2.0));
         Self.Label.Size_Allocate;
      end if;
   end Compute_Labels;

   ----------
   -- Sort --
   ----------

   procedure Sort (Arr : in out Gdouble_Array) is
      --  A simple bubble sort
      Switched : Boolean;
      Tmp      : Gdouble;
   begin
      loop
         Switched := False;

         for J in Arr'First .. Arr'Last - 1 loop
            if Arr (J + 1) < Arr (J) then
               Tmp         := Arr (J);
               Arr (J)     := Arr (J + 1);
               Arr (J + 1) := Tmp;
               Switched    := True;
            end if;
         end loop;

         exit when not Switched;
      end loop;
   end Sort;

   -----------------------
   --  Join_Link_Points --
   -----------------------
   --  Join two item point arrays where last point if first array is
   --  equal to first point of second array

   function Join_Link_Points
     (Points0, Points1 : in out Item_Point_Array_Access)
      return Item_Point_Array_Access;
   function Join_Link_Points
     (Points0, Points1 : in out Item_Point_Array_Access)
      return Item_Point_Array_Access
   is
      T : Item_Point_Array_Access;
   begin
      if Points1 /= null then
         T :=
           new Item_Point_Array'
             (Points0 (Points0'First .. Points0'Last - 1) & Points1.all);
      else
         T := new Item_Point_Array'(Points0 (Points0'First .. Points0'Last));
      end if;
      Unchecked_Free (Points0);
      Unchecked_Free (Points1);

      return T;
   end Join_Link_Points;

   ----------------------------------------
   -- Compute_Layout_For_Orthogonal_Link --
   ----------------------------------------

   procedure Compute_Layout_For_Orthogonal_Link
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Rect_Item_Record'Class, Rect_Item);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Canvas_Link_Record'Class, Canvas_Link);
   begin
      if Link.Waypoints = null then
         Orthogonal_Waypoints
           (Link, Context, Min_Margin_From => 6.0, Max_Margin_From => 25.0,
            Min_Margin_To                  => 6.0, Max_Margin_To => 25.0);
         return;
      end if;

      declare
         Number_Of_Waypoints : constant Integer := Link.Waypoints.all'Length;
         WP                  : Item_Point_Array (0 .. Number_Of_Waypoints - 1);
         TLink               : Canvas_Link;
         From, To            : Rect_Item;
      begin
         TLink :=
           Gtk_New
             (From        => Link.From, To => Link.To, Style => Link.Style,
              Routing     => Link.Routing, Label => Link.Label,
              Anchor_From => Link.Anchor_From, Label_From => Link.Label_From,
              Anchor_To   => Link.Anchor_To, Label_To => Link.Label_To);

         if Link.Relative_Waypoints then
            WP := Relative_To_Array (Link.From.Position, Link.Waypoints.all);
         else
            WP := Link.Waypoints.all;
         end if;
         TLink.Waypoints := null;
         TLink.Points    := null;

         From :=
           Gtk_New_Rect (Style => Link.Style, Width => 0.0, Height => 0.0);
         From.Set_Size_Request (Width => 0.0, Height => 0.0);

         To := Gtk_New_Rect (Style => Link.Style, Width => 0.0, Height => 0.0);
         To.Set_Size_Request (Width => 0.0, Height => 0.0);

         TLink.To := Abstract_Item (To);

         --  start conditions
         TLink.From        := Link.From;
         TLink.Anchor_From := Link.Anchor_From;

         --  now loop
         for W of WP loop
            TLink.To.Set_Position (W);
            TLink.Anchor_To := Middle_Attachment;

            if TLink.From = Link.From then
               Orthogonal_Waypoints
                 (TLink, Context, Min_Margin_From => 6.0,
                  Max_Margin_From => 25.0, Min_Margin_To => 0.0,
                  Max_Margin_To                   => Gdouble'Last);
               TLink.From := Abstract_Item (From);
            else
               Orthogonal_Waypoints
                 (TLink, Context, Min_Margin_From => 0.0,
                  Max_Margin_From => Gdouble'Last, Min_Margin_To => 0.0,
                  Max_Margin_To                   => Gdouble'Last);
            end if;
            if Link.Points = null then
               Link.Points := Join_Link_Points (TLink.Points, Link.Points);
            else
               Link.Points := Join_Link_Points (Link.Points, TLink.Points);
            end if;

            TLink.From.Set_Position (TLink.To.Position);
            TLink.Anchor_From := TLink.Anchor_To;
         end loop;

         --  final condition
         TLink.To        := Link.To;
         TLink.Anchor_To := Link.Anchor_To;
         Orthogonal_Waypoints
           (TLink, Context, Min_Margin_From => 0.0,
            Max_Margin_From => Gdouble'Last, Min_Margin_To => 6.0,
            Max_Margin_To                   => 25.0);
         Link.Points := Join_Link_Points (Link.Points, TLink.Points);

         Unchecked_Free (From);
         Unchecked_Free (To);
         Unchecked_Free (TLink);
      end;
   end Compute_Layout_For_Orthogonal_Link;

   -----------------------------
   -- Compute_Effective_Side  --
   -----------------------------
   function Compute_Effective_Side
     (Anchor : Anchor_Attachment; Info : End_Info) return Side_Attachment;

   function Compute_Effective_Side
     (Anchor : Anchor_Attachment; Info : End_Info) return Side_Attachment
   is
   begin
      case Anchor.Toplevel_Side is
         when Top | Bottom | Right | Left =>
            return Anchor.Toplevel_Side;
         when Auto =>
            if abs (Info.P.X - Info.Toplevel.X) < 0.001 then
               return Left;
            elsif abs (Info.P.Y - Info.Toplevel.Y) < 0.001 then
               return Top;
            elsif abs (Info.P.X - Info.Toplevel.X - Info.Toplevel.Width) <
              0.001
            then
               return Right;
            else
               return Bottom;
            end if;
         when No_Clipping =>
            return No_Clipping;
      end case;
   end Compute_Effective_Side;

   ------------------
   -- Compute_Side --
   ------------------

   procedure Compute_Side
     (Link   :     not null access Canvas_Link_Record'Class;
      Anchor :     Anchor_Attachment; Info : End_Info; Margin : Margins;
      P, P2  : out Item_Point)
   is
      Effective_Side : Side_Attachment;
   begin
      Effective_Side := Compute_Effective_Side (Anchor, Info);
      case Effective_Side is
         when Top =>
            P :=
              (Info.Box.X + Info.Box.Width * abs (Anchor.X), Info.Toplevel.Y);
            P2 := (P.X, P.Y - Margin.Top);
         when Bottom =>
            P :=
              (Info.Box.X + Info.Box.Width * abs (Anchor.X),
               Info.Toplevel.Y + Info.Toplevel.Height);
            P2 := (P.X, P.Y + Margin.Bottom);
         when Right =>
            P :=
              (Info.Toplevel.X + Info.Toplevel.Width,
               Info.Box.Y + Info.Box.Height * abs (Anchor.Y));
            P2 := (P.X + Margin.Right, P.Y);
         when Left =>
            P :=
              (Info.Toplevel.X, Info.Box.Y + Info.Box.Height * abs (Anchor.Y));
            P2 := (P.X - Margin.Left, P.Y);
         when others =>
            P := Link.Model_To_Item (Info.P);
      end case;
   end Compute_Side;

   ---------------------------
   -- Margins_Between_Items --
   ---------------------------

   procedure Margins_Between_Items
     (Item1, Item2    : Model_Rectangle; Min_Margin_From : Gdouble;
      Max_Margin_From : Gdouble; Min_Margin_To : Gdouble;
      Max_Margin_To   : Gdouble; Margin1, Margin2 : out Margins);
   --  When computing the layout for orthogonal links, we add some borders
   --  around the items, through which the link might go. These borders are
   --  larger between the two items.

   procedure Margins_Between_Items
     (Item1, Item2    : Model_Rectangle; Min_Margin_From : Gdouble;
      Max_Margin_From : Gdouble; Min_Margin_To : Gdouble;
      Max_Margin_To   : Gdouble; Margin1, Margin2 : out Margins)
   is
      pragma Unreferenced (Max_Margin_To);
      Dist, Tmp : Gdouble;
   begin
      if Item1.X + Item1.Width < Item2.X then
         Dist          := Item2.X - Item1.X - Item1.Width;
         Margin1.Left  := Min_Margin_From;
         Tmp           := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Right := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Left  := Dist - Margin1.Right;
         Margin2.Right := Min_Margin_To;

      elsif Item2.X + Item2.Width < Item1.X then
         Dist          := Item1.X - Item2.X - Item2.Width;
         Margin1.Right := Min_Margin_From;
         Tmp           := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Left  := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Right := Dist - Margin1.Left;
         Margin2.Left  := Min_Margin_To;

      else
         Margin1.Left  := Min_Margin_From;
         Margin1.Right := Min_Margin_From;
         Margin2.Left  := Min_Margin_To;
         Margin2.Right := Min_Margin_To;
      end if;

      if Item1.Y + Item1.Height < Item2.Y then
         Dist           := Item2.Y - Item1.Y - Item1.Height;
         Margin1.Top    := Min_Margin_From;
         Tmp            := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Bottom := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Top    := Dist - Margin1.Bottom;
         Margin2.Bottom := Min_Margin_To;

      elsif Item2.Y + Item2.Height < Item1.Y then
         Dist           := Item1.Y - Item2.Y - Item2.Height;
         Margin1.Bottom := Min_Margin_From;
         Tmp            := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Top    := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Bottom := Dist - Margin1.Top;
         Margin2.Top    := Min_Margin_To;

      else
         Margin1.Top    := Min_Margin_From;
         Margin1.Bottom := Min_Margin_From;
         Margin2.Top    := Min_Margin_To;
         Margin2.Bottom := Min_Margin_To;
      end if;
   end Margins_Between_Items;

   --------------------------
   -- Orthogonal_Waypoints --
   --------------------------

   procedure Orthogonal_Waypoints
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context;
      Min_Margin_From : Gdouble; Max_Margin_From : Gdouble;
      Min_Margin_To   : Gdouble; Max_Margin_To : Gdouble)
   is

      Min_Space : constant Gdouble := Link.Style.Get_Line_Width * 3.0;
      --  Minimal space between two boxes to pass a link between them

      Dim : constant Anchors := Compute_Anchors (Link);

      FTX1 : constant Gdouble := Dim.From.Toplevel.X;  --  from-top-x1
      FTX2 : constant Gdouble := FTX1 + Dim.From.Toplevel.Width;
      FTY1 : constant Gdouble := Dim.From.Toplevel.Y;  --  from-top-y1
      FTY2 : constant Gdouble := FTY1 + Dim.From.Toplevel.Height;

      TTX1 : constant Gdouble := Dim.To.Toplevel.X;  --  from-top-x1
      TTX2 : constant Gdouble := TTX1 + Dim.To.Toplevel.Width;
      TTY1 : constant Gdouble := Dim.To.Toplevel.Y;  --  from-top-y1
      TTY2 : constant Gdouble := TTY1 + Dim.To.Toplevel.Height;

      From, To               : Item_Point;
      M                      : Item_Point;
      P_From                 : Item_Point;   --  extending from the From box
      P_To                   : Item_Point;   --  extending from the To box
      C1, C2, C3             : Coordinate;
      Matrix                 : Layout_Matrix;
      Margin_From, Margin_To : Margins;

   begin  --  Orthogonal_Waypoints
      Margins_Between_Items
        (Dim.From.Toplevel, Dim.To.Toplevel, Min_Margin_From, Max_Margin_From,
         Min_Margin_To, Max_Margin_To, Margin_From, Margin_To);
      Compute_Side
        (Link, Link.Anchor_From, Dim.From, Margin_From, From, P_From);
      Compute_Side (Link, Link.Anchor_To, Dim.To, Margin_To, To, P_To);

      Unchecked_Free (Link.Points);

      if Link.Waypoints /= null then
         if Link.Relative_Waypoints then
            Link.Points :=
              new Item_Point_Array'
                (From & Relative_To_Array (From, Link.Waypoints.all) & To);
         else
            Link.Points :=
              new Item_Point_Array'(From & Link.Waypoints.all & To);
         end if;

         --  Is the final link supposed to be vertical ?
         --  And can we make it fully vertical, if the user requested it ?

         if abs (To.X - P_To.X) < 0.001 and then Link.Anchor_To.X < 0.0
           and then Link.Points (Link.Points'Last - 1).X >= Dim.To.Box.X
           and then Link.Points (Link.Points'Last - 1).X <=
             Dim.To.Box.X + Dim.To.Box.Width
         then
            Link.Points (Link.Points'Last).X :=
              Link.Points (Link.Points'Last - 1).X;

            --  Is the final link supposed to be horizontal ?
            --  And can we make it fully vertical, if the user requested it ?

         elsif abs (To.Y - P_To.Y) < 0.001 and then Link.Anchor_To.Y < 0.0
           and then Link.Points (Link.Points'Last - 1).Y >= Dim.To.Box.Y
           and then Link.Points (Link.Points'Last - 1).Y <=
             Dim.To.Box.Y + Dim.To.Box.Height
         then
            Link.Points (Link.Points'Last).Y :=
              Link.Points (Link.Points'Last - 1).Y;
         end if;

         Link.Bounding_Box := Compute_Bounding_Box (Link.Points.all);
         Compute_Labels (Link, Context, Dim);

         return;
      end if;

      --  We want to find a line that joins P_From to P_To, with the minimum
      --  number of bends. For this, there are in fact three possible X
      --  coordinates for each box (and same for Y):
      --          FTX1 - B, F.X, FTX2 + B
      --                TTX1 - B, T.X, TTX2 + B
      --
      --  When there is enough space between the two boxes, we can replace
      --  the two lines between the items with a single one in the middle.
      --  This gives use 5 or 6 possible X coordinates for vertical segments.
      --  Some of these cannot be used to go all the way down because they
      --  cross one of the two boxes.
      --  Also, P_From and P_To are aligned on that grid, so basically we need
      --  to find a path between two vertices of the grid.

      if FTX2 + Min_Space <= TTX1 then
         M.X := (FTX2 + TTX1) / 2.0;
      elsif TTX2 + Min_Space <= FTX1 then
         M.X := (FTX1 + TTX2) / 2.0;
      else
         M.X := Gdouble'Last;  --  no valid vertical line in between
      end if;

      if FTY2 + Min_Space <= TTY1 then
         M.Y := (FTY2 + TTY1) / 2.0;
      elsif TTY2 + Min_Space <= FTY1 then
         M.Y := (FTY1 + TTY2) / 2.0;
      else
         M.Y := Gdouble'Last;  --  no valid horizontal line in between
      end if;

      Matrix :=
        (Dim => Dim,
         X   =>
           ((FTX1 - Margin_From.Left, From.X, FTX2 + Margin_From.Right,
             TTX1 - Margin_To.Left, To.X, TTX2 + Margin_To.Right, M.X)),
         Y =>
           ((FTY1 - Margin_From.Top, From.Y, FTY2 + Margin_From.Bottom,
             TTY1 - Margin_To.Top, To.Y, TTY2 + Margin_To.Bottom, M.Y)));
      Sort (Matrix.X);
      Sort (Matrix.Y);

      for J in Matrix.X'Range loop
         if abs (Matrix.X (J) - P_From.X) < 0.01 then
            C1.X := J;
         end if;
         if abs (Matrix.X (J) - P_To.X) < 0.01 then
            C2.X := J;
         end if;
         if abs (Matrix.X (J) - From.X) < 0.01 then
            C3.X := J;
         end if;

         if abs (Matrix.Y (J) - P_From.Y) < 0.01 then
            C1.Y := J;
         end if;
         if abs (Matrix.Y (J) - P_To.Y) < 0.01 then
            C2.Y := J;
         end if;
         if abs (Matrix.Y (J) - From.Y) < 0.01 then
            C3.Y := J;
         end if;
      end loop;

      --  Compute the shortest path between the two points C1 and C2. We also
      --  do some post-processing to limit the number of segments (which in
      --  turn speeds up drawing and click

      declare
         Path : constant Coordinate_Array :=
           Astar_Find (Matrix, C1, C2, Parent => C3);
         Points  : Item_Point_Array (1 .. 4 + Path'Length);
         P       : Integer;
         Along_X : Boolean;

         procedure Add_Point (New_P : Item_Point);
         --  Add a new point to the final path, or merge with the previous
         --  point if possible

         procedure Add_Point (New_P : Item_Point) is
            Tmp : Boolean;
         begin
            Tmp := abs (New_P.X - Points (P - 1).X) < 0.01;
            if Tmp = Along_X then
               --  both along the X axis => merge points
               Points (P - 1) := New_P;
            else
               --  Validate the new point
               Along_X    := Tmp;
               Points (P) := New_P;
               P          := P + 1;
            end if;
         end Add_Point;

      begin
         Points (Points'First) := From;
         P                     := Points'First + 1;
         Points (P)            := P_From;
         Along_X               := abs (Points (P).X - Points (P - 1).X) < 0.01;
         P                     := P + 1;

         for C in Path'First + 1 .. Path'Last loop
            Add_Point ((Matrix.X (Path (C).X), Matrix.Y (Path (C).Y)));
         end loop;

         Add_Point (To);

         Link.Points := new Item_Point_Array'(Points (Points'First .. P - 1));
      end;

      Link.Bounding_Box := Compute_Bounding_Box (Link.Points.all);
      Compute_Labels (Link, Context, Dim);
   end Orthogonal_Waypoints;

   --------------------------------------
   -- Compute_Layout_For_Straight_Link --
   --------------------------------------

   procedure Compute_Layout_For_Straight_Link
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context)
   is
      Dim       : constant Anchors := Compute_Anchors (Link);
      Tmp, Tmp2 : Item_Point;
      Pos       : Item_Coordinate;
   begin
      if Link.From = Link.To then
         Tmp :=
           (Dim.From.Toplevel.X + Dim.From.Toplevel.Width,
            Dim.From.Toplevel.Y);
         Unchecked_Free (Link.Points);
         Link.Points :=
           new Item_Point_Array'
             ((Tmp.X - 10.0, Tmp.Y), (Tmp.X - 10.0, Tmp.Y - 10.0),
              (Tmp.X + 10.0, Tmp.Y - 10.0), (Tmp.X + 10.0, Tmp.Y + 10.0),
              (Tmp.X, Tmp.Y + 10.0));

      elsif Link.Waypoints /= null then
         Unchecked_Free (Link.Points);
         Tmp := Link.Model_To_Item (Dim.To.P);

         if Link.Relative_Waypoints then
            Tmp2        := Link.Model_To_Item (Dim.From.P);
            Link.Points :=
              new Item_Point_Array'
                (Tmp2 & Relative_To_Array (Tmp2, Link.Waypoints.all) & Tmp);
         else
            Link.Points :=
              new Item_Point_Array'
                (Link.Model_To_Item (Dim.From.P) & Link.Waypoints.all & Tmp);
         end if;

      else
         Unchecked_Free (Link.Points);
         Link.Points :=
           new Item_Point_Array'
             (Link.Model_To_Item (Dim.From.P) & Link.Model_To_Item (Dim.To.P));
      end if;

      --  Is the user asking to get the link fully vertical when possible.
      --  This only applies to the last segment.

      if Link.Anchor_To.X < 0.0 or else Link.Anchor_To.Y < 0.0 then
         Tmp2 := Link.Points (Link.Points'Last);
         Tmp  := Link.Points (Link.Points'Last - 1);

         if Link.Anchor_To.X < 0.0 and then Tmp.X >= Dim.To.Box.X
           and then Tmp.X <= Dim.To.Box.X + Dim.To.Box.Width
         then
            Pos                                  := (Tmp.X + Tmp2.X) / 2.0;
            Link.Points (Link.Points'Last - 1).X := Pos;
            Link.Points (Link.Points'Last).X     := Pos;

         elsif Link.Anchor_To.Y < 0.0 and then Tmp.Y >= Dim.To.Box.Y
           and then Tmp.Y <= Dim.To.Box.Y + Dim.To.Box.Height
         then
            Pos                                  := (Tmp.Y + Tmp2.Y) / 2.0;
            Link.Points (Link.Points'Last - 1).Y := Pos;
            Link.Points (Link.Points'Last).Y     := Pos;
         end if;
      end if;

      Link.Bounding_Box :=
        Compute_Bounding_Box (Link.Points.all, Relative => False);
      Compute_Labels (Link, Context, Dim);
   end Compute_Layout_For_Straight_Link;

   -------------------------------------------
   -- Compute Side Attachment_For_Line_Link --
   -------------------------------------------

   procedure Compute_Side_Attachment_For_Line_Link
     (Link      :     not null access Canvas_Link_Record'Class;
      Side_From : out Side_Anchor_Style; P_From : out Point;
      Side_To   : out Side_Anchor_Style; P_To : out Point);

   procedure Compute_Side_Attachment_For_Line_Link
     (Link      :     not null access Canvas_Link_Record'Class;
      Side_From : out Side_Anchor_Style; P_From : out Point;
      Side_To   : out Side_Anchor_Style; P_To : out Point)
   is
      Dim    : constant Anchors := Compute_Anchors (Link);
      Displ  : Gdouble;
      X0, Y0 : Gdouble;
   begin
      if Link.Routing = Line then
         Displ := Link.Offset + Link.Style.Get_Arrow_From.Length;
      else
         raise Program_Error;
      end if;
      case Link.Anchor_From.Toplevel_Side is
         when Top =>
            Side_From := Top;
            P_From    := (Dim.From.P.X, Dim.From.P.Y) + (0.0, -Displ);
         when Right =>
            Side_From := Right;
            P_From    := (Dim.From.P.X, Dim.From.P.Y) + (Displ, 0.0);
         when Bottom =>
            Side_From := Bottom;
            P_From    := (Dim.From.P.X, Dim.From.P.Y) + (0.0, Displ);
         when Left =>
            Side_From := Left;
            P_From    := (Dim.From.P.X, Dim.From.P.Y) + (-Displ, 0.0);
         when No_Clipping =>
            Side_From := Not_Defined;
            P_From    := (Dim.From.P.X, Dim.From.P.Y);
         when Auto =>
            if Dim.From.Toplevel.X = Dim.From.P.X then
               Side_From := Left;
               Y0 := Dim.From.Toplevel.Y + Dim.From.Toplevel.Height / 2.0;
               P_From    := (Dim.From.P.X, Y0) + (-Displ, 0.0);
            elsif Dim.From.Toplevel.X + Dim.From.Toplevel.Width = Dim.From.P.X
            then
               Side_From := Right;
               Y0 := Dim.From.Toplevel.Y + Dim.From.Toplevel.Height / 2.0;
               P_From    := (Dim.From.P.X, Y0) + (Displ, 0.0);
            elsif Dim.From.Toplevel.Y = Dim.From.P.Y then
               Side_From := Top;
               X0 := Dim.From.Toplevel.X + Dim.From.Toplevel.Width / 2.0;
               P_From    := (X0, Dim.From.P.Y) + (0.0, -Displ);
            elsif Dim.From.Toplevel.Y + Dim.From.Toplevel.Height = Dim.From.P.Y
            then
               Side_From := Bottom;
               X0 := Dim.From.Toplevel.X + Dim.From.Toplevel.Width / 2.0;
               P_From    := (X0, Dim.From.P.Y) + (0.0, Displ);
            else
               Side_From := Not_Defined;
               P_From    := (Dim.From.P.X, Dim.From.P.Y);
            end if;
      end case;
      case Link.Anchor_To.Toplevel_Side is
         when Top =>
            Side_To := Top;
            P_To    := (Dim.To.P.X, Dim.To.P.Y) + (0.0, -Displ);
         when Right =>
            Side_To := Right;
            P_To    := (Dim.To.P.X, Dim.To.P.Y) + (Displ, 0.0);
         when Bottom =>
            Side_To := Bottom;
            P_To    := (Dim.To.P.X, Dim.To.P.Y) + (0.0, Displ);
         when Left =>
            Side_To := Left;
            P_To    := (Dim.To.P.X, Dim.To.P.Y) + (-Displ, 0.0);
         when No_Clipping =>
            Side_To := Not_Defined;
            P_To    := (Dim.To.P.X, Dim.To.P.Y);
         when Auto =>
            if Dim.To.Toplevel.X = Dim.To.P.X then
               Side_To := Left;
               Y0      := Dim.To.Toplevel.Y + Dim.To.Toplevel.Height / 2.0;
               P_To    := (Dim.To.P.X, Y0) + (-Displ, 0.0);
            elsif Dim.To.Toplevel.X + Dim.To.Toplevel.Width = Dim.To.P.X then
               Side_To := Right;
               Y0      := Dim.To.Toplevel.Y + Dim.To.Toplevel.Height / 2.0;
               P_To    := (Dim.To.P.X, Y0) + (Displ, 0.0);
            elsif Dim.To.Toplevel.Y = Dim.To.P.Y then
               Side_To := Top;
               X0      := Dim.To.Toplevel.X + Dim.To.Toplevel.Width / 2.0;
               P_To    := (X0, Dim.To.P.Y) + (0.0, -Displ);
            elsif Dim.To.Toplevel.Y + Dim.To.Toplevel.Height = Dim.To.P.Y then
               Side_To := Bottom;
               X0      := Dim.To.Toplevel.X + Dim.To.Toplevel.Width / 2.0;
               P_To    := (X0, Dim.To.P.Y) + (0.0, Displ);
            else
               Side_To := Not_Defined;
               P_To    := (Dim.To.P.X, Dim.To.P.Y);
            end if;
      end case;
   end Compute_Side_Attachment_For_Line_Link;

   ------------
   -- Get_Wp --
   ------------

   function Get_Wp
     (Link : not null access Canvas_Link_Record'Class) return Item_Point_Array;
   --  Return the waypoints to use

   function Get_Wp
     (Link : not null access Canvas_Link_Record'Class) return Item_Point_Array
   is
   begin
      if Link.From = Link.To then
         return No_Waypoints;
      elsif Link.Waypoints /= null then
         return Link.Waypoints.all;
      else
         return No_Waypoints;
      end if;
   end Get_Wp;

   ----------------------------------
   -- Compute_Layout_For_Line_Link --
   ----------------------------------

   procedure Compute_Layout_For_Line_Link
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context;
      Offset : Gdouble := 10.0)
   is
      Dim           : constant Anchors  := Compute_Anchors (Link);
      Waypoints     : Item_Point_Array  := Get_Wp (Link);
      P1, P2        : Item_Point;
      FP, TP, Dummy : Item_Point;
      Radius, Tmp   : Gdouble;
      Side_From     : Side_Anchor_Style := Not_Defined;
      Side_To       : Side_Anchor_Style := Not_Defined;
   begin
      Unchecked_Free (Link.Points);

      if Link.From = Link.To then
         Tmp    := Gdouble'Min (abs (Offset), Dim.From.Toplevel.Width / 2.0);
         Radius := Gdouble'Min (Tmp, Dim.From.Toplevel.Height / 2.0);

         Link.Points :=
           new Item_Point_Array'
             (Circle_From_Bezier
                (Center =>
                   Link.Model_To_Item
                     ((Dim.From.Toplevel.X + Dim.From.Toplevel.Width,
                       Dim.From.Toplevel.Y)),
                 Radius => Radius));

      else
         Compute_Side
           (Link, Link.Anchor_From, Dim.From, No_Margins, FP, Dummy);
         Compute_Side (Link, Link.Anchor_To, Dim.To, No_Margins, TP, Dummy);

         if Waypoints'Length = 0 then
            Compute_Side_Attachment_For_Line_Link (Link, Side_From, P1, Side_To, P2);
            Link.Points := new Item_Point_Array'((FP, P1, P2, TP));
         else
            Compute_Side_Attachment_For_Line_Link (Link, Side_From, P1, Side_To, P2);

            if Link.Relative_Waypoints then
               Dummy     := Link.Model_To_Item (Dim.From.P);
               Waypoints := Relative_To_Array (Dummy, Waypoints);
            end if;

            Link.Points :=
              new Item_Point_Array'
                (Item_Point_Array'(FP, P1) & Waypoints &
                 Item_Point_Array'(P2, TP));
         end if;
      end if;

      Link.Bounding_Box :=
        Compute_Bounding_Box (Link.Points.all, Relative => False);
      Compute_Labels (Link, Context, Dim);
   end Compute_Layout_For_Line_Link;

   -------------------
   -- Continue_Side --
   -------------------
   function Continue_Side (Side : Side_Anchor_Style) return Side_Anchor_Style;
   function Continue_Side (Side : Side_Anchor_Style) return Side_Anchor_Style
   is
   begin
      case Side is
         when Top =>
            return Bottom;
         when Bottom =>
            return Top;
         when Right =>
            return Left;
         when Left =>
            return Right;
         when Not_Defined =>
            return Not_Defined;
      end case;
   end Continue_Side;

   ------------------------------
   -- Compute Side Attachment4 --
   ------------------------------

   procedure Compute_Side_Attachment4
     (Link           : not null access Canvas_Link_Record'Class;
      Prev_Side_From : Side_Anchor_Style;
      Side_From      : out Side_Anchor_Style;
      Side_To        : out Side_Anchor_Style);

   procedure Compute_Side_Attachment4
     (Link           : not null access Canvas_Link_Record'Class;
      Prev_Side_From : Side_Anchor_Style;
      Side_From      : out Side_Anchor_Style;
      Side_To        : out Side_Anchor_Style)
   is
      Dim     : constant Anchors := Compute_Anchors (Link);
   begin

      case Link.Anchor_From.Toplevel_Side is
         when Left =>
            Side_From := Left;
         when Right =>
            Side_From := Right;
         when Top =>
            Side_From := Top;
         when Bottom =>
            Side_From := Bottom;
         when No_Clipping =>
            Side_From := Not_Defined;
         when Auto =>
            if Dim.From.Toplevel.Width = 0.0 and Dim.From.Toplevel.Height = 0.0
            then
               Side_From := Continue_Side (Prev_Side_From);
            else
               if Dim.From.Toplevel.X = Dim.From.P.X then
                  Side_From := Left;
               elsif Dim.From.Toplevel.X + Dim.From.Toplevel.Width =
                 Dim.From.P.X
               then
                  Side_From := Right;
               elsif Dim.From.Toplevel.Y = Dim.From.P.Y then
                  Side_From := Top;
               elsif Dim.From.Toplevel.Y + Dim.From.Toplevel.Height =
                 Dim.From.P.Y
               then
                  Side_From := Bottom;
               else
                  Side_From := Not_Defined;
               end if;
            end if;
      end case;
      case Link.Anchor_To.Toplevel_Side is
         when Left =>
            Side_To := Left;
         when Right =>
            Side_To := Right;
         when Top =>
            Side_To := Top;
         when Bottom =>
            Side_To := Bottom;
         when No_Clipping =>
            Side_To := Not_Defined;
         when Auto =>
            if Dim.To.Toplevel.Width = 0.0 and Dim.To.Toplevel.Height = 0.0
            then
               Side_To := Not_Defined;
            else
               if Dim.To.Toplevel.X = Dim.To.P.X then
                  Side_To := Left;
               elsif Dim.To.Toplevel.X + Dim.To.Toplevel.Width = Dim.To.P.X
               then
                  Side_To := Right;
               elsif Dim.To.Toplevel.Y = Dim.To.P.Y then
                  Side_To := Top;
               elsif Dim.To.Toplevel.Y + Dim.To.Toplevel.Height = Dim.To.P.Y
               then
                  Side_To := Bottom;
               else
                  Side_To := Not_Defined;
               end if;
            end if;
      end case;
   end Compute_Side_Attachment4;

   ----------------------
   -- Margins_Of_Items --
   ----------------------

   procedure Margins_Of_Items
     (Item1, Item2    : Model_Rectangle; Min_Margin_From : Gdouble;
      Max_Margin_From : Gdouble; Min_Margin_To : Gdouble;
      Max_Margin_To   : Gdouble; Margin1, Margin2 : out Margins);
   --  When computing the layout for orthogonal links, we add some borders
   --  around the items, through which the link might go. These borders are
   --  larger between the two items.

   procedure Margins_Of_Items
     (Item1, Item2    : Model_Rectangle; Min_Margin_From : Gdouble;
      Max_Margin_From : Gdouble; Min_Margin_To : Gdouble;
      Max_Margin_To   : Gdouble; Margin1, Margin2 : out Margins)
   is
      pragma Unreferenced (Max_Margin_To);
      Dist, Tmp : Gdouble;
   begin
      if Item1.X + Item1.Width < Item2.X then
         Dist          := Item2.X - Item1.X - Item1.Width;
         Margin1.Left  := Min_Margin_From;
         Tmp           := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Right := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Left  :=
           (if Item2.Width > 0.0 then Margin1.Right else Min_Margin_To);
         Margin2.Right := Min_Margin_To;

      elsif Item2.X + Item2.Width < Item1.X then
         Dist          := Item1.X - Item2.X - Item2.Width;
         Margin1.Right := Min_Margin_From;
         Tmp           := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Left  := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Right :=
           (if Item2.Width > 0.0 then Margin1.Left else Min_Margin_To);
         Margin2.Left := Min_Margin_To;

      else
         Margin1.Left  := Min_Margin_From;
         Margin1.Right := Min_Margin_From;
         Margin2.Left  := Min_Margin_To;
         Margin2.Right := Min_Margin_To;
      end if;

      if Item1.Y + Item1.Height < Item2.Y then
         Dist           := Item2.Y - Item1.Y - Item1.Height;
         Margin1.Top    := Min_Margin_From;
         Tmp            := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Bottom := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Top    :=
           (if Item2.Height > 0.0 then Margin1.Bottom else Min_Margin_To);
         Margin2.Bottom := Min_Margin_To;

      elsif Item2.Y + Item2.Height < Item1.Y then
         Dist           := Item1.Y - Item2.Y - Item2.Height;
         Margin1.Bottom := Min_Margin_From;
         Tmp            := Gdouble'Max (Dist / 2.0, Min_Margin_From);
         Margin1.Top    := Gdouble'Min (Max_Margin_From, Tmp);
         Margin2.Bottom :=
           (if Item2.Height > 0.0 then Margin1.Top else Min_Margin_To);
         Margin2.Top := Min_Margin_To;

      else
         Margin1.Top    := Min_Margin_From;
         Margin1.Bottom := Min_Margin_From;
         Margin2.Top    := Min_Margin_To;
         Margin2.Bottom := Min_Margin_To;
      end if;
   end Margins_Of_Items;

   ---------------------------------------
   -- Compute_Layout_For_One_Curve_Link --
   ---------------------------------------

   procedure Compute_Layout_For_One_Curve_Link
     (Link            :     not null access Canvas_Link_Record'Class;
      Min_Margin_From :     Gdouble; Max_Margin_From : Gdouble;
      Min_Margin_To   :     Gdouble; Max_Margin_To : Gdouble;
      Prev_End_Side   :     Side_Anchor_Style;
      Next_End_Side   : out Side_Anchor_Style);
   procedure Compute_Layout_For_One_Curve_Link
     (Link            : not null access Canvas_Link_Record'Class;
      Min_Margin_From : Gdouble; Max_Margin_From : Gdouble;
      Min_Margin_To   : Gdouble; Max_Margin_To : Gdouble;
      Prev_End_Side : Side_Anchor_Style; Next_End_Side : out Side_Anchor_Style)
   is

      Dim                    : constant Anchors := Compute_Anchors (Link);
      Margin_From, Margin_To : Margins;
      From, To               : Item_Point;
      P_From                 : Item_Point;   --  extending from the From box
      P_To                   : Item_Point;   --  extending from the To box
      M                      : Item_Point;
      P1, P2                 : Item_Point;
      Q1, Q2                 : Item_Point;
      R1, R2                 : Item_Point;
      S1, S2                 : Item_Point;
      T1, T2                 : Item_Point;
      M_Bis                  : Item_Point;
      Q1_Bis, Q2_Bis         : Item_Point;
      R1_Bis, R2_Bis         : Item_Point;
      S1_Bis, S2_Bis         : Item_Point;
      T1_Bis, T2_Bis         : Item_Point;
      M_Tris                 : Item_Point;
      Q1_Tris, Q2_Tris       : Item_Point;
      R1_Tris, R2_Tris       : Item_Point;
      S1_Tris, S2_Tris       : Item_Point;
      T1_Tris, T2_Tris       : Item_Point;
      Side_From, Side_To     : Side_Anchor_Style;

      type Rect is record
         X0, Y0, X1, Y1 : Gdouble;
      end record;
      TL_To : constant Container_Item :=
        Container_Item (Link.To.Get_Toplevel_Item);
      TL_From : constant Container_Item :=
        Container_Item (Link.From.Get_Toplevel_Item);

      BB_To : constant Rect :=
        (X0 => TL_To.Position.X - Link.Offset,
         Y0 => TL_To.Position.Y - Link.Offset,
         X1 => TL_To.Position.X + TL_To.Get_Width + Link.Offset,
         Y1 => TL_To.Position.Y + TL_To.Get_Height + Link.Offset);
      BB_From : constant Rect :=
        (X0 => TL_From.Position.X - Link.Offset,
         Y0 => TL_From.Position.Y - Link.Offset,
         X1 => TL_From.Position.X + TL_From.Get_Width + Link.Offset,
         Y1 => TL_From.Position.Y + TL_From.Get_Height + Link.Offset);

      -------------------------------------------------------------
      package One_Turn is
         procedure Compute_Remaining_Points_1_Turn;
         procedure Curve_1_Turn_Start_Vertical_End_Horizontal; --  OK
         procedure Curve_1_Turn_Start_Horizontal_End_Vertical; --  OK
      end One_Turn;
      use One_Turn;

      package Two_Turns is
         procedure Compute_Remaining_Points_2_Turns
           (With_Link_Points : Boolean);
         procedure Curve_2_Turns_Start_End_Vertical_With_Inflection;   --  OK
         procedure Curve_2_Turns_Start_End_Horizontal_With_Inflection; --  OK
         procedure Curve_2_Turns_Start_Bottom_End_Bottom;              --  OK
         procedure Curve_2_Turns_Start_Top_End_Top;                    --  OK
         procedure Curve_2_Turns_Start_Left_End_Left;                  --
         procedure Curve_2_Turns_Start_Right_End_Right;                --
      end Two_Turns;
      use Two_Turns;

      package Three_Turns is
         procedure Compute_Remaining_Points_3_Turns
           (With_Link_Points : Boolean);
         procedure Curve_Start_Top_3_Turns_Clockwise;
         procedure Curve_Start_Top_3_Turns_AntiClockwise;
         procedure Curve_Start_Bottom_3_Turns_Clockwise;
         procedure Curve_Start_Bottom_3_Turns_AntiClockwise;
         procedure Curve_Start_Right_3_Turns_Clockwise;
         procedure Curve_Start_Right_3_Turns_AntiClockwise;
         procedure Curve_Start_Left_3_Turns_Clockwise;
         procedure Curve_Start_Left_3_Turns_AntiClockwise;
      end Three_Turns;
      use Three_Turns;

      package Four_Turns is
         procedure Compute_Remaining_Points_4_Turns
           (With_Link_Points : Boolean);
         procedure Curve_Start_Top_4_Turns_Clockwise;
         procedure Curve_Start_Top_4_Turns_AntiClockwise;     --  OK
         procedure Curve_Start_Bottom_4_Turns_Clockwise;       --  OK
         procedure Curve_Start_Bottom_4_Turns_AntiClockwise;   --  OK
         procedure Curve_Start_Right_4_Turns_AntiClockwise;  --  OK
         procedure Curve_Start_Right_4_Turns_Clockwise;      --  OK
         procedure Curve_Start_Left_4_Turns_Clockwise;       --  OK
         procedure Curve_Start_Left_4_Turns_AntiClockwise;   --  OK
      end Four_Turns;
      use Four_Turns;

      --------------------------------------------
      ------------ Curves with 1 turn -----------
      --------------------------------------------
      package body One_Turn is
         procedure Compute_Remaining_Points_1_Turn is
         begin
            P2 := P1;
            Link.Points :=
              new Item_Point_Array'(From
                                    & P1 & P2 & To);
         end Compute_Remaining_Points_1_Turn;

         procedure Curve_1_Turn_Start_Vertical_End_Horizontal
         is --  T/R-L, B/R-L
         begin
            P1 := (P_From.X, P_To.Y);
            Compute_Remaining_Points_1_Turn;
         end Curve_1_Turn_Start_Vertical_End_Horizontal;

         procedure Curve_1_Turn_Start_Horizontal_End_Vertical
         is  -- R/T-B, L/T-B
         begin
            P1 := (P_To.X, P_From.Y);
            Compute_Remaining_Points_1_Turn;
         end Curve_1_Turn_Start_Horizontal_End_Vertical;
      end One_Turn;

      --------------------------------------------
      ------------ Curves with 2 turns -----------
      --------------------------------------------
      package body Two_Turns is
         procedure Compute_Remaining_Points_2_Turns
           (With_Link_Points : Boolean)
         is
         begin
            M  := (P1 + T1) / 2.0;
            Q1 := (M + P1) / 2.0;
            Q2 := (M + T1) / 2.0;

            P2 := P1;
            T2 := T1;
            R1 := (M + Q1) / 2.0;
            R2 := R1;
            S1 := (Q2 + M) / 2.0;
            S2 := S1;

            if With_Link_Points then
               Link.Points :=
                 new Item_Point_Array'
                   (From
                    & P1 & P2 & Q1
                    & R1 & R2 & M
                    & S1 & S2 & Q2
                    & T1 & T2 & To);
            end if;
         end Compute_Remaining_Points_2_Turns;

         procedure Curve_2_Turns_Start_End_Vertical_With_Inflection is
         begin
            P1 := (P_From.X, (P_To.Y + P_From.Y) / 2.0);
            T1 := (P_To.X, P1.Y);

            Compute_Remaining_Points_2_Turns (With_Link_Points => True);
         end Curve_2_Turns_Start_End_Vertical_With_Inflection;

         procedure Curve_2_Turns_Start_End_Horizontal_With_Inflection is
         begin
            P1 := ((P_From.X + P_To.X) / 2.0, P_From.Y);
            T1 := (P1.X, P_To.Y);

            Compute_Remaining_Points_2_Turns (With_Link_Points => True);
         end Curve_2_Turns_Start_End_Horizontal_With_Inflection;

         procedure Curve_2_Turns_Start_Top_End_Top is       --  OK
         begin
            if To.X in BB_From.X0 .. BB_From.X1 and then To.Y > BB_From.Y1 then
               Curve_Start_Top_4_Turns_AntiClockwise;
               return;
            end if;

            if From.X in BB_To.X0 .. BB_To.X1 and then From.Y > BB_To.Y1 then
               Curve_Start_Top_4_Turns_Clockwise;
               return;
            end if;

            if P_From.Y < P_To.Y then
               P1 := (P_From.X, P_From.Y - Link.Offset);
            else
               P1 := (P_From.X, P_To.Y - Link.Offset);
            end if;
            T1 := (P_To.X, P1.Y);

            Compute_Remaining_Points_2_Turns (With_Link_Points => True);
         end Curve_2_Turns_Start_Top_End_Top;

         procedure Curve_2_Turns_Start_Bottom_End_Bottom is --  OK
         begin
            if From.X in BB_To.X0 .. BB_To.X1 and then To.Y > BB_From.Y1 then
               Curve_Start_Bottom_4_Turns_Clockwise;
               return;
            end if;

            if To.X in BB_From.X0 .. BB_From.X1 and then BB_To.Y1 < BB_From.Y0
            then
               Curve_Start_Bottom_4_Turns_AntiClockwise;
               return;
            end if;

            if P_From.Y < P_To.Y then
               P1 := (P_From.X, P_To.Y + Link.Offset);
            else
               P1 := (P_From.X, P_From.Y + Link.Offset);
            end if;
            T1 := (P_To.X, P1.Y);

            Compute_Remaining_Points_2_Turns (With_Link_Points => True);
         end Curve_2_Turns_Start_Bottom_End_Bottom;

         procedure Curve_2_Turns_Start_Left_End_Left is     --  OK
         begin
            if To.Y in BB_From.Y0 .. BB_From.Y1 and then To.X > BB_From.X1 then
               Curve_Start_Left_4_Turns_AntiClockwise;
               return;
            end if;

            if From.Y in BB_To.Y0 .. BB_To.Y1 and then To.X < BB_From.X0 then
               Curve_Start_Left_4_Turns_Clockwise;
               return;
            end if;

            if P_From.X < P_To.X then
               P1 := (P_From.X - Link.Offset, P_From.Y);
            else
               P1 := (P_To.X - Link.Offset, P_From.Y);
            end if;
            T1 := (P1.X, P_To.Y);

            Compute_Remaining_Points_2_Turns (With_Link_Points => True);
         end Curve_2_Turns_Start_Left_End_Left;

         procedure Curve_2_Turns_Start_Right_End_Right is   --  OK
         begin
            if To.Y in BB_From.Y0 .. BB_From.Y1 and then To.X < BB_From.X0 then
               Curve_Start_Right_4_Turns_AntiClockwise;
               return;
            end if;

            if From.Y in BB_To.Y0 .. BB_To.Y1 and then From.X < BB_To.X0 then
               Curve_Start_Right_4_Turns_Clockwise;
               return;
            end if;

            if P_From.X < P_To.X then
               P1 := (P_To.X + Link.Offset, P_From.Y);
            else
               P1 := (P_From.X + Link.Offset, P_From.Y);
            end if;
            T1 := (P1.X, P_To.Y);

            Compute_Remaining_Points_2_Turns (With_Link_Points => True);
         end Curve_2_Turns_Start_Right_End_Right;
      end Two_Turns;

      --------------------------------------------
      ------------ Curves with 3 turns -----------
      --------------------------------------------
      package body Three_Turns is
         procedure Compute_Remaining_Points_3_Turns
           (With_Link_Points : Boolean)
         is
         begin
            M_Bis  := (T1 + T1_Bis) / 2.0;
            Q1_Bis := (M_Bis + T1) / 2.0;
            Q2_Bis := (M_Bis + T1_Bis) / 2.0;

            T2_Bis := T1_Bis;
            R1_Bis := (M_Bis + Q1_Bis) / 2.0;
            R2_Bis := R1_Bis;
            S1_Bis := (M_Bis + Q2_Bis) / 2.0;
            S2_Bis := S1_Bis;

            if With_Link_Points then
               Link.Points :=
                 new Item_Point_Array'
                   (From
                    & P1 & P2 & Q1
                    & R1 & R2 & M
                    & S1 & S2 & Q2
                    & T1 & T2 & Q1_Bis
                    & R1_Bis & R2_Bis & M_Bis
                    & S1_Bis & S2_Bis & Q2_Bis
                    & T1_Bis & T2_Bis & To);
            end if;
         end Compute_Remaining_Points_3_Turns;

         procedure Curve_Start_Top_3_Turns_Clockwise
         is --  top/right          OK
         begin
            if BB_To.X1 > BB_From.X1 and then BB_To.Y0 > BB_From.Y0 then
               --  sud east
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_To.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif BB_To.X1 < BB_From.X1 and then To.Y > BB_From.Y1 then
               --  sud west
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_From.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif To.Y < BB_From.Y1 and then To.X < BB_From.X0 then
               --  north west
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_To.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif To.Y < BB_From.Y0 and then To.X < BB_From.X0 then
               --  north west before curve 1 turn
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_From.X0, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif To.Y < BB_From.Y0 and then From.X in BB_To.X0 .. BB_To.X1 then
               --  overlapping north east to north west
               P1     := (P_From.X, BB_To.Y1);
               T1     := (BB_To.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            else
               P1     := (P_From.X, BB_To.Y0);
               T1     := (BB_To.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Top_3_Turns_Clockwise;

         procedure Curve_Start_Top_3_Turns_AntiClockwise
         is --  top/left       OK
         begin
            if To.Y in BB_From.Y0 .. BB_From.Y1 and then To.X > BB_From.X1 then
               --  sud east overlap
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_From.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif BB_To.X0 > BB_From.X0 and then To.Y > BB_From.Y0 then
               --  sud east
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_From.X0, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif From.X in BB_To.X0 .. BB_To.X1 and then BB_To.Y0 < BB_From.Y0
            then
               --  north west overlap
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_To.X0, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif To.X < BB_To.X1 and then BB_To.Y0 < BB_From.Y0 then
               --  north west
               P1     := (P_From.X, BB_To.Y0);
               T1     := (BB_To.X0, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            else
               --  sud west
               P1     := (P_From.X, BB_From.Y0);
               T1     := (BB_To.X0, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Top_3_Turns_AntiClockwise;

         procedure Curve_Start_Bottom_3_Turns_Clockwise
         is --  bottom/left     OK
         begin
            if To.Y < From.Y then --  north
               if BB_To.X0 < BB_From.X0 then --  West
                  P1     := (P_From.X, BB_From.Y1);
                  T1     := (BB_To.X0, P1.Y);
                  T1_Bis := (T1.X, P_To.Y);
               else -- east
                  if To.Y in BB_From.Y0 .. BB_From.Y1 then --  overlap
                     P1     := (P_From.X, BB_From.Y1);
                     T1     := (BB_To.X0, P1.Y);
                     T1_Bis := (T1.X, P_To.Y);
                  else
                     P1     := (P_From.X, BB_From.Y1);
                     T1     := (BB_From.X0, P1.Y);
                     T1_Bis := (T1.X, P_To.Y);
                  end if;
               end if;
            else --  sud
               if BB_To.X1 < From.X then --  West
                  P1     := (P_From.X, BB_To.Y1);
                  T1     := (BB_To.X0, P1.Y);
                  T1_Bis := (T1.X, P_To.Y);
               else --  east
                  P1     := (P_From.X, BB_From.Y1);
                  T1     := (BB_To.X0, P1.Y);
                  T1_Bis := (T1.X, P_To.Y);
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Bottom_3_Turns_Clockwise;

         procedure Curve_Start_Bottom_3_Turns_AntiClockwise
         is -- bottom/right OK
         begin
            if To.Y < BB_From.Y0 then
               --  north
               if BB_To.X1 > BB_From.X1 then
                  --  north east
                  P1     := (From.X, BB_From.Y1);
                  T1     := (BB_To.X1, P1.Y);
                  T1_Bis := (T1.X, P_To.Y);
               else
                  --  north west
                  P1     := (From.X, BB_From.Y1);
                  T1     := (BB_From.X1, P1.Y);
                  T1_Bis := (T1.X, P_To.Y);
               end if;
            elsif From.X in BB_To.X0 .. BB_To.X1 and then From.X < BB_To.X1
            then
               --  sud east overlap
               P1     := (From.X, BB_From.Y1);
               T1     := (BB_To.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            elsif BB_To.Y1 > BB_From.Y1 and then From.X < BB_To.X1 then
               --  sud east
               P1     := (From.X, BB_To.Y1);
               T1     := (BB_To.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            else
               P1     := (From.X, BB_From.Y1);
               T1     := (BB_To.X1, P1.Y);
               T1_Bis := (T1.X, P_To.Y);
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Bottom_3_Turns_AntiClockwise;

         procedure Curve_Start_Right_3_Turns_Clockwise
         is  --  right/bottom    OK
         begin
            if To.X < From.X then --  West
               if To.Y < BB_From.Y1 then --  north west
                  if To.X in BB_From.X0 .. BB_From.X1 then
                     P1     := (BB_From.X1, P_From.Y);
                     T1     := (P1.X, BB_To.Y1);
                     T1_Bis := (P_To.X, T1.Y);
                  else
                     P1     := (BB_From.X1, P_From.Y);
                     T1     := (P1.X, BB_From.Y1);
                     T1_Bis := (P_To.X, T1.Y);
                  end if;
               else --  sud west
                  P1     := (BB_From.X1, P_From.Y);
                  T1     := (P1.X, BB_To.Y1);
                  T1_Bis := (P_To.X, T1.Y);
               end if;
            else --  sud east
               if From.Y in BB_To.Y0 .. BB_To.Y1 then --  overlap
                  P1     := (BB_From.X1, P_From.Y);
                  T1     := (P1.X, BB_To.Y1);
                  T1_Bis := (P_To.X, T1.Y);
               else
                  P1     := (BB_To.X1, P_From.Y);
                  T1     := (P1.X, BB_To.Y1);
                  T1_Bis := (P_To.X, T1.Y);
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Right_3_Turns_Clockwise;

         procedure Curve_Start_Right_3_Turns_AntiClockwise
         is --  right/top    OK
         begin
            if BB_To.Y0 < BB_From.Y0 then -- north
               if BB_To.X1 < BB_From.X1 then --  west
                  P1     := (BB_From.X1, P_From.Y);
                  T1     := (P1.X, BB_To.Y0);
                  T1_Bis := (P_To.X, BB_To.Y0);
               else --  east
                  if BB_To.Y1 > From.Y then
                     P1     := (BB_From.X1, P_From.Y);
                     T1     := (P1.X, BB_To.Y0);
                     T1_Bis := (P_To.X, T1.Y);
                  else
                     P1     := (BB_To.X1, P_From.Y);
                     T1     := (P1.X, BB_To.Y0);
                     T1_Bis := (P_To.X, BB_To.Y0);
                  end if;
               end if;
            else --  sud west
               if To.X in BB_From.X0 .. BB_From.X1 then
                  P1     := (BB_From.X1, P_From.Y);
                  T1     := (P1.X, BB_To.Y0);
                  T1_Bis := (P_To.X, T1.Y);
               elsif BB_From.X0 < BB_To.X0 then
                  P1     := (BB_From.X1, P_From.Y);
                  T1     := (P1.X, BB_To.Y0);
                  T1_Bis := (P_To.X, T1.Y);
               else
                  P1     := (BB_From.X1, P_From.Y);
                  T1     := (P1.X, BB_From.Y0);
                  T1_Bis := (P_To.X, T1.Y);
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Right_3_Turns_AntiClockwise;

         procedure Curve_Start_Left_3_Turns_Clockwise
         is --  left/Top          OK
         begin
            if BB_To.Y0 < BB_From.Y0 then -- north
               if BB_To.X0 < BB_From.X0 then --  west
                  if BB_To.Y1 > From.Y then --  overlap
                     P1     := (BB_To.X1, P_From.Y);
                     T1     := (P1.X, BB_To.Y0);
                     T1_Bis := (P_To.X, T1.Y);
                  else
                     P1     := (BB_To.X0, P_From.Y);
                     T1     := (P1.X, BB_To.Y0);
                     T1_Bis := (P_To.X, T1.Y);
                  end if;
               else --  east
                  P1     := (BB_From.X0, P_From.Y);
                  T1     := (P1.X, BB_To.Y0);
                  T1_Bis := (P_To.X, T1.Y);
               end if;
            else --  Sud
               if To.X in BB_From.X0 .. BB_From.X1 then --  overlap
                  P1     := (BB_From.X0, P_From.Y);
                  T1     := (P1.X, BB_To.Y0);
                  T1_Bis := (P_To.X, T1.Y);
               else
                  if BB_To.X1 < BB_From.X0 then -- west
                     P1     := (BB_To.X1, P_From.Y);
                     T1     := (P1.X, BB_To.Y0);
                     T1_Bis := (P_To.X, T1.Y);
                  else --  east
                     P1     := (BB_From.X0, P_From.Y);
                     T1     := (P1.X, BB_From.Y0);
                     T1_Bis := (P_To.X, T1.Y);
                  end if;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Left_3_Turns_Clockwise;

         procedure Curve_Start_Left_3_Turns_AntiClockwise
         is --  left/Bottom   OK
         begin
            if To.Y < BB_From.Y1 then -- north
               if To.X < BB_From.X0 then
                  P1     := (BB_To.X1, P_From.Y);
                  T1     := (P1.X, BB_To.Y1);
                  T1_Bis := (P_To.X, T1.Y);
               elsif To.X in BB_From.X0 .. BB_From.X1 then
                  P1     := (BB_From.X0, P_From.Y);
                  T1     := (P1.X, BB_To.Y1);
                  T1_Bis := (P_To.X, T1.Y);
               else
                  P1     := (BB_From.X0, P_From.Y);
                  T1     := (P1.X, BB_From.Y1);
                  T1_Bis := (P_To.X, T1.Y);
               end if;
            else --  sud
               if BB_To.X0 < BB_From.X0 then --  west
                  if BB_To.Y0 < From.Y then
                     P1     := (BB_To.X1, P_From.Y);
                     T1     := (P1.X, BB_To.Y1);
                     T1_Bis := (P_To.X, T1.Y);
                  else
                     P1     := (BB_To.X0, P_From.Y);
                     T1     := (P1.X, BB_To.Y1);
                     T1_Bis := (P_To.X, T1.Y);
                  end if;
               else --  east
                  P1     := (BB_From.X0, P_From.Y);
                  T1     := (P1.X, BB_To.Y1);
                  T1_Bis := (P_To.X, T1.Y);
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => True);
         end Curve_Start_Left_3_Turns_AntiClockwise;
      end Three_Turns;

      --------------------------------------------
      ------------ Curves with 4 turns -----------
      --------------------------------------------
      package body Four_Turns is
         procedure Compute_Remaining_Points_4_Turns
           (With_Link_Points : Boolean)
         is
         begin
            M_Tris  := (T1_Tris + T1_Bis) / 2.0;
            Q1_Tris := (M_Tris + T1_Bis) / 2.0;
            Q2_Tris := (M_Tris + T1_Tris) / 2.0;

            T2_Tris := T1_Tris;
            R1_Tris := (M_Tris + Q1_Tris) / 2.0;
            R2_Tris := R1_Tris;
            S1_Tris := (M_Tris + Q2_Tris) / 2.0;
            S2_Tris := S1_Tris;

            if With_Link_Points then
               Link.Points :=
                 new Item_Point_Array'
                   (From
                    & P1 & P2 & Q1
                    & R1 & R2 & M
                    & S1 & S2 & Q2
                    & T1 & T2 & Q1_Bis
                    & R1_Bis & R2_Bis & M_Bis
                    & S1_Bis & S2_Bis & Q2_Bis
                    & T1_Bis & T2_Bis & Q1_Tris
                    & R1_Tris & R2_Tris & M_Tris
                    & S1_Tris & S2_Tris & Q2_Tris
                    & T1_Tris & T2_Tris & To);
            end if;
         end Compute_Remaining_Points_4_Turns;

         procedure Curve_Start_Top_4_Turns_AntiClockwise is     --  OK
         begin --  Top/Bottom West
            if To.X in BB_From.X0 .. BB_From.X1 and then To.Y > BB_From.Y1
              and then Side_To = Top
            then
               P1      := (P_From.X, BB_From.Y0);
               T1      := (BB_From.X1, P1.Y);
               T1_Bis  := (T1.X, BB_From.Y1);
               T1_Tris := (P_To.X, T1_Bis.Y);
            else
               P1      := (P_From.X, BB_From.Y0);
               T1      := (BB_To.X0, P1.Y);
               T1_Bis  := (T1.X, BB_To.Y1);
               T1_Tris := (P_To.X, T1_Bis.Y);

               if BB_From.Y0 in BB_To.Y0 .. BB_To.Y1 then
                  P1.Y := BB_To.Y0;
                  T1.Y := P1.Y;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Top_4_Turns_AntiClockwise;

         procedure Curve_Start_Top_4_Turns_Clockwise is         --  OK
         begin --  Top/Bottom East
            if From.X in BB_To.X0 .. BB_To.X1 and then From.Y > BB_To.Y1
              and then Side_To = Top
            then
               P1      := (P_From.X, BB_From.Y0);
               T1      := (BB_To.X1, P1.Y);
               T1_Bis  := (T1.X, BB_To.Y0);
               T1_Tris := (P_To.X, T1_Bis.Y);
            else
               P1      := (P_From.X, BB_From.Y0);
               T1      := (BB_To.X1, P1.Y);
               T1_Bis  := (T1.X, BB_To.Y1);
               T1_Tris := (P_To.X, T1_Bis.Y);

               if BB_From.Y0 in BB_To.Y0 .. BB_To.Y1 then
                  P1.Y := BB_To.Y0;
                  T1.Y := P1.Y;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Top_4_Turns_Clockwise;

         procedure Curve_Start_Bottom_4_Turns_Clockwise is      --  OK
         begin --  Bottom/top west
            if From.X in BB_To.X0 .. BB_To.X1 and then To.Y > BB_From.Y1
              and then Side_To = Bottom
            then
               P1      := (P_From.X, BB_From.Y1 - Link.Offset / 2.0);
               T1      := (BB_To.X1, P1.Y);
               T1_Bis  := (T1.X, BB_To.Y1);
               T1_Tris := (P_To.X, T1_Bis.Y);
            else
               P1      := (P_From.X, BB_From.Y1);
               T1      := (BB_To.X0, P1.Y);
               T1_Bis  := (T1.X, BB_To.Y0);
               T1_Tris := (P_To.X, T1_Bis.Y);

               if P1.Y in BB_To.Y0 .. BB_To.Y1 then
                  P1.Y := BB_To.Y1;
                  T1.Y := P1.Y;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Bottom_4_Turns_Clockwise;

         procedure Curve_Start_Bottom_4_Turns_AntiClockwise is  --  OK
         begin --  bottom/top east
            if To.X in BB_From.X0 .. BB_From.X1 and then BB_To.Y1 < BB_From.Y0
              and then Side_To = Bottom
            then
               P1      := (P_From.X, BB_From.Y1);
               T1      := (BB_From.X1, P1.Y);
               T1_Bis  := (T1.X, BB_From.Y0);
               T1_Tris := (P_To.X, T1_Bis.Y);
            else
               P1      := (P_From.X, BB_From.Y1);
               T1      := (BB_To.X1, P1.Y);
               T1_Bis  := (T1.X, BB_To.Y0);
               T1_Tris := (P_To.X, T1_Bis.Y);

               if P1.Y in BB_To.Y0 .. BB_To.Y1 then
                  P1.Y := BB_To.Y1;
                  T1.Y := P1.Y;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Bottom_4_Turns_AntiClockwise;

         procedure Curve_Start_Right_4_Turns_AntiClockwise is   --  OK
         begin
            if To.Y in BB_From.Y0 .. BB_From.Y1 and then To.X < BB_From.X0
              and then Side_To = Right
            then
               P1      := (BB_From.X1, P_From.Y);
               T1      := (P1.X, BB_From.Y0);
               T1_Bis  := (BB_From.X0 + Link.Offset / 2.0, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);
            else
               P1      := (P_From.X + Link.Offset, P_From.Y);
               T1      := (P1.X, BB_To.Y0);
               T1_Bis  := (BB_To.X0, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);

               if P1.X in BB_To.X0 .. BB_To.X1 then
                  P1.X := BB_To.X1;
                  T1.X := P1.X;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Right_4_Turns_AntiClockwise;

         procedure Curve_Start_Right_4_Turns_Clockwise is       --  OK
         begin
            if From.Y in BB_To.Y0 .. BB_To.Y1 and then From.X < BB_To.X0
              and then Side_To = Right
            then
               P1      := (From.X + Link.Offset / 2.0, From.Y);
               T1      := (P1.X, BB_To.Y1);
               T1_Bis  := (BB_To.X1, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);
            else
               P1      := (P_From.X + Link.Offset, P_From.Y);
               T1      := (P1.X, BB_To.Y1);
               T1_Bis  := (BB_To.X0, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);

               if P1.X in BB_To.X0 .. BB_To.X1 then
                  P1.X := BB_To.X1;
                  T1.X := P1.X;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Right_4_Turns_Clockwise;

         procedure Curve_Start_Left_4_Turns_Clockwise is        --  OK
         begin
            if From.Y in BB_To.Y0 .. BB_To.Y1 and then To.X < BB_From.X0
              and then Side_To = Left
            then
               P1      := (BB_From.X0, P_From.Y);
               T1      := (P1.X, BB_To.Y1);
               T1_Bis  := (BB_To.X0, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);
            else

               P1      := (BB_From.X0, P_From.Y);
               T1      := (P1.X, BB_To.Y0);
               T1_Bis  := (BB_To.X1, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);

               if P1.X > BB_To.X0 then
                  P1.X := BB_To.X0;
                  T1.X := P1.X;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Left_4_Turns_Clockwise;

         procedure Curve_Start_Left_4_Turns_AntiClockwise is    --  OK
         begin
            if To.Y in BB_From.Y0 .. BB_From.Y1 and then To.X > BB_From.X1
              and then Side_To = Left
            then
               P1      := (P_From.X - Link.Offset, P_From.Y);
               T1      := (P1.X, BB_From.Y0);
               T1_Bis  := (BB_From.X1, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);
            else
               P1      := (P_From.X - Link.Offset, P_From.Y);
               T1      := (P1.X, BB_To.Y1);
               T1_Bis  := (BB_To.X1, T1.Y);
               T1_Tris := (T1_Bis.X, P_To.Y);

               if P1.X in BB_To.X0 .. BB_To.X1 then
                  P1.X := BB_To.X0;
                  T1.X := P1.X;
               end if;
            end if;

            Compute_Remaining_Points_2_Turns (With_Link_Points => False);
            Compute_Remaining_Points_3_Turns (With_Link_Points => False);
            Compute_Remaining_Points_4_Turns (With_Link_Points => True);
         end Curve_Start_Left_4_Turns_AntiClockwise;
      end Four_Turns;

      --------------------------------------------
   begin --  Compute_Layout_For_One_Curve_Link
      if Link.Waypoints /= null then
         raise Program_Error
           with "Incorrect programming: Between two waypoints cannot exist more waypoints";
      end if;

      Margins_Of_Items
        (Dim.From.Toplevel, Dim.To.Toplevel, Min_Margin_From, Max_Margin_From,
         Min_Margin_To, Max_Margin_To, Margin_From, Margin_To);
      Compute_Side
        (Link, Link.Anchor_From, Dim.From, Margin_From, From, P_From);
      Compute_Side (Link, Link.Anchor_To, Dim.To, Margin_To, To, P_To);

      --  add some points to force orthogonal exits
      Compute_Side_Attachment4 (Link, Prev_End_Side, Side_From, Side_To);

      --  obtain the points
      case Side_From is
         when Top =>
            case Side_To is
               when Top =>
                  Curve_2_Turns_Start_Top_End_Top;
                  Next_End_Side := Bottom;
               when Bottom =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        if To.Y + Link.Offset < From.Y then
                           Curve_2_Turns_Start_End_Vertical_With_Inflection;
                        else
                           Curve_Start_Top_4_Turns_AntiClockwise;
                        end if;
                     when North_East =>
                        if To.Y + Link.Offset < From.Y then
                           Curve_2_Turns_Start_End_Vertical_With_Inflection;
                        else
                           Curve_Start_Top_4_Turns_Clockwise;
                        end if;
                     when Sud_East =>
                        Curve_Start_Top_4_Turns_Clockwise;
                     when Sud_West =>
                        Curve_Start_Top_4_Turns_AntiClockwise;
                  end case;
                  Next_End_Side := Top;
               when Right =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        if To.X + Link.Offset < From.X
                          and then To.Y + Link.Offset < From.Y
                        then
                           Curve_1_Turn_Start_Vertical_End_Horizontal;
                        else
                           Curve_Start_Top_3_Turns_Clockwise;
                        end if;
                     when North_East =>
                        Curve_Start_Top_3_Turns_Clockwise;
                     when Sud_East =>
                        Curve_Start_Top_3_Turns_Clockwise;
                     when Sud_West =>
                        Curve_Start_Top_3_Turns_Clockwise;
                  end case;
                  Next_End_Side := Left;
               when Left =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Top_3_Turns_AntiClockwise;
                     when North_East =>
                        if To.X - Link.Offset > From.X
                          and then To.Y + Link.Offset < From.Y
                        then
                           Curve_1_Turn_Start_Vertical_End_Horizontal;
                        else
                           Curve_Start_Top_3_Turns_AntiClockwise;
                        end if;
                     when Sud_East =>
                        Curve_Start_Top_3_Turns_AntiClockwise;
                     when Sud_West =>
                        Curve_Start_Top_3_Turns_AntiClockwise;
                  end case;
                  Next_End_Side := Right;
               when Not_Defined =>
                  Curve_2_Turns_Start_Top_End_Top;
                  Next_End_Side := Bottom;
            end case;
         when Bottom =>
            case Side_To is
               when Top =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Bottom_4_Turns_Clockwise;
                     when North_East =>
                        Curve_Start_Bottom_4_Turns_AntiClockwise;
                     when Sud_East =>
                        if From.Y + Link.Offset < To.Y then
                           Curve_2_Turns_Start_End_Vertical_With_Inflection;
                        else
                           Curve_Start_Bottom_4_Turns_AntiClockwise;
                        end if;
                     when Sud_West =>
                        if From.Y + Link.Offset < To.Y then
                           Curve_2_Turns_Start_End_Vertical_With_Inflection;
                        else
                           Curve_Start_Bottom_4_Turns_Clockwise;
                        end if;
                  end case;
                  Next_End_Side := Bottom;
               when Bottom =>
                  Curve_2_Turns_Start_Bottom_End_Bottom;
                  Next_End_Side := Top;
               when Right =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Bottom_3_Turns_AntiClockwise;
                     when North_East =>
                        Curve_Start_Bottom_3_Turns_AntiClockwise;
                     when Sud_East =>
                        Curve_Start_Bottom_3_Turns_AntiClockwise;
                     when Sud_West =>
                        if To.Y > From.Y + Link.Offset
                          and then To.X + Link.Offset < From.X
                        then
                           Curve_1_Turn_Start_Vertical_End_Horizontal;
                        else
                           Curve_Start_Bottom_3_Turns_AntiClockwise;
                        end if;
                  end case;
                  Next_End_Side := Left;
               when Left =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Bottom_3_Turns_Clockwise;
                     when North_East =>
                        Curve_Start_Bottom_3_Turns_Clockwise;
                     when Sud_East =>
                        if From.X + Link.Offset < To.X
                          and then From.Y + Link.Offset < To.Y
                        then
                           Curve_1_Turn_Start_Vertical_End_Horizontal;
                        else
                           Curve_Start_Bottom_3_Turns_Clockwise;
                        end if;
                     when Sud_West =>
                        Curve_Start_Bottom_3_Turns_Clockwise;
                  end case;
                  Next_End_Side := Right;
               when Not_Defined =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Bottom_4_Turns_AntiClockwise;
                     when North_East =>
                        Curve_Start_Bottom_4_Turns_Clockwise;
                     when Sud_East =>
                        Curve_2_Turns_Start_End_Vertical_With_Inflection;
                     when Sud_West =>
                        Curve_2_Turns_Start_End_Vertical_With_Inflection;
                  end case;
                  Next_End_Side := Bottom;
            end case;
         when Right =>
            case Side_To is
               when Top =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Right_3_Turns_AntiClockwise;
                     when North_East =>
                        Curve_Start_Right_3_Turns_AntiClockwise;
                     when Sud_East =>
                        if From.Y + Link.Offset < To.Y
                          and then From.X + Link.Offset < To.X
                        then
                           Curve_1_Turn_Start_Horizontal_End_Vertical;
                        else
                           Curve_Start_Right_3_Turns_AntiClockwise;
                        end if;
                     when Sud_West =>
                        Curve_Start_Right_3_Turns_AntiClockwise;
                  end case;
                  Next_End_Side := Bottom;
               when Bottom =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Right_3_Turns_Clockwise;
                     when North_East =>
                        if From.X + Link.Offset < To.X
                          and then To.Y + Link.Offset < From.Y
                        then
                           Curve_1_Turn_Start_Horizontal_End_Vertical;
                        else
                           Curve_Start_Right_3_Turns_Clockwise;
                        end if;
                     when Sud_East =>
                        Curve_Start_Right_3_Turns_Clockwise;
                     when Sud_West =>
                        Curve_Start_Right_3_Turns_Clockwise;
                  end case;
                  Next_End_Side := Top;
               when Right =>
                  Curve_2_Turns_Start_Right_End_Right;
                  Next_End_Side := Left;
               when Left =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Right_4_Turns_AntiClockwise;
                     when North_East =>
                        if From.X + Link.Offset < To.X then
                           Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                        else
                           Curve_Start_Right_4_Turns_AntiClockwise;
                        end if;
                     when Sud_East =>
                        if From.X + Link.Offset < To.X then
                           Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                        else
                           Curve_Start_Right_4_Turns_Clockwise;
                        end if;
                     when Sud_West =>
                        Curve_Start_Right_4_Turns_Clockwise;
                  end case;
                  Next_End_Side := Right;
               when Not_Defined =>
                  Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                  Next_End_Side := Right;  --  improve
            end case;
         when Left =>
            case Side_To is
               when Top =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        Curve_Start_Left_3_Turns_Clockwise;
                     when North_East =>
                        Curve_Start_Left_3_Turns_Clockwise;
                     when Sud_East =>
                        Curve_Start_Left_3_Turns_Clockwise;
                     when Sud_West =>
                        if From.Y + Link.Offset < To.Y
                          and then To.X + Link.Offset < From.X
                        then
                           Curve_1_Turn_Start_Horizontal_End_Vertical;
                        else
                           Curve_Start_Left_3_Turns_Clockwise;
                        end if;
                  end case;
                  Next_End_Side := Bottom;
               when Bottom =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        if To.X + Link.Offset < From.X
                          and then To.Y + Link.Offset < From.Y
                        then
                           Curve_1_Turn_Start_Horizontal_End_Vertical;
                        else
                           Curve_Start_Left_3_Turns_AntiClockwise;
                        end if;
                     when North_East =>
                        Curve_Start_Left_3_Turns_AntiClockwise;
                     when Sud_East =>
                        Curve_Start_Left_3_Turns_AntiClockwise;
                     when Sud_West =>
                        Curve_Start_Left_3_Turns_AntiClockwise;
                  end case;
                  Next_End_Side := Top;
               when Right =>
                  case Quadrant_Position_Of_Point_To (From, To) is
                     when North_West =>
                        if To.X + Link.Offset < From.X then
                           Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                        else
                           Curve_Start_Left_4_Turns_Clockwise;
                        end if;
                     when North_East =>
                        Curve_Start_Left_4_Turns_Clockwise;
                     when Sud_East =>
                        Curve_Start_Left_4_Turns_AntiClockwise;
                     when Sud_West =>
                        if To.X + Link.Offset < From.X then
                           Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                        else
                           Curve_Start_Left_4_Turns_AntiClockwise;
                        end if;
                  end case;
                  Next_End_Side := Left;
               when Left =>
                  Curve_2_Turns_Start_Left_End_Left;
                  Next_End_Side := Right;
               when Not_Defined =>
                  Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                  Next_End_Side := Left;
            end case;
         when Not_Defined =>
            case Side_To is
               when Left =>
                  Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                  Next_End_Side := Right;
               when Right =>
                  Curve_2_Turns_Start_End_Horizontal_With_Inflection;
                  Next_End_Side := Left;
               when Top =>
                  Curve_2_Turns_Start_End_Vertical_With_Inflection;
                  Next_End_Side := Bottom;
               when Bottom =>
                  Curve_2_Turns_Start_End_Vertical_With_Inflection;
                  Next_End_Side := Top;
               when Not_Defined =>
                  Curve_2_Turns_Start_End_Vertical_With_Inflection;
                  Next_End_Side := Continue_Side (Prev_End_Side);
            end case;
      end case;
   end Compute_Layout_For_One_Curve_Link;

   ---------------------------------------
   -- Compute_Layout_For_Curve_Link --
   ---------------------------------------

   procedure Compute_Layout_For_Curve_Link
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Rect_Item_Record'Class, Rect_Item);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Canvas_Link_Record'Class, Canvas_Link);

      function To_Side_Attachment
        (Side_Anchor : Side_Anchor_Style) return Side_Attachment;
      function To_Side_Attachment
        (Side_Anchor : Side_Anchor_Style) return Side_Attachment
      is
      begin
         case Side_Anchor is
            when Not_Defined =>
               return Auto;
            when Top =>
               return Top;
            when Right =>
               return Right;
            when Bottom =>
               return Bottom;
            when Left =>
               return Left;
         end case;
      end To_Side_Attachment;

      Dim   : constant Anchors := Compute_Anchors (Link);
      Dummy : Side_Anchor_Style;

   begin
      if Link.Waypoints = null then
         Compute_Layout_For_One_Curve_Link
           (Link, Min_Margin_From => 6.0, Max_Margin_From => 25.0,
            Min_Margin_To         => 6.0, Max_Margin_To => 25.0,
            Prev_End_Side         => Not_Defined, Next_End_Side => Dummy);
      else

         declare
            Prev_Start_Side : Side_Anchor_Style := Not_Defined;
            Next_Start_Side : Side_Anchor_Style;

            Number_Of_Waypoints : constant Integer :=
              Link.Waypoints.all'Length;
            WP : Item_Point_Array (0 .. Number_Of_Waypoints - 1);
            TLink               : Canvas_Link;
            From, To            : Rect_Item;
            Effective_From_Side : Side_Attachment;
            Best_Entry          : Side_Attachment;
            NWP                 : Integer := WP'First - 1;
         begin
            TLink :=
              Gtk_New
                (From        => Link.From, To => Link.To, Style => Link.Style,
                 Routing     => Link.Routing, Label => Link.Label,
                 Anchor_From => Link.Anchor_From,
                 Label_From  => Link.Label_From, Anchor_To => Link.Anchor_To,
                 Label_To    => Link.Label_To);

            if Link.Relative_Waypoints then
               WP :=
                 Relative_To_Array (Link.From.Position, Link.Waypoints.all);
            else
               WP := Link.Waypoints.all;
            end if;
            TLink.Waypoints := null;
            TLink.Points    := null;

            From :=
              Gtk_New_Rect (Style => Link.Style, Width => 0.0, Height => 0.0);
            From.Set_Size_Request (Width => 0.0, Height => 0.0);

            To :=
              Gtk_New_Rect (Style => Link.Style, Width => 0.0, Height => 0.0);
            To.Set_Size_Request (Width => 0.0, Height => 0.0);

            TLink.To := Abstract_Item (To);

            --  start conditions
            TLink.Set_Offset (Link.Offset);

            --  now loop
            for W of WP loop
               NWP := NWP + 1;
               TLink.To.Set_Position (W);

               if TLink.From = Link.From then
                  Effective_From_Side :=
                    Compute_Effective_Side (Link.Anchor_From, Dim.From);
                  if Number_Of_Waypoints > 1 then
                     Best_Entry :=
                       Best_Entry_Side
                         (Start_Point    => Link.From.Position,
                          Exit_Side      => Effective_From_Side,
                          End_Point      => WP (NWP),
                          Next_End_Point => WP (NWP + 1));
                  else
                     Best_Entry :=
                       Best_Entry_Side
                         (Start_Point    => Link.From.Position,
                          Exit_Side      => Effective_From_Side,
                          End_Point      => WP (NWP),
                          Next_End_Point => Link.To.Position);
                  end if;
                  TLink.Anchor_To               := Middle_Attachment;
                  TLink.Anchor_To.Toplevel_Side := Best_Entry;
                  Compute_Layout_For_One_Curve_Link
                    (TLink, Min_Margin_From => 6.0, Max_Margin_From => 25.0,
                     Min_Margin_To => 0.0, Max_Margin_To => Gdouble'Last,
                     Prev_End_Side          => Not_Defined,
                     Next_End_Side          => Next_Start_Side);
                  TLink.From := Abstract_Item (From);
               else
                  Prev_Start_Side := Continue_Side (Next_Start_Side);
                  TLink.Anchor_From               := TLink.Anchor_To;
                  TLink.Anchor_From.Toplevel_Side :=
                    To_Side_Attachment (Next_Start_Side);
                  if NWP < WP'Last then
                     Best_Entry :=
                       Best_Entry_Side
                         (Start_Point    => Link.From.Position,
                          Exit_Side      => Effective_From_Side,
                          End_Point      => WP (NWP),
                          Next_End_Point => WP (NWP + 1));
                  else
                     Best_Entry :=
                       Best_Entry_Side
                         (Start_Point    => Link.From.Position,
                          Exit_Side      => Effective_From_Side,
                          End_Point      => WP (NWP),
                          Next_End_Point => Link.To.Position);
                  end if;
                  TLink.Anchor_To               := Middle_Attachment;
                  TLink.Anchor_To.Toplevel_Side := Best_Entry;
                  Compute_Layout_For_One_Curve_Link
                    (TLink, Min_Margin_From => 0.0,
                     Max_Margin_From => Gdouble'Last, Min_Margin_To => 0.0,
                     Max_Margin_To          => Gdouble'Last,
                     Prev_End_Side          => Prev_Start_Side,
                     Next_End_Side          => Next_Start_Side);
               end if;
               if Link.Points = null then
                  Link.Points := Join_Link_Points (TLink.Points, Link.Points);
               else
                  Link.Points := Join_Link_Points (Link.Points, TLink.Points);
               end if;
               Link.Waypoints_Order (NWP) := Link.Points'Last;

               TLink.From.Set_Position (TLink.To.Position);
               TLink.Anchor_From := TLink.Anchor_To;
            end loop;

            --  final condition
            TLink.Anchor_From := TLink.Anchor_To;
            TLink.Anchor_From.Toplevel_Side :=
              To_Side_Attachment (Next_Start_Side);

            TLink.To        := Link.To;
            TLink.Anchor_To := Link.Anchor_To;
            Prev_Start_Side := Continue_Side (Next_Start_Side);
            Compute_Layout_For_One_Curve_Link
              (TLink, Min_Margin_From => 0.0, Max_Margin_From => 0.0,
               Min_Margin_To          => 6.0, Max_Margin_To => 25.0,
               Prev_End_Side => Prev_Start_Side, Next_End_Side => Dummy);
            Link.Points := Join_Link_Points (Link.Points, TLink.Points);

            Unchecked_Free (From);
            Unchecked_Free (To);
            Unchecked_Free (TLink);
         end;
      end if;

      Link.Bounding_Box :=
        Compute_Bounding_Box (Link.Points.all, Relative => False);
      Compute_Labels (Link, Context, Dim);
   end Compute_Layout_For_Curve_Link;

   ------------------
   -- Mid_Waypoint --
   ------------------

   function Mid_Waypoint
     (From : Item_Point; To : Item_Point; Offset : Gdouble) return Item_Point;
   --  Compute the midwaypoint in the curve from From to To

   function Mid_Waypoint
     (From : Item_Point; To : Item_Point; Offset : Gdouble) return Item_Point
   is
      use Ada.Numerics;
      Dx    : Gdouble;
      Dy    : Gdouble;
      D     : Gdouble;
      A     : Gdouble;
      SinA  : Gdouble;
      CosA  : Gdouble;
      Ctrl1 : Item_Point;
   begin
      if From = To then
         return From;
      end if;
      Dx    := To.X - From.X;
      Dy    := To.Y - From.Y;
      D     := Sqrt (Dx * Dx + Dy * Dy);
      Ctrl1 := (D / 2.0, Offset);
      --  Rotate with center 0,0 the angle From-To
      if To.X - From.X = 0.0 then
         A := Pi / 2.0;
      else
         A := Arctan (To.Y - From.Y, To.X - From.X);
      end if;
      SinA  := Sin (A);
      CosA  := Cos (A);
      Ctrl1 :=
        (Ctrl1.X * CosA - Ctrl1.Y * SinA, Ctrl1.X * SinA + Ctrl1.Y * CosA);
      --  translate to From
      Ctrl1 := Ctrl1 + From;
      return Ctrl1;
   end Mid_Waypoint;

   -----------------------------------
   -- Compute_Layout_For_Curve_Link --
   -----------------------------------

   procedure Compute_Layout_For_Spline_Link
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context;
      Offset : Gdouble := 10.0)
   is
      Dim : constant Anchors := Compute_Anchors (Link);

      Waypoints     : Item_Point_Array_Access := Link.Waypoints;
      P1, P2, P3    : Item_Point;
      FP, TP, Dummy : Item_Point;
      Radius, Tmp   : Gdouble;
      Must_Free_WP  : Boolean                 := False;
   begin
      Unchecked_Free (Link.Points);

      if Link.From = Link.To then
         Tmp    := Gdouble'Min (abs (Offset), Dim.From.Toplevel.Width / 2.0);
         Radius := Gdouble'Min (Tmp, Dim.From.Toplevel.Height / 2.0);

         Link.Points :=
           new Item_Point_Array'
             (Circle_From_Bezier
                (Center =>
                   Link.Model_To_Item
                     ((Dim.From.Toplevel.X + Dim.From.Toplevel.Width,
                       Dim.From.Toplevel.Y)),
                 Radius => Radius));

      else
         Compute_Side
           (Link, Link.Anchor_From, Dim.From, No_Margins, FP, Dummy);
         Compute_Side (Link, Link.Anchor_To, Dim.To, No_Margins, TP, Dummy);

         if Waypoints = null then
            if Offset = 0.0 then
               Link.Points := new Item_Point_Array'(FP, TP);
            else
               P1 := Mid_Waypoint (From => FP, To => TP, Offset => Offset);
               Waypoints    := new Item_Point_Array'((0 => P1));
               Must_Free_WP := True;
               declare
                  WL    : constant Integer := Waypoints'Length;
                  Knots : Item_Point_Array (0 .. WL + 1);
                  WP    : Item_Point_Array (0 .. 3 * (WL + 1));
               begin
                  Knots (0) := FP;
                  for I in Waypoints'Range loop
                     Knots (I + 1) := Waypoints (I);
                  end loop;
                  Knots (Knots'Last) := TP;

                  WP          := Cubic_Bezier_Spline (Knots);
                  Link.Points := new Item_Point_Array'(WP);
               end;
            end if;
         else
            declare
               WL    : constant Integer := 2 * Waypoints'Length + 1;
               Knots : Item_Point_Array (0 .. WL + 1);
               WP    : Item_Point_Array (0 .. 3 * (WL + 1));
               H     : Gdouble          := Offset;
            begin
               Knots (0) := FP;
               P1        :=
                 Mid_Waypoint
                   (From   => FP, To => Waypoints (Waypoints'First),
                    Offset => H);
               H         := -H;
               Knots (1) := P1;

               for I in Waypoints'First .. Waypoints'Last loop
                  Knots (2 * (I - Waypoints'First + 1)) := Waypoints (I);
                  if I < Waypoints'Last then
                     P3 :=
                       Mid_Waypoint
                         (From   => Waypoints (I), To => Waypoints (I + 1),
                          Offset => H);
                     H                                         := -H;
                     Knots (2 * (I - Waypoints'First + 1) + 1) := P3;
                  end if;
               end loop;

               P2 :=
                 Mid_Waypoint
                   (From => Waypoints (Waypoints'Last), To => TP, Offset => H);
               Knots (Knots'Last - 1) := P2;
               Knots (Knots'Last)     := TP;

               WP          := Cubic_Bezier_Spline (Knots);
               Link.Points := new Item_Point_Array'(WP);
            end;
         end if;
      end if;

      Link.Bounding_Box :=
        Compute_Bounding_Box (Link.Points.all, Relative => False);
      Compute_Labels (Link, Context, Dim);

      if Must_Free_WP then
         Unchecked_Free (Waypoints);
      end if;
   end Compute_Layout_For_Spline_Link;

   ------------------
   -- Prepare_Path --
   ------------------

   function Prepare_Path
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context)
      return Boolean
   is
      P : constant Item_Point_Array_Access := Link.Points;
   begin
      case Link.Routing is
         when Straight | Orthogonal =>
            return Link.Style.Path_Polyline (Context.Cr, P.all);

         when Rounded | Spline =>
            if P'Length = 2 then
               return Link.Style.Path_Polyline (Context.Cr, P.all);
            else
               return Link.Style.Path_Polycurve (Context.Cr, P.all);
            end if;

         when Line =>
            if Link.From = Link.To then
               return Link.Style.Path_Polycurve (Context.Cr, P.all);
            else
               return Link.Style.Path_Polyline (Context.Cr, P.all);
            end if;
      end case;
   end Prepare_Path;

   -----------------------------
   -- Compute Side Attachment --
   -----------------------------

   procedure Compute_Side_Attachment
     (Link      :     not null access Canvas_Link_Record'Class;
      Side_From : out Side_Anchor_Style; Side_To : out Side_Anchor_Style);

   procedure Compute_Side_Attachment
     (Link      :     not null access Canvas_Link_Record'Class;
      Side_From : out Side_Anchor_Style; Side_To : out Side_Anchor_Style)
   is
      Dim   : constant Anchors := Compute_Anchors (Link);
      Displ : Gdouble;
   begin
      if Link.Routing = Line then
         Displ := Link.Offset + Link.Style.Get_Arrow_From.Length;
      else
         Displ := Link.Style.Get_Arrow_From.Length;
      end if;
      case Link.Anchor_From.Toplevel_Side is
         when Top =>
            Side_From := Top;
         when Right =>
            Side_From := Right;
         when Bottom =>
            Side_From := Bottom;
         when Left =>
            Side_From := Left;
         when No_Clipping =>
            Side_From := Not_Defined;
         when Auto =>
            if Dim.From.Toplevel.X = Dim.From.P.X then
               Side_From := Left;
            elsif Dim.From.Toplevel.X + Dim.From.Toplevel.Width = Dim.From.P.X
            then
               Side_From := Right;
            elsif Dim.From.Toplevel.Y = Dim.From.P.Y then
               Side_From := Top;
            elsif Dim.From.Toplevel.Y + Dim.From.Toplevel.Height = Dim.From.P.Y
            then
               Side_From := Bottom;
            else
               Side_From := Not_Defined;
            end if;
      end case;
      case Link.Anchor_To.Toplevel_Side is
         when Top =>
            Side_To := Top;
         when Right =>
            Side_To := Right;
         when Bottom =>
            Side_To := Bottom;
         when Left =>
            Side_To := Left;
         when No_Clipping =>
            Side_To := Not_Defined;
         when Auto =>
            if Dim.To.Toplevel.X = Dim.To.P.X then
               Side_To := Left;
            elsif Dim.To.Toplevel.X + Dim.To.Toplevel.Width = Dim.To.P.X then
               Side_To := Right;
            elsif Dim.To.Toplevel.Y = Dim.To.P.Y then
               Side_To := Top;
            elsif Dim.To.Toplevel.Y + Dim.To.Toplevel.Height = Dim.To.P.Y then
               Side_To := Bottom;
            else
               Side_To := Not_Defined;
            end if;
      end case;
   end Compute_Side_Attachment;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Link : not null access Canvas_Link_Record'Class; Context : Draw_Context;
      Selected : Boolean)
   is
      P                  : constant Item_Point_Array_Access := Link.Points;
      Fill               : Cairo_Pattern;
      S                  : Drawing_Style;
      Side_From, Side_To : Side_Anchor_Style;
   begin
      pragma Assert (P /= null, "waypoints must be computed first");
      pragma Assert (P'Length >= 2, "no enough waypoints");

      if Prepare_Path (Link, Context) then
         --  never fill a link
         Fill := Link.Style.Get_Fill;
         Link.Style.Set_Fill (Null_Pattern);
         Link.Style.Finish_Path (Context.Cr); --  really draw the link

         Compute_Side_Attachment (Link, Side_From, Side_To);

         case Link.Routing is
            when Line =>
               if P'Length >= 4 then
                  Link.Style.Draw_Arrows_And_Symbols
                    (Cr     => Context.Cr,
                     Points =>
                       (P (P'First),
                  --  middle of the first two control points (approximate)

                        ((P (P'First + 1).X + P (P'First + 2).X) / 2.0,
                         (P (P'First + 1).Y + P (P'First + 2).Y) / 2.0),
                  --  middle of the last two control points (approximate)

                        ((P (P'Last - 1).X + P (P'Last - 2).X) / 2.0,
                         (P (P'Last - 1).Y + P (P'Last - 2).Y) / 2.0),
                        P (P'Last)),
                     Relative     => False, Side_From => Side_From,
                     Side_To      => Side_To, Routing => Link.Routing,
                     No_Waypoints => Link.Waypoints = null);
               else
                  Link.Style.Draw_Arrows_And_Symbols
                    (Cr => Context.Cr, Points => P.all, Relative => False,
                     Side_From    => Side_From, Side_To => Side_To,
                     Routing      => Link.Routing,
                     No_Waypoints => Link.Waypoints = null);
               end if;

            when others =>
               Link.Style.Draw_Arrows_And_Symbols
                 (Cr => Context.Cr, Points => P.all, Relative => False,
                  Side_From    => Side_From, Side_To => Side_To,
                  Routing      => Link.Routing,
                  No_Waypoints => Link.Waypoints = null);
         end case;

         Link.Style.Set_Fill (Fill);

         if Selected and then Context.View /= null then
            S          := Link.Style;
            Link.Style := Context.View.The_Selection_Style;

            if Prepare_Path (Link, Context) then
               Fill := Link.Style.Get_Fill;
               Link.Style.Set_Fill (Null_Pattern);
               Link.Style.Finish_Path (Context.Cr);
               Link.Style.Draw_Arrows_And_Symbols
                 (Cr => Context.Cr, Points => P.all, Relative => False,
                  Side_From    => Side_From, Side_To => Side_To,
                  Routing      => Link.Routing,
                  No_Waypoints => Link.Waypoints = null);
               Link.Style.Set_Fill (Fill);
            end if;

            Link.Style := S;
         end if;
      end if;

      if Link.Label /= null then
         Link.Label.Translate_And_Draw_Item (Context);
      end if;

      if Link.Label_From /= null then
         Link.Label_From.Translate_And_Draw_Item (Context);
      end if;

      if Link.Label_To /= null then
         Link.Label_To.Translate_And_Draw_Item (Context);
      end if;
   end Draw_Link;

   ------------------------
   -- Circle_From_Bezier --
   ------------------------

   function Circle_From_Bezier
     (Center : Item_Point; Radius : Glib.Gdouble) return Item_Point_Array
   is
      --  Magic number comes from several articles on the web, including
      --    http://www.charlespetzold.com/blog/2012/12/
      --       Bezier-Circles-and-Bezier-Ellipses.html
      --  Better approx in https://spencermortensen.com/articles/bezier-circle/

      P : Item_Point_Array (1 .. 13);
      R : constant Gdouble := Radius * 0.552_284_749_830_79;
   begin
      P (P'First + 0) := (Center.X, Center.Y - Radius);

      P (P'First + 1) := (Center.X + R, Center.Y - Radius);
      P (P'First + 2) := (Center.X + Radius, Center.Y - R);

      P (P'First + 3) := (Center.X + Radius, Center.Y);

      P (P'First + 4) := (Center.X + Radius, Center.Y + R);
      P (P'First + 5) := (Center.X + R, Center.Y + Radius);

      P (P'First + 6) := (Center.X, Center.Y + Radius);

      P (P'First + 7) := (Center.X - R, Center.Y + Radius);
      P (P'First + 8) := (Center.X - Radius, Center.Y + R);

      P (P'First + 9) := (Center.X - Radius, Center.Y);

      P (P'First + 10) := (Center.X - Radius, Center.Y - R);
      P (P'First + 11) := (Center.X - R, Center.Y - Radius);

      P (P'First + 12) := (Center.X, Center.Y - Radius);

      return P;

      --  For quadratic bezier curves, we could have used:
      --  See http://texdoc.net/texmf-dist/doc/latex/lapdf/rcircle.pdf
   end Circle_From_Bezier;

end Gtkada.Canvas_Link_Pkg.Links;
