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
with Cairo;                         use Cairo;

with Gtkada.Canvas_Link_Pkg.Links;  use Gtkada.Canvas_Link_Pkg.Links;

package body Gtkada.Canvas_Link_Pkg is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Point_Array, Point_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Order_Array, Order_Array_Access);

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New
     (From, To    : not null access Abstract_Item_Record'Class;
      Style       : Drawing_Style;
      Routing     : Route_Style := Straight;
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null)
      return Canvas_Link
   is
      L : constant Canvas_Link := new Canvas_Link_Record;
      F : Anchor_Attachment := Anchor_From;
      T : Anchor_Attachment := Anchor_To;
   begin
      if From.Is_Link then
         F.Toplevel_Side := No_Clipping;
      end if;

      if To.Is_Link then
         T.Toplevel_Side := No_Clipping;
      end if;

      Initialize
        (L, From, To, Style, Routing, Label, F, Label_From, T, Label_To);
      return L;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Link        : not null access Canvas_Link_Record'Class;
      From, To    : not null access Abstract_Item_Record'Class;
      Style       : Drawing_Style;
      Routing     : Route_Style := Straight;
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null)
   is
   begin
      Link.From        := Abstract_Item (From);
      Link.To          := Abstract_Item (To);
      Link.Style       := Style;
      Link.Routing     := Routing;
      Link.Anchor_From := Anchor_From;
      Link.Anchor_To   := Anchor_To;
      Link.Label       := Container_Item (Label);
      Link.Label_From  := Container_Item (Label_From);
      Link.Label_To    := Container_Item (Label_To);
   end Initialize;

   --------------
   -- Get_From --
   --------------

   function Get_From
     (Self : not null access Canvas_Link_Record) return Abstract_Item is
   begin
      return Self.From;
   end Get_From;

   ------------
   -- Get_To --
   ------------

   function Get_To
     (Self : not null access Canvas_Link_Record) return Abstract_Item is
   begin
      return Self.To;
   end Get_To;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Self : not null access Canvas_Link_Record) return Container_Item is
   begin
      return Self.Label;
   end Get_Label;

   --------------------
   -- Get_Label_From --
   --------------------

   function Get_Label_From
     (Self : not null access Canvas_Link_Record) return Container_Item is
   begin
      return Self.Label_From;
   end Get_Label_From;

   ------------------
   -- Get_Label_To --
   ------------------

   function Get_Label_To
     (Self : not null access Canvas_Link_Record) return Container_Item is
   begin
      return Self.Label_To;
   end Get_Label_To;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
     (Self    : not null access Canvas_Link_Record;
      Offset  : Gdouble)
   is
   begin
      if Self.Offset < 0.0 then
         case Self.Routing is
            when Line | Rounded =>
               Self.Offset := 0.0;
               return;
            when Straight | Orthogonal | Spline => null;
         end case;
      end if;
      Self.Offset := Offset;
   end Set_Offset;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context) is
   begin
      --  Target links must already have their own layout

      if Self.From.Is_Link
         and then Canvas_Link (Self.From).Points = null
      then
         Canvas_Link (Self.From).Refresh_Layout (Context);
      end if;

      if Self.To.Is_Link
         and then Canvas_Link (Self.To).Points = null
      then
         Canvas_Link (Self.To).Refresh_Layout (Context);
      end if;

      case Self.Routing is
         when Orthogonal =>
            Compute_Layout_For_Orthogonal_Link (Self, Context);
         when Straight =>
            Compute_Layout_For_Straight_Link (Self, Context);
         when Line =>
            Compute_Layout_For_Line_Link (Self, Context, Self.Offset);
         when Rounded =>
            Compute_Layout_For_Curve_Link (Self, Context);
         when Spline =>
            Compute_Layout_For_Spline_Link (Self, Context, Self.Offset);
      end case;
   end Refresh_Layout;

   -------------------
   -- Set_Waypoints --
   -------------------

   procedure Set_Waypoints
     (Self     : not null access Canvas_Link_Record;
      Points   : Item_Point_Array;
      Relative : Boolean := False) is
   begin
      Unchecked_Free (Self.Waypoints);
      if Points'Length /= 0 then
         Self.Waypoints := new Item_Point_Array'(Points);
      end if;
      Self.Relative_Waypoints := Relative;

      Unchecked_Free (Self.Waypoints_Order);
      if Points'Length /= 0 then
         Self.Waypoints_Order := new Order_Array (Points'First .. Points'Last);
         Self.Waypoints_Order.all := (others => -1);
      end if;
   end Set_Waypoints;

   -------------------
   -- Get_Waypoints --
   -------------------

   function Get_Waypoints
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array is
   begin
      if Self.Waypoints /= null then
         return Self.Waypoints.all;
      else
         return Item_Point_Array'(1 .. 0 => <>);
      end if;
   end Get_Waypoints;

   -------------------------
   -- Get_Waypoints_Order --
   -------------------------

   function Get_Waypoints_Order
     (Self : not null access Canvas_Link_Record)
      return Order_Array is
   begin
      if Self.Waypoints_Order /= null then
         return Self.Waypoints_Order.all;
      else
         return Order_Array'(1 .. 0 => <>);
      end if;
   end Get_Waypoints_Order;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Self : not null access Canvas_Link_Record) return Drawing_Style is
   begin
      return Self.Style;
   end Get_Style;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Self  : not null access Canvas_Link_Record;
      Style : Drawing_Style) is
   begin
      Self.Style := Style;
   end Set_Style;

   ----------------
   -- Get_Points --
   ----------------

   function Get_Points
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array_Access is
   begin
      return Self.Points;
   end Get_Points;

   ------------------------------
   -- Set_Visibility_Threshold --
   ------------------------------

   overriding procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Link_Record;
      Threshold : Gdouble)
   is
   begin
      Self.Visibility_Threshold := Threshold;
   end Set_Visibility_Threshold;

   ------------------------------
   -- Get_Visibility_Threshold --
   ------------------------------

   overriding function Get_Visibility_Threshold
     (Self : not null access Canvas_Link_Record) return Gdouble is
   begin
      return Self.Visibility_Threshold;
   end Get_Visibility_Threshold;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : not null access Canvas_Link_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
   begin
      Unchecked_Free (Self.Points);
      Unchecked_Free (Self.Waypoints);
      Destroy_And_Free (Abstract_Item (Self.Label), In_Model);
      Destroy_And_Free (Abstract_Item (Self.Label_From), In_Model);
      Destroy_And_Free (Abstract_Item (Self.Label_To), In_Model);

      --  Can't call destroy from the interface, this results in infinite loop
      --  Abstract_Item_Record (Self.all).Destroy (In_Model); --  inherited
   end Destroy;

   ------------------
   -- Bounding_Box --
   ------------------

   overriding function Bounding_Box
     (Self : not null access Canvas_Link_Record)
      return Item_Rectangle is
   begin
      return Self.Bounding_Box;
   end Bounding_Box;

   --------------
   -- Position --
   --------------

   overriding function Position
     (Self : not null access Canvas_Link_Record)
      return Point
   is
      pragma Unreferenced (Self);
   begin
      --  Since we'll be using model coordinates to draw the link
      return (0.0, 0.0);
   end Position;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context) is
   begin
      if Size_Above_Threshold (Self, Context.View) then
         Canvas_Link_Pkg.Links.Draw_Link (Self, Context, Selected => False);
      end if;
   end Draw;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Canvas_Link_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      Tolerance : constant Gdouble := 10.0;
   begin
      if Self.Points = null then
         return False;
      end if;

      Save (Context.Cr);
      Set_Line_Width (Context.Cr, Tolerance);

      return Prepare_Path (Self, Context)
        and then In_Stroke (Context.Cr, Point.X, Point.Y);
   end Contains;

   --------Gtk_New-------
   -- Clip_Line --
   ---------------

   overriding function Clip_Line
     (Self   : not null access Canvas_Link_Record;
      P1, P2 : Item_Point) return Item_Point
   is
      pragma Unreferenced (Self, P2);
   begin
      return P1;
   end Clip_Line;

   -----------------------
   -- Link_Anchor_Point --
   -----------------------

   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Link_Record;
      Anchor : Anchor_Attachment)
      return Item_Point
   is
      pragma Unreferenced (Anchor);
      Index : Integer;
   begin
      --  We connect to the middle of the middle segment.
      --  ?Gtk_New?? We could instead look for the closest segment or some other
      --  algorithm.

      Index := Self.Points'First + Self.Points'Length / 2 - 1;

      return ((Self.Points (Index).X + Self.Points (Index + 1).X) / 2.0,
              (Self.Points (Index).Y + Self.Points (Index + 1).Y) / 2.0);
   end Link_Anchor_Point;

   ----------------------
   -- Draw_As_Selected --
   ----------------------

   procedure Draw_As_Selected
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context) is
   begin
      if Size_Above_Threshold (Self, Context.View) then
         Canvas_Link_Pkg.Links.Draw_Link (Self, Context, Selected => True);
      end if;
   end Draw_As_Selected;

   ---------------------------
   -- Unchecked Free Points --
   --------------------------

   procedure Unchecked_Free_Points (Self : not null access Canvas_Link_Record) is
   begin
      Unchecked_Free (Self.Points);
   end Unchecked_Free_Points;

   -----------------
   -- Get_Routing --
   -----------------

   function Get_Routing (Self : not null access Canvas_Link_Record)
                         return Route_Style is
   begin
      return Self.Routing;
   end Get_Routing;

end Gtkada.Canvas_Link_Pkg;
