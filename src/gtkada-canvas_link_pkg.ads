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

with Gtk.Widget;
with Gtkada.Style_Pkg;                 use Gtkada.Style_Pkg;
with Glib;                             use Glib;

with Gtkada.Canvas_Defs;               use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Abstract_Item_Pkg;  use Gtkada.Canvas_Abstract_Item_Pkg;
with Gtkada.Canvas_Container_Item_Pkg; use Gtkada.Canvas_Container_Item_Pkg;
with Gtkada.Canvas_Model_Pkg;          use Gtkada.Canvas_Model_Pkg;
with Gtkada.Canvas_View_Pkg;           use Gtkada.Canvas_View_Pkg;

package Gtkada.Canvas_Link_Pkg is

   type Canvas_Link_Record is new Abstract_Item_Record with private;
   type Canvas_Link is access all Canvas_Link_Record'Class;
   --  Special support is provided for links.
   --  These are a special kind of item, which provides automatic routing
   --  algorithms. They always join two items (including possibly two lines)

   function Gtk_New
     (From, To    : not null access Abstract_Item_Record'Class;
      Style       : Drawing_Style;
      Routing     : Route_Style := Straight;
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null)
      return Canvas_Link;

   procedure Initialize
     (Link        : not null access Canvas_Link_Record'Class;
      From, To    : not null access Abstract_Item_Record'Class;
      Style       : Drawing_Style;
      Routing     : Route_Style := Straight;
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null);
   --  Create a new link between the two items.
   --  This link is not automatically added to the model.
   --  Both items must belong to the same model.
   --
   --  The label is displayed approximately in the middle of the link.
   --  The Label_From is displayed next to the origin of the link, whereas
   --  Label_To is displayed next to the target of the link.
   --  These labels will generally be some Text_Item, but it might make sense
   --  to use more complex labels, for instance to draw something with a
   --  polyline item, or using an image.
   --
   --  If the Label is directed, the direction of the arrow will be changed
   --  automatically to match the layout of the link.

   function Get_From
     (Self : not null access Canvas_Link_Record) return Abstract_Item;

   function Get_To
     (Self : not null access Canvas_Link_Record) return Abstract_Item;
   --  Return both ends of the link

   function Get_Label
     (Self : not null access Canvas_Link_Record) return Container_Item;

   function Get_Label_From
     (Self : not null access Canvas_Link_Record) return Container_Item;

   function Get_Label_To
     (Self : not null access Canvas_Link_Record) return Container_Item;
   --  Retrieve the various label items

   procedure Set_Offset
     (Self    : not null access Canvas_Link_Record;
      Offset  : Gdouble);
   --  Orthogonal links: not used
   --  Straight links: not used
   --  Line links: displacement of the first orthogonal point
   --  Spline links: specify the curve of the arc (this is basically the
   --                maximal distance between the straight line and the
   --                summit of the arc
   --  Rounded links: used in several parts of the link such as where bezier
   --                 points are placed
   --  Offset must not be 0.0

   procedure Refresh_Layout
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context);
   --  Recompute the layout/routing for the link.
   --  This procedure should be called whenever any of the end objects changes
   --  side or position. The view will do this automatically the first time,
   --  but will not update links later on.

   procedure Set_Waypoints
     (Self     : not null access Canvas_Link_Record;
      Points   : Item_Point_Array;
      Relative : Boolean := False);
   --  Set explicit waypoints for the link, which forces the link to go through
   --  the given points.
   --  Relative should be true if all points coordinates are relative (except
   --  the first point).

   function Get_Waypoints
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array;
   --  Return the absolute waypoints, if there are any.

   type Order_Array is array (Natural range <>) of Integer;
   type Order_Array_Access is access Order_Array;

   function Get_Waypoints_Order
     (Self : not null access Canvas_Link_Record)
      return Order_Array;
   --  Return the position of each waypoint in the points of the link.

   procedure Set_Style
     (Self  : not null access Canvas_Link_Record;
      Style : Drawing_Style);

   function Get_Style
     (Self : not null access Canvas_Link_Record) return Drawing_Style;
   --  Return the style used for the drawingo of this link.
   --  When changing the style, you must force a refresh of the canvas.

   function Get_Points
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array_Access;
   --  Return the computed points for the link.
   --  Do not free or store the result

   overriding function Is_Invisible
     (Self : not null access Canvas_Link_Record)
      return Boolean is (False);

   overriding function Inner_Most_Item
     (Self           : not null access Canvas_Link_Record;
      Dummy_At_Point : Model_Point;
      Dummy_Context  : Draw_Context)
      return Abstract_Item is (Self);

   overriding function Parent
     (Self : not null access Canvas_Link_Record)
      return Abstract_Item is (null);

   overriding function Edit_Widget
     (Self       : not null access Canvas_Link_Record;
      Dummy_View : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget is (null);

   overriding procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Link_Record;
      Threshold : Gdouble);

   overriding function Get_Visibility_Threshold
     (Self : not null access Canvas_Link_Record) return Gdouble;

   overriding procedure Destroy
     (Self     : not null access Canvas_Link_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   overriding function Bounding_Box
     (Self : not null access Canvas_Link_Record)
      return Item_Rectangle;

   overriding function Position
     (Self : not null access Canvas_Link_Record)
      return Point;

   overriding procedure Draw
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context);

   overriding function Contains
     (Self    : not null access Canvas_Link_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;

   overriding function Clip_Line
     (Self   : not null access Canvas_Link_Record;
      P1, P2 : Item_Point) return Item_Point;

   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Link_Record;
      Anchor : Anchor_Attachment)
      return Item_Point;

   overriding function Is_Link
     (Self : not null access Canvas_Link_Record)
      return Boolean is (True);

   procedure Draw_As_Selected
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context);

   function Get_Routing (Self : not null access Canvas_Link_Record)
                         return Route_Style;

   procedure Unchecked_Free_Points (Self : not null access Canvas_Link_Record);

private

   No_Waypoints : constant Item_Point_Array := (1 .. 0 => (0.0, 0.0));


   type Canvas_Link_Record is new Abstract_Item_Record with record
      From, To     : Abstract_Item;
      Style        : Drawing_Style;
      Routing      : Route_Style;
      Bounding_Box : Item_Rectangle;
      Label        : Container_Item;
      Label_From   : Container_Item;
      Label_To     : Container_Item;

      Visibility_Threshold : Gdouble := 0.0;

      Offset : Gdouble := 10.0;
      --  For arc links

      Waypoints   : Item_Point_Array_Access;
      --  The waypoints created by the user (as opposed to Points, which
      --  contains the list of waypoints computed automatically, in addition
      --  to the user's waypoints).
      --  These are absolute coordinates.
      --  For straight and orthogonal links, these are the points the link must
      --  go through.
      --  For curve and arc links, these are the list of points and
      --  control points for the bezier curve:
      --      pt1, ctrl1, ctrl2, pt2, ctrl3, ctrl4, pt3, ...

      Waypoints_Order : Order_Array_Access;
      --  Position of each waypoint in the points of the link.

      Relative_Waypoints : Boolean := False;
      --  Whether the waypoints are given in relative coordinates.
      --  This does not apply to Points.

      Points   : Item_Point_Array_Access;
      --  The cached computation of waypoints for this link.
      --  These are recomputed every time the layout of the canvas changes, but
      --  are cached so that redrawing the canvas is fast.
      --  These are absolute coordinates, even if waypoints are relative.
      --  See the documentation on Waypoints for more information on the format

      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
   end record;

end Gtkada.Canvas_Link_Pkg;
