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

with Glib;                       use Glib;
with Gtkada.Style_Pkg;           use Gtkada.Style_Pkg;

package Gtkada.Canvas_Defs is

   -----------------
   -- Coordinates --
   -----------------
   --  There are multiple coordinate systems used in this API. Here is a full
   --  description:
   --
   --  - Model coordinates: these are the coordinates of items without
   --    considering canvas scrolling or zooming. These do not change when the
   --    view is zoomed or scrolled, and these are therefore the coordinates
   --    that are stored in the model.
   --    The drawing of links is done within this system.
   --    These coordinates are in general oriented so that x increases towards
   --    the right, and y increases towards the bottom of the screen. This
   --    can be changed by overriding Set_Transform below.
   --
   --  - View coordinates: these are the coordinates of items in the widget
   --    representing the view. They change when the view is scrolled or
   --    zoomed. These coordinates are mostly an implementation detail.
   --
   --  - Item coordinates: these are the coordinates relative to the
   --    top-left corner of an item as if it was displayed at a zoom level of
   --    100%. All drawing of items is done with this system, so that the
   --    same item can be displayed at different positions in the view
   --    without changing the drawing instructions.
   --    The drawing coordinates are automatically converted to the view
   --    coordinates by the use of a transformation matrix, which is done very
   --    efficiently on modern systems.
   --
   --  - Window coordinates
   --    These are rarely used, only when interfacing with gtk+ events. These
   --    are the coordinates relative to the Gdk_Window of the view.

   subtype Model_Coordinate  is Gdouble;
   subtype View_Coordinate   is Gdouble;
   subtype Item_Coordinate   is Gdouble;
   subtype Window_Coordinate is Gdouble;
   --  We use subtypes for convenience in your applications to avoid casts.

   type Model_Rectangle  is record
     X, Y, Width, Height : Model_Coordinate;
   end record;
   type View_Rectangle   is record
      X, Y, Width, Height : View_Coordinate;
   end record;
   type Item_Rectangle   is record
      X, Y, Width, Height : Item_Coordinate;
   end record;
   type Window_Rectangle is record
      X, Y, Width, Height : Window_Coordinate;
   end record;

   --  A rectangle in various coordinates

   type Model_Point is record
      X, Y : Model_Coordinate;
   end record;
   type View_Point  is record
      X, Y : View_Coordinate;
   end record;
   type Window_Point  is record
      X, Y : Window_Coordinate;
   end record;

   subtype Item_Point is Gtkada.Style_Pkg.Point;
   --  A point in various coordinates

   type Model_Point_Array is array (Natural range <>) of Model_Point;
   type Model_Point_Array_Access is access Model_Point_Array;

   subtype Item_Point_Array is Gtkada.Style_Pkg.Point_Array;
   subtype Item_Point_Array_Access is Gtkada.Style_Pkg.Point_Array_Access;

   No_Rectangle  : constant Model_Rectangle := (0.0, 0.0, 0.0, 0.0);
   No_Point      : constant Model_Point := (Gdouble'First, Gdouble'First);
   No_Item_Point : constant Item_Point := (Gdouble'First, Gdouble'First);

   function Point_In_Rect
     (Rect : Model_Rectangle; P : Model_Point) return Boolean;
   function Point_In_Rect
     (Rect : Item_Rectangle; P : Item_Point) return Boolean;
   --  Whether the point is in the rectangle

   function Intersects (Rect1, Rect2 : Model_Rectangle) return Boolean;
   function Intersects (Rect1, Rect2 : Item_Rectangle) return Boolean;
   --  Whether the two rectangles intersect.

   procedure Union
     (Rect1 : in out Model_Rectangle;
      Rect2 : Model_Rectangle);
   --  Store in Rect1 the minimum rectangle that contains both Rect1 and Rect2.

   function Intersection
     (P11, P12, P21, P22 : Item_Point) return Item_Point;
   --  Compute the intersection of the two segments (P11,P12) and (P21,P22).
   --  The result has X set to Gdouble'First when no intersection exists

   ------------------
   -- Enumerations --
   ------------------

   type Side_Attachment is (Auto, Top, Right, Bottom, Left, No_Clipping);
   --  Which side of the toplevel item the link is attached to.
   --
   --  For toplevel items, this can be controlled by using the
   --  Anchor_Attachment's X and Y properties.
   --  But for nested item, this forces the link to start from the
   --  toplevel item's border. Here is an example:
   --        +----------+
   --        | +-+      |
   --        | |A|      |\
   --        | +-+      | \1
   --        |     B    |\ \
   --        +----------+ \ \
   --                     2\ +----------------+
   --                       \|       C        |
   --                        +----------------+
   --
   --  The link 1 is attached to the nested item A, and the side_attachment
   --  is set to Right. As a result, it always starts at the same height as A
   --  itself.
   --  The link 2 is also attached to A, but the side is set to Auto. So the
   --  canvas draws the shortest path from A to C (and clips the line to the
   --  border of B). So it is not as visible that 2 is linked to A.
   --
   --  The "No_Clipping" side should be used when a link is connected to
   --  another link, since in that case there is no notion of link.

   type Anchor_Attachment is record
      X, Y          : Glib.Gdouble := 0.5;
      Toplevel_Side : Side_Attachment := Auto;
      Distance      : Model_Coordinate := 0.0;
   end record;
   Middle_Attachment : constant Anchor_Attachment := (0.5, 0.5, Auto, 0.0);
   --  Where in the item the link is attached (0.5 means the middle, 0.0
   --  means left or top, and 1.0 means right or bottom).
   --
   --  For the target side of a link, if X or Y are negative, Gtkada will try
   --  to draw a strictly orthogonal or vertical segment next on that end by
   --  adjusting the location of the end point along the border of the item. If
   --  it cannot, then GtkAda will use the absolute value of X and Y to specify
   --  the attachment.
   --
   --  You can therefore force a link to always emerge from the right side of
   --  an item by setting X to 1.0 and Y to any value, for instance.
   --  See the description of Side_Attachment for an example on how to use
   --  Toplevel_Side.
   --  Distance indicates at which distance from the border of the item the
   --  link should stop. By default, it reaches the border.

   type Size_Unit is (Unit_Pixels, Unit_Percent, Unit_Auto, Unit_Fit);
   --  A size can be expressed either in actual screen pixels, or
   --  proportionnaly to the parent's size.
   --  When the unit is Unit_Auto, the size of the item is computed
   --  automatically based on its children or its own intrinsic needs
   --  (for a text, this is the size needed to display the text in the given
   --  font).
   --  When the unit is Unit_Fit: this sets the width of a child so that
   --  this width plus the child's margins take the full width of the parent
   --  container. Setting a width to 100% using Unit_Percent would not take
   --  the margins into account, so that the full size (margins+width) might
   --  actually be wider than the parent. When the parent layout is
   --  horizontal, the above description applies to the height of the child.
   --  In both cases, Unit_Fit is ignored for the other axis (height for
   --  a vertical layout), in which case the child's height will be that
   --  computed from the children.

   type Size (Unit : Size_Unit := Unit_Pixels) is record
      case Unit is
         when Unit_Auto | Unit_Fit =>
            null;
         when Unit_Pixels =>
            Length : Model_Coordinate;
         when Unit_Percent =>
            Value  : Percent;
      end case;
   end record;

   Auto_Size : constant Size := (Unit => Unit_Auto);
   Fit_Size : constant Size := (Unit => Unit_Fit);
   --  See the descriptions for Size_Unit.

   Fit_Size_As_Double  : constant Model_Coordinate := -1.0;
   Auto_Size_As_Double : constant Model_Coordinate := -2.0;
   --  See the description of Fit_Size and Auto_Size.
   --  These are used for parameters that take a Double instead of a Size
   --  for backward compatibility (consider using Set_Size instead).


   function Size_From_Value (Value : Model_Coordinate) return Size;
   pragma Inline (Size_From_Value);
   --  Return a size suitable for internal use, given a size (in pixels)
   --  given by the user. When the user provides a negative size, it is
   --  meant to indicate an automatic sizing (backward compatibility)

   type Text_Arrow_Direction is
     (No_Text_Arrow,
      Up_Text_Arrow,
      Down_Text_Arrow,
      Left_Text_Arrow,
      Right_Text_Arrow);

   No_Position : constant Gtkada.Style_Pkg.Point := (Gdouble'First, Gdouble'First);
   --  Indicates that the item did not get assigned a proper position

   type Margins is record
      Top, Right, Bottom, Left : Model_Coordinate;
   end record;
   No_Margins : constant Margins := (0.0, 0.0, 0.0, 0.0);

   type Alignment_Style is (Align_Start, Align_Center, Align_End);
   --  How an item should be aligned within its parent.
   --  When the parent stacks its children vertically, alignment is taken into
   --  account horizontally; and similarly when the parent organizes its
   --  children horizontally, the alignment is vertical.
   --
   --  When an item does not request a specific size along the alignment axis,
   --  it always uses the full width or height of its parent, so the alignment
   --  does not play a role.
   --
   --  However, when the item requests a size smaller than its parent's along
   --  the alignment axis, extra margin needs to be added, and they are added
   --  either to its left/top (when Align_Start), to both sides (when
   --  Align_Center), or to its right/bottom (when Align_End)..
   --
   --  Alignment does not apply to floating children, nor to children with
   --  a specific position given along a specific axis (in which case the
   --  Anchor_X or Anchor_Y might be used for a slightly similar effect).

   type Overflow_Style is (Overflow_Prevent, Overflow_Hide);
   --  An overflow situation occurs when an item's contents is larger than its
   --  contents.
   --  If Overflow_Prevent is true, an item will always request enough size to
   --  fit all its contents. There might still be cases where the parent item
   --  was set to a small size, though, and the overflow is hidden nonetheless.
   --  If Overflow_Hide is true, an item will request a minimal size, and
   --  simply hide the part of its contents that does not fit.

   -----------------
   --  for models --
   -----------------
   type Item_Kind_Filter is (Kind_Item, Kind_Link, Kind_Any);

   type Selection_Mode is
     (Selection_None, Selection_Single, Selection_Multiple);
end Gtkada.Canvas_Defs;
