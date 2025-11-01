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

with Ada.Unchecked_Conversion;
with System;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Glib;                               use Glib;
with Glib.Values;                        use Glib.Values;
with Gtk.Widget;
with Gtkada.Handlers;                    use Gtkada.Handlers;
with Gtkada.Style_Pkg;                       use Gtkada.Style_Pkg;

with Gtkada.Canvas_Defs;                 use Gtkada.Canvas_Defs;
limited with Gtkada.Canvas_View_Pkg;
limited with Gtkada.Canvas_Model_Pkg;

package Gtkada.Canvas_Abstract_Item_Pkg is

   package CVP renames Canvas_View_Pkg;
   package CMP renames Canvas_Model_Pkg;

   --------------------
   -- Abstract Items --
   --------------------

   type Abstract_Item_Record is interface;
   type Abstract_Item is access all Abstract_Item_Record'Class;
   --  These are all the elements that can be displayed on a canvas, including
   --  the boxes, the links between the boxes, any annotations on those links,
   --  and so on.
   --  Items can be grouped, so that toplevel items contain one or more
   --  other items. The toplevel items are the ones that are moved
   --  interactively by the user, and their contained items will be moved
   --  along.
   --  All primitive operations on items, except its position, are done in the
   --  Item's own coordinate systems so that it is easy to create new types of
   --  items without paying attention to any of its parents rotation or
   --  scaling, or the rotation and scaling of the view itself).
   --
   --  This interface is meant for use when you already have ways to store
   --  coordinates and sizes in your own data types, at which point you can
   --  implement a simpler wrapper for your data type that implements this
   --  interface. In general, though, it is better to extend the type
   --  Abstract_Item_Record which provides its own non-abstract handling for a
   --  number of subprograms below.

   function GValue_To_Abstract_Item (Value : GValue) return Abstract_Item;
   function Abstract_Item_To_Address is new Ada.Unchecked_Conversion
     (Abstract_Item, System.Address);
   package Abstract_Item_Marshallers is new Object_Callback.Marshallers
     .Generic_Marshaller (Abstract_Item, GValue_To_Abstract_Item);
   procedure Abstract_Item_Emit
     is new Abstract_Item_Marshallers.Emit_By_Name_Generic
     (Abstract_Item_To_Address);
   --  support for the "item_contents_changed" signal

   package Items_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Abstract_Item);

   function Hash (Key : Abstract_Item) return Ada.Containers.Hash_Type;
   package Item_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Abstract_Item,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   type Item_Drag_Info is record
      Item : Abstract_Item;
      Pos  : Model_Point;
   end record;

   package Item_Drag_Infos is new Ada.Containers.Hashed_Maps
     (Key_Type        => Abstract_Item,
      Element_Type    => Item_Drag_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Is_Link
     (Self : not null access Abstract_Item_Record) return Boolean is abstract;
   --  Whether this item should be considered as a link between two other
   --  items.
   --  Such links have a few specific behavior: for instance, they cannot be
   --  dragged by the user to a new position (their layout is provided by the
   --  items they are linked to).
   --  They also do not contribute to the smart guides that are used while
   --  items are moved around.

   function Position
     (Self : not null access Abstract_Item_Record)
      return Gtkada.Style_Pkg.Point is abstract;
   --  The coordinates of the item within its parent.
   --  If the item has no parent, the coordinates should be returned in model
   --  coordinates. These coordinates describe the origin (0,0) point of
   --  the item's coordinate system (even if Set_Position was specified to
   --  point to another location in the item).

   procedure Set_Position
     (Self     : not null access Abstract_Item_Record;
      Pos      : Gtkada.Style_Pkg.Point) is null;
   --  Used to change the position of an item (by default an item cannot be
   --  moved). You must call the model's Refresh_Layout after moving items.

   function Bounding_Box
     (Self : not null access Abstract_Item_Record)
      return Item_Rectangle is abstract;
   --  Returns the area occupied by the item.
   --  Any drawing for the item, including shadows for instance, must be
   --  within this area.
   --  This bounding box is always returned in the item's own coordinate
   --  system, so that it is not necessary to pay attention to the current
   --  scaling factor or rotation for the item, its parents or the canvas view.

   --
   --  The coordinates of the item are always the top-left corner of their
   --  bounding box. These coordinates are either relative to the item's
   --  toplevel container, or model coordinates for toplevel items.
   --
   --  The bounding box is also used for fast detection on whether the item
   --  might be clicked on by the user.

   procedure Refresh_Layout
     (Self    : not null access Abstract_Item_Record;
      Context : CVP.Draw_Context) is null;
   --  Called when Refresh_Layout is called on the model.
   --  This is an opportunity for the item to update its size for instance, or
   --  do other computation that might impact the result of Bounding_Box.

   procedure Draw
     (Self    : not null access Abstract_Item_Record;
      Context : CVP.Draw_Context) is abstract;
   --  Draw the item on the given cairo context.
   --  A transformation matrix has already been applied to Cr, so that all
   --  drawing should be done in item-coordinates for Self, so that (0,0) is
   --  the top-left corner of Self's bounding box.
   --  Do not call this procedure directly. Instead, call
   --  Translate_And_Draw_Item below.

   procedure Translate_And_Draw_Item
     (Self          : not null access Abstract_Item_Record'Class;
      Context       : CVP.Draw_Context;
      As_Outline    : Boolean := False;
      Outline_Style : Drawing_Style := No_Drawing_Style);
   --  Translate the transformation matrix and draw the item.
   --  This procedure should be used instead of calling Draw directly.
   --  If As_Outline is true, then only the outline of the item is displayed,
   --  using the provided style

   procedure Draw_Outline
     (Self    : not null access Abstract_Item_Record;
      Style   : Gtkada.Style_Pkg.Drawing_Style;
      Context : CVP.Draw_Context) is null;
   --  Draw an outline for Self (which is used for the selection for instance).
   --  Do not call this procedure directly, use Translate_And_Draw_Item
   --  instead, unless called directly from an overriding of Draw.

   procedure Draw_As_Selected
     (Self    : not null access Abstract_Item_Record;
      Context : CVP.Draw_Context) is abstract;
   --  Draw the item when it is selected.
   --  The default is to draw both the item and its outline.
   --  Do not call this procedure directly, use Translate_And_Draw_Item
   --  instead, unless called directly from an overriding of Draw.

   function Contains
     (Self    : not null access Abstract_Item_Record;
      Point   : Item_Point;
      Context : CVP.Draw_Context) return Boolean is abstract;
   --  Should test whether Point is within the painted region for Self (i.e.
   --  whether Self should be selected when the user clicks on the point).
   --  For an item with holes, this function should return False when the
   --  point is inside one of the holes, for instance.

   function Edit_Widget
     (Self  : not null access Abstract_Item_Record;
      View  : not null access CVP.Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the widget to use for in-place editing of the item.
   --  null should be returned when the item is not editable in place.
   --  It is the responsibility of the returned widget to monitor events and
   --  validate the editing, update Self, and then call model's layout_changed
   --  signal.

   procedure Destroy
     (Self     : not null access Abstract_Item_Record;
      In_Model : not null access CMP.Canvas_Model_Record'Class) is null;
   --  Called when Self is no longer needed.
   --  Do not call directly.

   procedure Destroy_And_Free
     (Self     : in out Abstract_Item;
      In_Model : not null access CMP.Canvas_Model_Record'Class);
   --  Free the memory used by Self

   function Parent
     (Self : not null access Abstract_Item_Record)
      return Abstract_Item is abstract;
   --  Return the item inside which Self is contained.
   --  null is returned for toplevel items, in which case the coordinates of
   --  the bounding box are model coordinats. Otherwise, the coordinates are
   --  relative to the returned item.

   function Get_Toplevel_Item
     (Self : not null access Abstract_Item_Record'Class)
      return Abstract_Item;
   --  Return the toplevel item that contains Self (or self itself)

   function Inner_Most_Item
     (Self     : not null access Abstract_Item_Record;
      At_Point : Model_Point;
      Context  : CVP.Draw_Context)
      return Abstract_Item is abstract;
   --  Return the inner-most item at the specific coordinates in Self (or
   --  Self itself).

   function Link_Anchor_Point
     (Self   : not null access Abstract_Item_Record;
      Anchor : Anchor_Attachment)
      return Item_Point is abstract;
   --  Return the anchor point for links to or from this item. In general,
   --  this anchor point is in the middle of the item or depends on the
   --  Anchor parameter, and the link will automatically be clipped to one
   --  of the borders. The coordinates are absolute.
   --  This anchor point can be in the middle of an item, the link itself
   --  will be clipped with a call to Clip_Line_On_Top_Level

   function Clip_Line
     (Self   : not null access Abstract_Item_Record;
      P1, P2 : Item_Point) return Item_Point is abstract;
   --  Returns the intersection of the line from P1 to P2 with the border of
   --  the item. Drawing a line from this intersection point to P2 will not
   --  intersect the item.

   function Model_Bounding_Box
     (Self     : not null access Abstract_Item_Record'Class)
      return Model_Rectangle;
   --  Return the bounding box of Self always in model coordinates.
   --  As opposed to Bounding_Box, model coordinates are also returned
   --  for nested items.

   function Is_Invisible
     (Self : not null access Abstract_Item_Record)
     return Boolean is abstract;
   --  True if Self has no filling or stroke information (and therefore is
   --  invisible even when displayed, although some of its children might be
   --  visible).
   --  This function is independent of Set_Visibility_Threshold, Show or Hide.

   procedure Set_Visibility_Threshold
     (Self      : not null access Abstract_Item_Record;
      Threshold : Gdouble) is null;

   function Get_Visibility_Threshold
     (Self : not null access Abstract_Item_Record) return Gdouble is abstract;
   --  When the items bounding box (on the screen) width or height are less
   --  than Threshold pixels, the item is automatically hidden.
   --  Making the item invisibile does not impact the visibility of links from
   --  or to that item (but you could use Include_Related_Items to find these
   --  related items.
   --  You need to refresh the view afterwards

   procedure Show (Self : not null access Abstract_Item_Record'Class);

   procedure Hide (Self : not null access Abstract_Item_Record'Class);
   --  Hide or show the item unconditionally. This overrides the settings
   --  done by Set_Visibility_Threshold.

   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      Rect   : Item_Rectangle) return Model_Rectangle;

   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Item_Point) return Model_Point;

   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Point) return Item_Point;

   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Rectangle) return Item_Rectangle;
   --  Conversion between the various coordinate systems.
   --  Calling these should seldom be needed, as Cairo uses a transformation
   --  matrix to automatically (and efficiently) do the transformation on
   --  your behalf. See the documentation for Set_Transform.

end Gtkada.Canvas_Abstract_Item_Pkg;
