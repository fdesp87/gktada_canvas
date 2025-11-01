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

with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Gtk.Handlers;
with Pango.Layout;     use Pango.Layout;

with Gtkada.Canvas_Defs;                use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Abstract_Item_Pkg;   use Gtkada.Canvas_Abstract_Item_Pkg;
limited with Gtkada.Canvas_View_Pkg;

package Gtkada.Canvas_Model_Pkg is
   package CVP renames Gtkada.Canvas_View_Pkg;

   ------------------
   -- Canvas model --
   ------------------

   type Canvas_Model_Record
      is abstract new Glib.Object.GObject_Record with private;
   type Canvas_Model is access all Canvas_Model_Record'Class;
   --  A model is a common interface to query the list of items that should
   --  be displayed in the canvas. It does not assume anything regarding the
   --  actual storage of the items, so it is possible to create your own
   --  model implementation that simply query the rest of your application
   --  (or a database, or some other source of data) as needed, without
   --  duplicating the items.
   --
   --  This type is not an Ada interface because it needs to inherit from
   --  GObject so that it can send signals.
   --
   --  The interface does not provide support for adding items to the model:
   --  instead, this is expected to be done by the concrete implementations of
   --  the model, which must then send the signal "layout_changed".

   function Model_Get_Type return Glib.GType;
   pragma Convention (C, Model_Get_Type);
   --  Return the internal type

   procedure Initialize
     (Self : not null access Canvas_Model_Record'Class);
   --  Initialize the internal data so that signals can be sent.
   --  This procedure must always be called when you create a new model.

   procedure For_Each_Item
     (Self     : not null access Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      Selected_Only : Boolean := False;
      Filter        : Item_Kind_Filter := Kind_Any;
      In_Area       : Model_Rectangle := No_Rectangle) is abstract;
   --  Calls Callback for each item in the model, including links.
   --  Only the items that intersect In_Area should be returned for
   --  efficiency, although it is valid to return all items.
   --
   --  If Selected_Only is true, then only selected items are returned
   --
   --  Items are returned in z-layer order: lowest items first, highest items
   --  last.
   --
   --  You should not remove items while iterating, since removing items might
   --  end up removing other items (links to or from the original item for
   --  instance). Instead, create a temporary structure via
   --  Include_Related_Items and use Remove to remove them all at once.

   function Get_Selected_Items
     (Self : not null access Canvas_Model_Record) return Item_Sets.Set;
   --  Return the currently selected items. If no item is selected, an empty
   --  set is returned.

   procedure For_Each_Link
     (Self       : not null access Canvas_Model_Record;
      Callback   : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      From_Or_To : Item_Sets.Set);
   --  This iterator should return all the links in the model.
   --  If possible, it should restrict itself to the links with at least one
   --  end on an item in From_Or_To (or on a link to such an item).
   --  This function is important for performance when dragging items in a
   --  large model (tens of thousands of items). The default implementation
   --  simply calls For_Each_Item.
   --  From_Or_To is never empty.

   procedure Include_Related_Items
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set);
   --  Append Item and all items and links related to Item (i.e. the links for
   --  which one of the ends is Item, and then the links to these links, and so
   --  on).

   procedure From
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set);
   --  Append all the items with a link coming from Item

   procedure To
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set);
   --  Append all the items with a link going to Item

   function Bounding_Box
     (Self   : not null access Canvas_Model_Record;
      Margin : Model_Coordinate := 0.0)
      return Model_Rectangle;
   --  Returns the rectangle that encompasses all the items in the model.
   --  This is used by views to compute the maximum area that should be made
   --  visible.
   --  An extra margin is added to each side of the box.
   --  The default implementation is not efficient, since it will iterate all
   --  items one by one to compute the rectangle. No caching is done.

   procedure Refresh_Layout
     (Self        : not null access Canvas_Model_Record;
      Send_Signal : Boolean := True);
   --  Refresh the layout of Self.
   --  This procedure should be called every time items are moved (because
   --  this impacts links to or from these items), or when they are added or
   --  removed (it could also impact the layout of links if they displays to
   --  avoid going underneath items).
   --  This procedure is also used to compute the size of items (see
   --  Container_Item below).
   --  The default implementation will simply iterate over all items, but it
   --  could be implemented more efficiently.
   --
   --  This procedure will in general send a Layout_Changed signal if
   --  Send_Signal is true. This should in general always be left to True
   --  unless you are writting your own model.
   --
   --  WARNING: this procedure must be called only once at least one view has
   --  been created for the model. This ensures that the necessary information
   --  for the layout of text has been retrieved from the view layer. If you
   --  do not have at least one view, all text will be hidden or displayed as
   --  ellipsis.
   --  In fact, this procedure is called automatically on the model the first
   --  time it is associated with a view.

   function Toplevel_Item_At
     (Self    : not null access Canvas_Model_Record;
      Point   : Model_Point;
      Context : CVP.Draw_Context) return Abstract_Item;
   --  Return the toplevel item at the specific coordinates (if any).
   --  The default implementation simply traverses the list of items, and
   --  calls Contains on each child.
   --  This function returns the topmost item

   procedure Remove
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is null;
   --  Remove an item from the model, and destroy it.
   --  This also removes all links to and from the element, and links to
   --  these links (and so on).

   procedure Remove
     (Self : not null access Canvas_Model_Record;
      Set  : Item_Sets.Set);
   --  Remove all elements in the set from the model.
   --  It is expected that the set already contains related items (see
   --  Include_Related_Items)
   --  The default implementation is to call Remove for each of the element in
   --  the set, so you will need to override this procedure if your
   --  implementation of Remove calls this one.

   procedure Raise_Item
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is abstract;

   procedure Lower_Item
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is abstract;
   --  Change the z-order of the item.
   --  This emits the layout_changed signal

   procedure Set_Selection_Mode
     (Self : not null access Canvas_Model_Record;
      Mode : Selection_Mode);
   --  Controls whether items can be selected.
   --  Changing the mode always clears the selection.

   procedure Clear_Selection (Self : not null access Canvas_Model_Record);

   procedure Add_To_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);

   procedure Remove_From_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);

   function Is_Selected
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
      return Boolean;
   --  Handling of selection. Depending on the selection mode, some of these
   --  operations might have no effect, or might unselect the current selection
   --  before selecting a new item.
   --  The selection might contain child items (i.e. not just toplevel items).
   --
   --  Whenever the selection is changed, the signal "selection_changed" is
   --  emitted.

   function Is_Selectable
     (Self       : not null access Canvas_Model_Record;
      Dummy_Item : not null access Abstract_Item_Record'Class)
      return Boolean is (True);
   --  Whether the given item is selectable. By default, all items are
   --  selectable.

   procedure Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Item : access Abstract_Item_Record'Class := null);

   function On_Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Selection_Changed : constant Glib.Signal_Name := "selection_changed";
   --  Item is set to null when the selection was cleared, otherwise it is
   --  set to the element that was just added or removed from the selection.

   procedure Layout_Changed (Self : not null access Canvas_Model_Record'Class);

   function On_Layout_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Layout_Changed : constant Glib.Signal_Name := "layout_changed";
   --  Emits or handles the "layout_changed" signal.
   --  This signal must be emitted by models whenever new items are added,
   --  existing items are resized or removed, or any other event that impacts
   --  coordinates of any item in the model.
   --  It is recommended to emit this signal only once per batch of changes,

   procedure Item_Contents_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class);

   function On_Item_Contents_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Item_Contents_Changed : constant Glib.Signal_Name :=
     "item_contents_changed";
   --  This signal should be emitted instead of layout_changed when only the
   --  contents of an item (but not its size) has changed). This will only
   --  trigger the refresh of that specific item.

   procedure Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class);
   --  Emits the "item_destroyed" signal

   function On_Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Item_Destroyed : constant Glib.Signal_Name := "item_destroyed";
   --  This signal is emitted just before an item is destroyed.

   function The_Layout
     (Self : not null access Canvas_Model_Record'Class)
      return Pango.Layout.Pango_Layout;

   procedure Set_Layout
     (Self   : not null access Canvas_Model_Record'Class;
      Layout : Pango.Layout.Pango_Layout);

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_Drag_Infos.Map := Item_Drag_Infos.Empty_Map);
   --  Refresh the layout for all links (or only the ones linked to Item, or
   --  indirectly to a link to Item).

private

   type Canvas_Model_Record is abstract new Glib.Object.GObject_Record
   with record
      Layout    : Pango.Layout.Pango_Layout;

      Selection : Item_Sets.Set;
      Mode      : Selection_Mode := Selection_Single;
   end record;

end Gtkada.Canvas_Model_Pkg;
