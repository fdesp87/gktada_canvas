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

--  <description>
--  This package is a rewrite of Gtkada.Canvas, with hopefully more
--  capabilities and a cleaner API.
--
--  It provides a drawing area (canvas) on which items can be displayed and
--  linked together. It also supports interactive manipulation of those
--  items.
--
--  This package is organized around the concept of Model-View-Controller:
--    - The model is an item that gives access to all the items contained
--      in the canvas, although it need not necessarily own them. A default
--      model implementation is provided which indeed stores the items
--      internally, but it is possible to create a model which is a simple
--      wrapper around an application-specific API that would already have the
--      list of items.
--
--    - The view is in charge of representing the model, or a subset of it. It
--      is possible to have multiple views for a single model, each displaying
--      a different subset or a different part of the whole canvas.
--      When a view is put inside a Gtk_Scrolled_Window, it automatically
--      supports scrolling either via the scrollbars, or directly with the
--      mouse wheel or touchpad.
--
--    - The controller provides the user interaction in the canvas, and will
--      change the view and model properties when the user performs actions.
--
--  A view does not draw any background (image, grid,...). This is because
--  there are simply too many ways application want to take advantage of the
--  background. Instead, you should override the Draw_Internal primitive and
--  take advantage (optionally) of some of the helps in
--  Gtkada.Canvas_View.Views, which among other things provide ways to draw
--  grids.
--
--  Likewise, a view does not handle events by default (except for scrolling
--  when it is put in a Gtk_Scrolled_Window). This is also because applications
--  want to do widely different things (for some, clicking in the background
--  should open a menu, whereas others will want to let the user scroll by
--  dragging the mouse in the background -- likewise when clicking on items
--  for instance).
--
--  Differences with Gtkada.Canvas
--  ==============================
--
--  This package is organized around the concept of Model-View-Controller,
--  which provides a much more flexible approach. There is for instance no
--  need to duplicate the items in memory if you already have them available
--  somewhere else in your application.
--
--  Various settings that were set on an Interactive_Canvas (like the font for
--  annotations, arrow sizes,...) are now configured on each item or link
--  separately, which provides much more flexibility in what this canvas can
--  display.
--
--  The support for items is much richer: via a number of new primitive
--  operations, it is possible to control with more details the behavior of
--  items and where links should be attached to them.
--  More importantly, this package provides a ready-to-use set of predefined
--  items (rectangles, circles, text, polygons,...) which can be composited
--  and have automatic size computation. This makes it easier than before to
--  have an item that contains, for instance, a list of text fields, since
--  there is no need any more to compute the size of the text explicitly.
--
--  This package systematically use a Gdouble for coordinates (in any of the
--  coordinate systems), instead of the mix of Gint, Gdouble and Gfloat that
--  the Gtkada.Canvas is using. In fact, most of the time applications will
--  only have to deal with the item coordinate system (see below), and never
--  with the view coordinate system.
--
--  The behavior of snap-to-grid is different: whereas in Gtkada.Canvas it
--  forces items to always be aligned with the grid (with no way to have items
--  not aligned), the Canvas_View's effect is more subtle: basically, when an
--  item is moved closed enough to the grid, it will be aligned to the grid.
--  But if it is far from any grid line, you can drop it anywhere.
--  Snapping also takes into account all four edges of items, not just their
--  topleft corner.
--
--  User interaction
--  ================
--
--  By default, limited user interaction is supported:
--     * When a view is added to a Gtk_Scrolled_Window, scrolling is
--       automatically supported (it is handled by the scrolled window).
--       Users can use the mouse wheel to scroll vertically, shift and the
--       mouse wheel to scroll horizontally, or use the touchpad to navigate
--       (in general with multiple fingers).
--
--  But of course it supports much more advanced interactions, like clicking
--  on items, moving them with the mouse or keyboard,...
--
--  For this, you need to connect to the "item_event" signal, and either
--  directly handle the signal (a simple click for instance), or set some
--  data in the details parameters, to enable dragging items or the background
--  of the canvas (for scrolling). The package Gtkada.Canvas_View.Views
--  provides a number of precoded behaviors.
--
--  When dragging items, the view will scroll automatically if the mouse is
--  going outside of the visible area. Scrolling will continue while the mouse
--  stays there, even if the user does not move the mouse.
--
--  The following has not been backported yet:
--  ==========================================
--
--  Items are selected automatically when they are clicked. If Control is
--  pressed at the same time, multiple items can be selected.
--  If the background is clicked (and control is not pressed), then all items
--  are unselected.
--  Pressing and dragging the mouse in the backgroudn draws a virtual box on
--  the screen. All the items fully included in this box when it is released
--  will be selected (this will replace the current selection if Control was
--  not pressed).
--
--  </description>
--  <group>Drawing</group>
--  <testgtk>create_canvas_view.adb</testgtk>

with Ada.Containers.Doubly_Linked_Lists;
with Cairo;
with Gdk.Event;        use Gdk.Event;
with Gdk.Types;        use Gdk.Types;
private with Glib.Main;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Handlers;
with Gtk.Bin;          use Gtk.Bin;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Style_Pkg;     use Gtkada.Style_Pkg;
with Pango.Layout;     use Pango.Layout;

with Gtkada.Canvas_Defs;              use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Model_Pkg;         use Gtkada.Canvas_Model_Pkg;
with Gtkada.Canvas_Abstract_Item_Pkg; use Gtkada.Canvas_Abstract_Item_Pkg;

package Gtkada.Canvas_View_Pkg is

   -----------------
   -- Canvas view --
   -----------------

   type Canvas_View_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Canvas_View is access all Canvas_View_Record'Class;
   --  A view is a display of one particular part of the model, or a subset of
   --  it. Multiple views can be associated with a specific model, and will
   --  monitor changes to it view signals.
   --  The view automatically refreshes its display when its model changes.

   ------------------
   -- Draw context --
   ------------------

   type Draw_Context is record
      Cr     : Cairo.Cairo_Context := Cairo.Null_Context;
      Layout : Pango.Layout.Pango_Layout := null;
      View   : Canvas_View := null;
   end record;
   --  Context to perform the actual drawing

   function Build_Context
     (Self : not null access Canvas_View_Record'Class)
      return Draw_Context;
   --  Returns a draw context for the view. This context is suitable for
   --  computing sizes (in Refresh_Layout), but not for actual drawing.

   -----------------
   -- Canvas view --
   -----------------

   procedure Gtk_New
     (Self  : out Canvas_View;
      Model : access Canvas_Model_Record'Class := null);

   procedure Initialize
     (Self  : not null access Canvas_View_Record'Class;
      Model : access Canvas_Model_Record'Class := null);
   --  Create a new view which displays the model.
   --  A new reference to the model is created (and released when the view is
   --  destroyed), so that in general the code will look like:
   --       Model := new ....;
   --       Initialize (Model);
   --       Gtk_New (View, Model);
   --       Unref (Model);  --  unless you need to keep a handle on it too

   procedure Set_Model
      (Self  : not null access Canvas_View_Record'Class;
       Model : access Canvas_Model_Record'Class);
   --  Change the model, and redraw the whole draw.

   function Model
     (Self  : not null access Canvas_View_Record'Class)
      return Canvas_Model;
   --  Return the model

   function View_Get_Type return Glib.GType;
   pragma Convention (C, View_Get_Type);
   --  Return the internal type

   procedure Set_Grid_Size
     (Self : not null access Canvas_View_Record'Class;
      Size : Model_Coordinate := 30.0);
   --  Set the size of the grid.
   --  This grid is not visible by default. To display it, you should override
   --  Draw_Internal and call one of the functions in Gtkada.Canvas_View.Views.
   --
   --  This grid is also size for snapping of items while they are moved: when
   --  they are dragged to a position close to one of the grid lines, they will
   --  be moved by a small extra amount to align on this grid line.

   Default_Guide_Style : constant Gtkada.Style_Pkg.Drawing_Style :=
     Gtkada.Style_Pkg.Gtk_New (Stroke => (0.957, 0.363, 0.913, 1.0));

   procedure Set_Snap
     (Self           : not null access Canvas_View_Record'Class;
      Snap_To_Grid   : Boolean := True;
      Snap_To_Guides : Boolean := False;
      Snap_Margin    : Model_Coordinate := 5.0;
      Guides_Style   : Gtkada.Style_Pkg.Drawing_Style := Default_Guide_Style);
   --  Configure the snapping feature.
   --  When items are moved interactively, they will tend to snap to various
   --  coordinates, as defined for instance by Set_Grid_Size.
   --  For instance, when any size of the item gets close to one of the grid
   --  lines (i.e. less than Snap_Margin), it will be moved an extra small
   --  amount so that the coordinate of that size of the item is exactly that
   --  of the grid line. This results in nicer alignment of the items.
   --
   --  No snapping to grid occurs if the grid size is set to 0.

   procedure Draw_Internal
     (Self    : not null access Canvas_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle);
   --  Redraw either the whole view, or a specific part of it only.
   --  The transformation matrix has already been set on the context.
   --  This procedure can be overridden if you need to perform special
   --  operations, like drawing a grid for instance. See the various helper
   --  subprograms in Gtkada.Canvas_View.Views to do so.

   function Get_Visible_Area
     (Self : not null access Canvas_View_Record)
      return Model_Rectangle;
   --  Return the area of the model that is currently displayed in the view.
   --  This is in model coordinates (since the canvas coordinates are always
   --  from (0,0) to (Self.Get_Allocation_Width, Self.Get_Allocation_Height).

   procedure Set_Transform
     (Self   : not null access Canvas_View_Record;
      Cr     : Cairo.Cairo_Context;
      Item   : access Abstract_Item_Record'Class := null);
   --  Set the transformation matrix for the current settings (scrolling and
   --  zooming).
   --
   --  The effect is that any drawing on this context should now be done using
   --  the model coordinates, which will automatically be converted to the
   --  canvas_coordinates internally.
   --
   --  If Item is specified, all drawing becomes relative to that item
   --  instead of the position of the top-left corner of the view. All drawing
   --  to this context must then be done in item_coordinates, which will
   --  automatically be converted to canvas_coordinates internally.
   --
   --  This procedure does not need to be call directly in general, since the
   --  context passed to the Draw primitive of the item has already been set
   --  up appropriately.
   --
   --  The default coordinates follow the industry standard of having y
   --  increase downwards. This is sometimes unusual for mathematically-
   --  oriented people. One solution is to override this procedure in your
   --  own view, and call Cairo.Set_Scale as in:
   --      procedure Set_Transform (Self, Cr) is
   --          Set_Transform (Canvas_View_Record (Self.all)'Access, Cr);
   --          Cairo.Set_Scale (Cr, 1.0, -1.0);
   --  which will make y increase upwards instead.

   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : View_Rectangle) return Model_Rectangle;
   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : View_Point) return Model_Point;
   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return View_Rectangle;
   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      P      : Model_Point) return View_Point;
   function Model_To_Window
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return Window_Rectangle;
   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : Window_Rectangle) return Model_Rectangle;
   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : Window_Point) return Model_Point;

   procedure Set_Selection_Style
     (Self  : not null access Canvas_View_Record;
      Style : Gtkada.Style_Pkg.Drawing_Style);

   function Get_Selection_Style
     (Self  : not null access Canvas_View_Record)
      return Gtkada.Style_Pkg.Drawing_Style;
   --  The style used to highlight selected items

   procedure Set_Scale
     (Self     : not null access Canvas_View_Record;
      Scale    : Gdouble := 1.0;
      Preserve : Model_Point := No_Point);
   --  Changes the scaling factor for Self.
   --  This also scrolls the view so that either Preserve or the current center
   --  of the view remains at the same location in the widget, as if the user
   --  was zooming towards that specific point.
   --  See also Gtkada.Canvas_View.Views.Animate_Scale for a way to do this
   --  change via an animation.

   procedure Set_Topleft
     (Self         : not null access Canvas_View_Record;
      Topleft      : Model_Point);
   --  Set a specific position for the topleft corner of the visible area.
   --  This function is mostly useful to restore previous settings (which you
   --  can get through Get_Visible_Area). Interactively, it is likely better
   --  to call one of Center_On, Scroll_Into_View or Scale_To_Fit.

   procedure Center_On
     (Self         : not null access Canvas_View_Record;
      Center_On    : Model_Point;
      X_Pos, Y_Pos : Gdouble := 0.5;
      Duration     : Standard.Duration := 0.0);
   --  Scroll the canvas so that Center_On appears at the given position
   --  within the view (center when using 0.5, or left when using 0.0, and so
   --  on).
   --  If the duration is not 0, animation is used.

   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Item     : not null access Abstract_Item_Record'Class;
      Duration : Standard.Duration := 0.0);

   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Rect     : Model_Rectangle;
      Duration : Standard.Duration := 0.0);
   --  Do the minimal amount of scrolling to make the item or rectangle
   --  visible. If the duration is not 0, animation is used.

   function Get_Scale
     (Self : not null access Canvas_View_Record) return Gdouble;
   --  Return the current scale

   procedure Scale_To_Fit
     (Self      : not null access Canvas_View_Record;
      Rect      : Model_Rectangle := No_Rectangle;
      Min_Scale : Gdouble := 1.0 / 4.0;
      Max_Scale : Gdouble := 4.0;
      Duration  : Standard.Duration := 0.0);
   --  Chose the scale and scroll position so that the whole model (or the
   --  specified rectangle) is visible.
   --  This procedure leaves a small margin on each sides of the model, since
   --  that looks nicer.
   --  This function can be called even before Self has got a size assigned by
   --  window manager, but the computation of the scale will be delayed until
   --  an actual size is known.
   --  If a duration is specified, the scaling and scrolling will be animated

   procedure Avoid_Overlap
     (Self     : not null access Canvas_View_Record'Class;
      Avoid    : Boolean;
      Duration : Standard.Duration := 0.2);
   --  Sets whether items should avoid overlap when possible.
   --  When the user is moving items interactively and dropping them in a new
   --  position, items that would be overlapped are moved aside to make space
   --  for the new item.
   --  If Duration is not 0, the other items are animated to the new position.
   --
   --  This setting has no effect when you set the position of items
   --  explicitly via a call to Set_Position. In such cases, you can force
   --  the behavior manually by calling Gtkada.Canvas_View.Views.Reserve_Space.

   type Page_Format is record
      Width_In_Inches, Height_In_Inches : Gdouble;
   end record;

   type Predefined_Page_Format_Type is
     (A3_Portrait,
      A3_Landscape,
      A4_Portrait,
      A4_Landscape,
      Letter_Portrait,
      Letter_Landscape);

   function To_Page_Format
     (Value : Predefined_Page_Format_Type) return Page_Format
   is
     (case Value is
         when A3_Portrait => (11.7, 16.5),
         when A3_Landscape => (16.5, 11.7),
         when A4_Portrait => (8.3, 11.7),
         when A4_Landscape => (11.7, 8.3),
         when Letter_Portrait => (8.5, 11.0),
         when Letter_Landscape => (11.0, 8.5));

   type Export_Format is (Export_PDF, Export_SVG, Export_PNG);

   function Export
     (Self              : not null access Canvas_View_Record;
      Filename          : String;
      Page              : Page_Format;
      Format            : Export_Format := Export_PDF;
      Visible_Area_Only : Boolean := True)
     return Boolean;
   --  Create a file with the contents of the view (or the whole model
   --  if Visible_Area_Only is False).
   --  True is returned if the file was created successfully, False otherwise

   No_Drag_Allowed : constant Model_Rectangle := (0.0, 0.0, 0.0, 0.0);
   Drag_Anywhere   : constant Model_Rectangle :=
     (Gdouble'First, Gdouble'First, Gdouble'Last, Gdouble'Last);
   --  Values for the Event_Details.Allowed_Drag_Area field

   type Canvas_Event_Type is
     (Button_Press, Button_Release, Double_Click,
      Start_Drag, In_Drag, End_Drag, Key_Press, Scroll, Custom);
   --  The event types that are emitted for the Item_Event signal:
   --  * Button_Press is called when the user presses any mouse buttton either
   --    on an item or in the background.
   --    This event can also be used to start a drag event (by
   --    setting the Allowed_Drag_Area field of the Canvas_Event_Details).
   --    It can be used also to display contextual menus.
   --
   --  * Double_Click is used when the left mouse button is pressed twice in
   --    rapid succession (note that Button_Press is also emitted for the first
   --    click).
   --
   --  * Start_Drag is used after a user has pressed a mouse button, and the
   --    callback has enabled a drag area, and the mouse has moved by at least
   --    a small margin. It applies to either the item (and all other selected
   --    items, or to the background, for instance to scroll the canvas).
   --
   --  * In_Drag is used during an actual drag.
   --
   --  * End_Drag is used after a successfull drag, when the mouse is released.
   --
   --  * Button_Release is called when the mouse is released but no drag action
   --    too place. This is the event to use to modify the current selection,
   --    either by unselecting everything, adding the specific item to the
   --    selection,...
   --
   --  * Key_Press is used when the user types something on the keyboard while
   --    the canvas has the focus. It can be used to move items with the arrow
   --    keys, edit an item,...
   --
   --  * Scroll is used when the user uses the mouse wheel. It is not possible
   --    to start a drag from this event.
   --    In the Canvas_Event_Details, the button is set to either 5 or 6,
   --    depending on the direction of the scrolling.
   --
   --  * Custom is used when generating a custom event from the code.

   type Canvas_Event_Details is record
      Event_Type     : Canvas_Event_Type;
      Button         : Guint;

      State          : Gdk.Types.Gdk_Modifier_Type;
      --  The modifier keys (shift, alt, control). It can be used to activate
      --  different behavior in such cases.

      Key            : Gdk.Types.Gdk_Key_Type;
      --  The key that was pressed (for key events)

      Root_Point     : Gtkada.Style_Pkg.Point;
      --  Coordinates in root window.
      --  Attributes of the low-level event.
      --   This is an implementation detail for proper handling of dragging.

      M_Point        : Model_Point;
      --  Where in the model the user clicked. This is independent of the zoom
      --  level or current scrolling.

      Item           : Abstract_Item;
      --  The actual item that was clicked.
      --  Set to null when the user clicked in the background.

      Toplevel_Item  : Abstract_Item;
      --  The toplevel item that contains Item (might be Item itself).
      --  Set to null when the user clicked in the background.

      T_Point        : Item_Point;
      --  The corodinates of the click in toplevel_item

      I_Point        : Item_Point;
      --  The coordinates of the click in Item

      Allowed_Drag_Area : Model_Rectangle := No_Drag_Allowed;
      --  Allowed_Drag_Area should be modified by the callback when the event
      --  is a button_press event. It should be set to the area within which
      --  the item (and all currently selected items) can be moved. If you
      --  leave it to No_Drag_Allowed, the item cannot be moved.
      --
      --  This field is ignored for events other than button_press, since it
      --  makes no sense for instance to start a drag on a button release.

      Allow_Snapping    : Boolean := True;
      --  If set to False, this temporary overrides the settings from
      --  Set_Snap, and prevents any snapping on the grid or smart guides.
      --  It should be set at the same time that Allowed_Drag_Area is set.
   end record;
   type Event_Details_Access is not null access all Canvas_Event_Details;
   --  This record describes high-level aspects of user interaction with the
   --  canvas.

   Null_Canvas_Event_Details : constant Canvas_Event_Details :=
     Canvas_Event_Details'
       (Event_Type        => Custom,
        Button            => 0,
        State             => 0,
        Key               => 0,
        Root_Point        => (0.0, 0.0),
        M_Point           => (0.0, 0.0),
        Item              => null,
        Toplevel_Item     => null,
        T_Point           => (0.0, 0.0),
        I_Point           => (0.0, 0.0),
        Allowed_Drag_Area => (0.0, 0.0, 0.0, 0.0),
        Allow_Snapping    => False);

   procedure Initialize_Details
     (Self    : not null access Canvas_View_Record'Class;
      Details : out Canvas_Event_Details);
   --  Initialize Details for a Custom event type.
   --  When you have a real Gtk event, better to use Set_Details below.

   procedure Set_Details
     (Self    : not null access Canvas_View_Record'Class;
      Details : out Canvas_Event_Details;
      Event   : Gdk.Event.Gdk_Event_Button);
   --  Set the details from a specific gtk+ event

   procedure Viewport_Changed
     (Self   : not null access Canvas_View_Record'Class);

   function On_Viewport_Changed
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Viewport_Changed : constant Glib.Signal_Name := "viewport_changed";
   --  This signal is emitted whenever the view is zoomed or scrolled.
   --  This can be used for instance to synchronize multiple views, or display
   --  a "mini-map" of the whole view.

   function Item_Event
     (Self    : not null access Canvas_View_Record'Class;
      Details : Event_Details_Access) return Boolean;

   procedure On_Item_Event
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access function
        (Self    : not null access GObject_Record'Class;
         Details : Event_Details_Access)
      return Boolean;
      Slot : access GObject_Record'Class := null);
   Signal_Item_Event : constant Glib.Signal_Name := "item_event";
   --  This signal is emitted whenever the user interacts with an item (button
   --  press or release, key events,...).
   --  It is recommended to connect to this signal rather than the lower-level
   --  Button_Press_Event, Button_Release_Event,... since most information is
   --  provided here in the form of the details parameter.
   --
   --  The callback should return True if the event was processed, or False if
   --  the default handling should be performed.
   --
   --  The package Gtkada.Canvas_View.Views contains a number of examples of
   --  compatible callbacks which enable behaviors such as a moving items,
   --  scrolling the canvas by dragging the background,...

   procedure Inline_Editing_Started
     (Self : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class);

   function On_Inline_Editing_Started
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Inline_Editing_Started : constant Glib.Signal_Name :=
      "inline_editing_started";
   --  Called when the user starts inline editing of items.

   procedure Inline_Editing_Finished
     (Self : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class);

   function On_Inline_Editing_Finished
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Inline_Editing_Finished : constant Glib.Signal_Name :=
      "inline_editing_finished";
   --  Called when the user finishes (cancels ot validates) inline
   --  editing of items.

   function Size_Above_Threshold
     (Self : not null access Abstract_Item_Record'Class;
      View : access Canvas_View_Record'Class) return Boolean;
   --  Whether the item's size is above the visibility threshold, i.e. whether
   --  the item is visible.

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_Drag_Infos.Map := Item_Drag_Infos.Empty_Map);
   --  Refresh the layout for all links (or only the ones linked to Item, or
   --  indirectly to a link to Item).

   function The_Selection_Style (Self : not null access Canvas_View_Record'Class)
                                 return Gtkada.Style_Pkg.Drawing_Style;

   function The_Scale (Self : not null access Canvas_View_Record'Class)
                       return Gdouble;

   function The_Inline_Edit_Item (Self : not null access Canvas_View_Record'Class)
                                  return Abstract_Item;

private

   View_Margin : constant View_Coordinate := 20.0;
   --  The number of blank pixels on each sides of the view. This avoids having
   --  items displays exactly next to the border of the view.

   type Continuous_Scroll_Data is record
      Id      : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  The timeout callback used to provide continuous scrolling

      Dx, Dy  : Model_Coordinate := 0.0;
      --  Amount of scrolling at each step

      Timeout : Glib.Guint := 30;
      --  Number of milliseconds between each step of the auto scrolling

      Margin  : View_Coordinate := 10.0;
      --  Number of pixels on each side of the view in which the auto
      --  scrolling should start. We can't start it only when the mouse is
      --  outside of the view, since otherwise there would be no way to get
      --  it started when the view is aligned with the screen edge.

      Speed   : Model_Coordinate := 15.0;
      --  Speed of the scrolling at each step
   end record;

   type Smart_Guide is record
      Pos        : Model_Coordinate;
      Min, Max   : Model_Coordinate;
      Visible    : Boolean := False;
   end record;
   --  Description for a smart guide:
   --  For a horizontal guide, Pos is the y coordinate of the guide, and
   --  Min,Max are its minimum and maximum x coordinates for all items along
   --  that guide.

   package Smart_Guide_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Smart_Guide);

   type Snap_Data is record
      Grid             : Boolean := True;
      Smart_Guides     : Boolean := False;
      Margin           : Model_Coordinate := 5.0;

      Hguides, Vguides : Smart_Guide_Lists.List;
      Style            : Gtkada.Style_Pkg.Drawing_Style := Default_Guide_Style;
   end record;
   type Inline_Edit_Data is record
      Item : Abstract_Item;
   end record;
   --  Data used when editing a widget

   type Base_Animation_Data is abstract tagged null record;
   type Base_Animation_Data_Access is access Base_Animation_Data'Class;

   type Canvas_View_Record is new Gtk.Bin.Gtk_Bin_Record with record
      Model     : Canvas_Model;
      Topleft   : Model_Point := (0.0, 0.0);
      Scale     : Gdouble := 1.0;
      Grid_Size : Model_Coordinate := 20.0;

      Animation_Data : Base_Animation_Data_Access;
      Id_Animation   : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  The animation loop (see Gtkada.Canvas_View.Views.Animate)

      Id_Layout_Changed,
      Id_Item_Contents_Changed,
      Id_Item_Destroyed,
      Id_Selection_Changed : Gtk.Handlers.Handler_Id :=
         (Gtk.Handlers.Null_Handler_Id, null);
      --  Connections to model signals

      Layout     : Pango.Layout.Pango_Layout;
      Hadj, Vadj : Gtk.Adjustment.Gtk_Adjustment;

      Selection_Style : Gtkada.Style_Pkg.Drawing_Style :=
        Gtkada.Style_Pkg.Gtk_New
          (Stroke     => (0.8, 0.0, 0.0, 0.3),
           Line_Width => 4.0);

      Scale_To_Fit_Requested : Gdouble := 0.0;
      Scale_To_Fit_Area : Model_Rectangle;
      --  Set to true when the user calls Scale_To_Fit before the view has had
      --  a size allocated (and thus we could not perform computation).
      --  This is set to the maximal zoom requested (or 0.0 if not requested)

      Last_Button_Press : Canvas_Event_Details;
      --  Attributes of the last button_press event, used to properly handle
      --  dragging and avoid recomputing the selectd item on button_release.

      Dragged_Items : Item_Drag_Infos.Map;
      --  The items that are being dragged.

      In_Drag : Boolean := False;
      --  Whether we are in the middle of a drag.

      Topleft_At_Drag_Start : Model_Point;
      --  Toplevel at the stat of the drag

      Avoid_Overlap : Boolean := False;
      Avoid_Overlap_Duration : Standard.Duration := 0.2;

      Continuous_Scroll : Continuous_Scroll_Data;
      Snap              : Snap_Data;
      Inline_Edit       : Inline_Edit_Data;
   end record;

   procedure Copy_Selected_To_Dragged_Items
     (Self  : not null access Canvas_View_Record'Class;
      Force : access Abstract_Item_Record'Class);
   --  Setup the 'dragged_items" field from the contents of the selection, and
   --  forces a specific item to be there (in addition)

   procedure Set_Adjustment_Values
     (Self : not null access Canvas_View_Record'Class);
   --  Update the values for both adjustments

end Gtkada.Canvas_View_Pkg;
