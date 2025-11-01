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

with Gtkada.Style_Pkg;                   use Gtkada.Style_Pkg;
with Glib;                           use Glib;

with Gtkada.Canvas_Defs;                    use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Abstract_Item_Pkg;       use Gtkada.Canvas_Abstract_Item_Pkg;
with Gtkada.Canvas_Item_Pkg;                use Gtkada.Canvas_Item_Pkg;
with Gtkada.Canvas_Model_Pkg;               use Gtkada.Canvas_Model_Pkg;
with Gtkada.Canvas_View_Pkg;                use Gtkada.Canvas_View_Pkg;

package Gtkada.Canvas_Container_Item_Pkg is

   type Container_Item_Record is abstract new Canvas_Item_Record with private;
   type Container_Item is access all Container_Item_Record'Class;

   type Child_Layout_Strategy is (Horizontal_Stack, Vertical_Stack);
   procedure Set_Child_Layout
     (Self   : not null access Container_Item_Record'Class;
      Layout : Child_Layout_Strategy);
   --  How should the children of a container be organized: either one on top
   --  of another, or one next to another.


   procedure Add_Child
     (Self     : not null access Container_Item_Record'Class;
      Child    : not null access Container_Item_Record'Class;
      Align    : Alignment_Style := Align_Start;
      Pack_End : Boolean := False;
      Margin   : Margins := No_Margins;
      Float    : Boolean := False;
      Overflow : Overflow_Style := Overflow_Prevent);
   --  Add a new child to the container.
   --  If the child's position is set, it is then interpreted as relative to
   --  Self. If the position is not specified, it will be computed
   --  automatically based on the container's policy (either below the previous
   --  child, or to its right).
   --
   --  When Pack_End is true, the child will be added at the end of the
   --  parent's area (right or bottom depending on orientation). If the
   --  parent's size is larger than that needed by all its children, there
   --  will thus be an empty space between children with Pack_End=>False and
   --  children with Pack_End => True.
   --
   --  When Pack_End is True, the children are put in reverse order starting
   --  from the end of Self: for a vertical layout, for instance, the first
   --  pack_end child will appear at the bottom of Self.
   --
   --  Margin are added to each size of the child. The child's width, as set
   --  via Set_Size, does not include the margins.
   --
   --  A floating child does not participate in the stacking: it will still be
   --  displayed below or to the right of the previous child, but the next
   --  item will then be displayed at the same coordinate as the floating
   --  child.

   procedure Clear
      (Self     : not null access Container_Item_Record;
       In_Model : not null access Canvas_Model_Record'Class);
   --  Remove all children of Self

   procedure Set_Width_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size);

   procedure Set_Height_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size);
   --  Specify a minimal and maximal size for the item, along each axis.
   --  The default is for items to occupy the full width of their parent
   --  (in vertical layout) or the full height (in horizontal layout),
   --  and the child required by their children for the other axis.
   --  Calling this procedure overrides any specific size set via
   --  Set_Size or one of the constructors for the items, like rectangles
   --  and ellipsis, for that axis.

   procedure Set_Size
      (Self : not null access Container_Item_Record;
       Width, Height : Size := Auto_Size);
   --  Force a specific size for the item if any of the dimensions is positive.
   --  When Auto_Size is given, the size along that axis will be computed
   --  automatically.
   --  Calling this procedure cancels effects from Set_Size_Range.
   --  The size of a container is influenced by its children as follows:
   --     * the preferred size for each child is computed, based on its own
   --       intrinsic needs (given size for rectangles, text size,...)
   --     * if the child has a min and max size given in pixels, these
   --       constraints are applied immediately.
   --     * the container will then use the maximal computed size amongst
   --       its children.
   --     * Once the size of the container is known, the size for its
   --       children is recomputed when the size or the size constraints
   --       were given as percent of the parent size. It means that sizees
   --       given in percent do not influence the parent's size computation.

   procedure Size_Request
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context);
   --  Compute the ideal size for Self.
   --  It might be either a size specifically forced by the user, or computed
   --  from Self's children's own size_request.
   --  The size is stored internally in the object.
   --  The requested size must not include the margins that were defined in
   --  Add_Child.
   --  Self can modify its computed position (i.e. the position within its
   --  parent) as part of the size computation in this procedure.
   --  One example of overridding this procedure is when you are building an
   --  item which shoud align some text on two columns (for instance in a UML
   --  diagram we might want the field names and their types to each be on
   --  their own column. In this case, the container's Size_Request would
   --  first call the inherited version (so that each child requests a size),
   --  then iterate over the children in each column and compute the maximum
   --  requested width for that column. Finally, another pass for the children
   --  in each column to call Set_Size_Request and override their requested
   --  width.

   procedure Set_Size_Request
     (Self    : not null access Container_Item_Record;
      Width, Height : Gdouble := -1.0);
   --  This procedure should only be called from an override of Size_Request
   --  (but it can then be called for any item, not just the one passed in
   --  parameter).
   --  It can be used to request a specific size for an item, or override the
   --  size already computed. When Width or Height is negative, they do not
   --  override the existing size request.

   procedure Size_Allocate
     (Self  : not null access Container_Item_Record);
   --  Called once the size of the parent object has been decided (i.e. after
   --  all the calls to Size_Request).
   --  The parent must set its child's position and size, and then call
   --  Size_Allocate to let it know about the final size and position.
   --  This can be used to compute attributes that need the actual size of the
   --  item (gradients, centering or right-aligning objects,...)
   --  Alignments and margins are automatically handled by the parent.

   procedure For_Each_Child
     (Self     : not null access Container_Item_Record'Class;
      Callback : not null access procedure
        (Child : not null access Container_Item_Record'Class);
      Recursive : Boolean := False);
   --  Traverse all children of Self, and calls Callback for each.

   procedure Draw_Children
     (Self    : not null access Container_Item_Record'Class;
      Context : Draw_Context);
   --  Display all the children of Self

   procedure Set_Style
     (Self  : not null access Container_Item_Record;
      Style : Drawing_Style);

   function Get_Style
     (Self : not null access Container_Item_Record) return Drawing_Style;
   --  Return the style used for the drawingo of this item.
   --  When changing the style, you must force a refresh of the canvas.

   overriding procedure Refresh_Layout
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context);

   overriding procedure Set_Position
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style_Pkg.Point);

   procedure Set_Position
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style_Pkg.Point := (Gdouble'First, Gdouble'First);
      Anchor_X : Percent;
      Anchor_Y : Percent);
   --  Anchor_X and Anchor_Y indicate which part of the item is at the given
   --  coordinates. For instance, (0, 0) indicates that Pos is the location of
   --  the top-left corner of the item, but (0.5, 0.5) indicates that Pos is
   --  the position of the center of the item.

   overriding procedure Destroy
     (Self     : not null access Container_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   overriding function Position
     (Self : not null access Container_Item_Record) return Gtkada.Style_Pkg.Point;

   overriding function Parent
     (Self : not null access Container_Item_Record)
      return Abstract_Item;

   overriding function Bounding_Box
     (Self : not null access Container_Item_Record)
      return Item_Rectangle;

   overriding function Inner_Most_Item
     (Self     : not null access Container_Item_Record;
      At_Point : Model_Point;
      Context  : Draw_Context) return Abstract_Item;

   overriding function Is_Invisible
     (Self : not null access Container_Item_Record)
      return Boolean;

   function Get_Width (Self : not null access Container_Item_Record)
                       return Model_Coordinate;

   function Get_Height (Self : not null access Container_Item_Record)
                        return Model_Coordinate;

   function Get_Computed_Position (Self : not null access Container_Item_Record)
                                   return Gtkada.Style_Pkg.Point;

   procedure Store_Computed_Position (Self : not null access Container_Item_Record;
                                      CP   : Gtkada.Style_Pkg.Point);

   procedure Resize_Fill_Pattern
     (Self : not null access Container_Item_Record'Class);
   --  Resize the fill pattern so that it extends to the whole item, instead of
   --  just the 0.0 .. 1.0 pattern space.

   function The_Children (Self : not null access Container_Item_Record)
                          return Items_Lists.List;

private

   type Container_Item_Record is abstract new Canvas_Item_Record with record
      Width, Height : Model_Coordinate;
      --  Computed by Size_Request. Always expressed in pixels.
      --  These do not include the margins.

      Computed_Position : Gtkada.Style_Pkg.Point := No_Position;
      --  The position within the parent, as computed in Size_Allocate.
      --  The field Position is used for the position specifically requested by
      --  the user.
      --  This is always the position of the top-left corner, no matter what
      --  Anchor_X and Anchor_Y are set to.

      Anchor_X : Percent := 0.0;
      Anchor_Y : Percent := 0.0;
      --  The position within the item that Self.Position points to. This
      --  is only relevant when an explicit position was given by the user.

      Margin : Margins := No_Margins;
      --  Margins around the child

      Parent : Container_Item;
      --  The parent item

      Min_Width, Min_Height : Size := (Unit_Pixels, 1.0);
      Max_Width, Max_Height : Size := Fit_Size;
      --  Size constraints for the child. If Max_* if Fixed_Size, then the
      --  child is constrained to have Min_* has a specific size.

      Pack_End : Boolean := False;
      Layout   : Child_Layout_Strategy := Vertical_Stack;
      Align    : Alignment_Style := Align_Start;
      Float    : Boolean := False;
      Overflow : Overflow_Style := Overflow_Prevent;

      Style    : Gtkada.Style_Pkg.Drawing_Style;

      Children : Items_Lists.List;
   end record;

end Gtkada.Canvas_Container_Item_Pkg;
