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

with Gtkada.Canvas_Defs;                 use Gtkada.Canvas_Defs;
with Gtkada.Canvas_Model_Pkg;            use Gtkada.Canvas_Model_Pkg;
with Gtkada.Canvas_Abstract_Item_Pkg;    use Gtkada.Canvas_Abstract_Item_Pkg;
with Gtkada.Canvas_View_Pkg;             use Gtkada.Canvas_View_Pkg;

package Gtkada.Canvas_List_Model_Pkg is

   type List_Canvas_Model_Record is new Canvas_Model_Record with private;
   type List_Canvas_Model is access all List_Canvas_Model_Record'Class;
   --  A very simple-minded concrete implementation for a model.
   --  This model is suitable for most cases where only a few thousands items
   --  are displayed. If you have tens of thousands, you should consider
   --  wrapping this model with a Gtkada.Canvas_View.Models.Rtree_Model to
   --  speed things up.

   procedure Gtk_New (Self : out List_Canvas_Model);
   --  Create a new model

   procedure Add
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   --  Add a new item to the model.

   procedure Clear
     (Self : not null access List_Canvas_Model_Record);
   --  Remove all items from the model, and destroy them.

   overriding procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);

   overriding procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Set  : Item_Sets.Set);

   overriding procedure For_Each_Item
     (Self     : not null access List_Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      Selected_Only : Boolean := False;
      Filter        : Item_Kind_Filter := Kind_Any;
      In_Area       : Model_Rectangle := No_Rectangle);

   overriding procedure Raise_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);

   overriding procedure Lower_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);

   overriding function Toplevel_Item_At
     (Self    : not null access List_Canvas_Model_Record;
      Point   : Model_Point;
      Context : Draw_Context) return Abstract_Item;

private

   type List_Canvas_Model_Record is new Canvas_Model_Record with record
      Items : Items_Lists.List;
      --  items are sorted: lowest items first (minimal z-layer)
   end record;

end Gtkada.Canvas_List_Model_Pkg;
