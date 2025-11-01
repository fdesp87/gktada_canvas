#!/bin/sh

gprinstall --uninstall gtkada_canvas

gprinstall -Pgtkada_canvas -p -f -XLibrary_Type=relocatable --prefix=/opt/GNAT/15.1.0-2 \
           --build-name=relocatable --build-var=GtkAda_Canvas
gprinstall -Pgtkada_canvas -p -f -XLibrary_Type=static-pic  --prefix=/opt/GNAT/15.1.0-2 \
           --build-name=static-pic  --build-var=GtkAda_Canvas
gprinstall -Pgtkada_canvas -p -f -XLibrary_Type=static      --prefix=/opt/GNAT/15.1.0-2 \
           --build-name=static      --build-var=GtkAda_Canvas
