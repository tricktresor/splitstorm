REPORT zt9r_splitstorm_demo01.

" Aufbau einer Splitter-Hierachie und dynamisches Auslesen der Splitter-Größen

SELECTION-SCREEN PUSHBUTTON /1(20) TEXT-sav USER-COMMAND zsave.
SELECTION-SCREEN PUSHBUTTON /1(20) TEXT-lod USER-COMMAND zload.



INITIALIZATION.

  DATA(docker) = NEW cl_gui_docking_container( ratio = 90 side = cl_gui_docking_container=>dock_at_bottom ).

  DATA(sca)   = NEW cl_gui_splitter_container( parent = docker rows = 2 columns = 2 ).
  DATA(sca1)  = NEW cl_gui_splitter_container( parent = sca->get_container( row = 1 column = 1 ) rows = 1 columns = 2 ).
  DATA(sca2)  = NEW cl_gui_splitter_container( parent = sca->get_container( row = 2 column = 1 ) rows = 2 columns = 1 ).
  DATA(scb1)  = NEW cl_gui_splitter_container( parent = sca2->get_container( row = 1 column = 1 ) rows = 1 columns = 2 ).
  DATA(scb2)  = NEW cl_gui_splitter_container( parent = sca2->get_container( row = 2 column = 1 ) rows = 2 columns = 1 ).


AT SELECTION-SCREEN.

  DATA(info) = NEW zt9r_splitstorm( sy-cprog ).

  CASE sy-ucomm.
    WHEN 'ZSAVE'.
      info->analyze_and_save( docker ).
    WHEN 'ZLOAD'.
      info->load_and_restore( docker ).
  ENDCASE.
