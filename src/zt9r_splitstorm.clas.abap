CLASS zt9r_splitstorm DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_splitter_info,
             path          TYPE string,
             rows          TYPE i,
             columns       TYPE i,
             row_heights   TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
             column_widths TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
           END OF ty_splitter_info.

    TYPES ty_splitter_info_tab TYPE STANDARD TABLE OF ty_splitter_info WITH DEFAULT KEY.

    METHODS delete.

    METHODS load_and_restore
      IMPORTING
        i_container TYPE REF TO cl_gui_container.

    METHODS analyze_and_save
      IMPORTING
        i_container TYPE REF TO cl_gui_container.

    METHODS constructor
      IMPORTING
        repid TYPE clike.

  PRIVATE SECTION.
    METHODS save.
    METHODS load.

    CONSTANTS mc_root TYPE string VALUE `ROOT`.

    METHODS analyze_recursive
      IMPORTING
        i_container   TYPE REF TO cl_gui_control
        i_path        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS get_row_count
      IMPORTING
        i_splitter    TYPE REF TO cl_gui_splitter_container
      RETURNING
        VALUE(result) TYPE i.

    METHODS get_column_count
      IMPORTING
        i_splitter    TYPE REF TO cl_gui_splitter_container
      RETURNING
        VALUE(result) TYPE i.

    METHODS restore_recursive
      IMPORTING
        i_container TYPE REF TO cl_gui_control
        i_path      TYPE string.

    METHODS get_splitter_row_height
      IMPORTING
        io_splitter   TYPE REF TO cl_gui_splitter_container
      RETURNING
        VALUE(r_size) TYPE i.

    METHODS get_splitter_col_width
      IMPORTING
        io_splitter   TYPE REF TO cl_gui_splitter_container
      RETURNING
        VALUE(r_size) TYPE i.

    METHODS analyze
      IMPORTING
        i_container   TYPE REF TO cl_gui_container
      RETURNING
        VALUE(result) TYPE ty_splitter_info_tab.

    METHODS restore_sizes
      IMPORTING
        i_container TYPE REF TO cl_gui_container.

    METHODS get_id
      RETURNING
        VALUE(r_id) TYPE char20.

    DATA size_info TYPE ty_splitter_info_tab.
    DATA repid     TYPE c LENGTH 40.
ENDCLASS.


CLASS zt9r_splitstorm IMPLEMENTATION.
  METHOD constructor.
    me->repid = repid.
  ENDMETHOD.

  METHOD analyze.
    LOOP AT i_container->children INTO DATA(child).
      analyze_recursive( i_container = child
                         i_path      = mc_root ).
    ENDLOOP.

    result = size_info.
  ENDMETHOD.

  METHOD analyze_recursive.
    TRY.
        DATA(lo_splitter) = CAST cl_gui_splitter_container( i_container ).
        result = abap_true.
      CATCH cx_sy_move_cast_error.
        result = abap_false.
        RETURN.
    ENDTRY.

    DATA(lv_rows)    = get_row_count( lo_splitter ).
    DATA(lv_columns) = get_column_count( lo_splitter ).

    DATA lt_row_heights TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA lt_col_widths  TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DO lv_rows TIMES.
      APPEND get_splitter_row_height( lo_splitter ) TO lt_row_heights.
    ENDDO.

    DO lv_columns TIMES.
      APPEND get_splitter_col_width( lo_splitter ) TO lt_col_widths.
    ENDDO.

    APPEND VALUE ty_splitter_info( path          = i_path
                                   rows          = lv_rows
                                   columns       = lv_columns
                                   row_heights   = lt_row_heights
                                   column_widths = lt_col_widths )
           TO size_info.

    DATA(index_row) = 0.
    DATA(index_col) = 0.
    DO lv_rows TIMES.
      index_col = 0.
      index_row = index_row + 1.
      DO lv_columns TIMES.
        index_col = index_col + 1.
        DATA(lo_container) = lo_splitter->get_container( row    = index_row
                                                         column = index_col ).
        LOOP AT lo_container->children INTO DATA(lo_child).
          IF NOT analyze_recursive(
                     i_container = lo_child
                     i_path      = |{ i_path }[{ index_row },{ index_col }]| ).
            EXIT. " from do
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD get_row_count.
    result = 0.
    DO 20 TIMES.
      DATA(container) = i_splitter->get_container( row    = sy-index
                                                   column = 1 ).
      IF container IS INITIAL.
        RETURN.
      ELSE.
        result = result + 1.
      ENDIF.

    ENDDO.
  ENDMETHOD.

  METHOD get_column_count.
    result = 0.
    DO 20 TIMES.
      DATA(container) = i_splitter->get_container( row    = 1
                                                   column = sy-index ).
      IF container IS INITIAL.
        RETURN.
      ELSE.
        result = result + 1.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD save.
    IF size_info IS INITIAL.
      RETURN.
    ENDIF.

    DATA(id) = get_id( ).
    EXPORT splitter_info FROM size_info TO DATABASE indx(z1) ID id.
    MESSAGE 'Splitter configuration saved' TYPE 'S'.
  ENDMETHOD.

  METHOD load.
    DATA(id) = get_id( ).
    IMPORT splitter_info TO size_info FROM DATABASE indx(z1) ID id.
    IF sy-subrc = 0 AND size_info IS NOT INITIAL.
      MESSAGE 'Splitter configuration loaded' TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD restore_sizes.
    IF size_info IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_container->children INTO DATA(child).
      restore_recursive( i_container = child
                         i_path      = mc_root ).
    ENDLOOP.
  ENDMETHOD.

  METHOD restore_recursive.
    TRY.
        DATA(splitter) = CAST cl_gui_splitter_container( i_container ) ##NO_TEXT.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    READ TABLE size_info INTO DATA(info) WITH KEY path = i_path.
    IF sy-subrc <> 0 OR splitter IS INITIAL.
      RETURN.
    ENDIF.

    DATA(idx) = 1.

    LOOP AT info-row_heights INTO DATA(lv_r).
      splitter->set_row_height( id = idx height = lv_r ).
      idx = idx + 1.
    ENDLOOP.

    idx = 1.
    LOOP AT info-column_widths INTO DATA(lv_c).
      splitter->set_column_width( id = idx width = lv_c ).
      idx = idx + 1.
    ENDLOOP.

    " Rekursiv weiter
    DATA(index_row) = 0.
    DATA(index_col) = 0.
    DO info-rows TIMES.
      index_col = 0.
      index_row = index_row + 1.
      DO info-columns TIMES.
        index_col = index_col + 1.
        DATA(lo_container) = splitter->get_container( row    = index_row
                                                      column = index_col ).
        DATA(lv_subpath) = |{ i_path }[{ index_row },{ index_col }]|.
        LOOP AT lo_container->children INTO DATA(lo_child).
          restore_recursive( i_container = lo_child
                             i_path      = lv_subpath ).
        ENDLOOP.
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD get_splitter_row_height.
    io_splitter->get_row_height( EXPORTING
                                   id     = sy-index
                                 IMPORTING
                                   result = r_size  ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD get_splitter_col_width.
    io_splitter->get_column_width( EXPORTING
                                     id     = sy-index
                                   IMPORTING
                                     result = r_size  ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD analyze_and_save.
    analyze( i_container = i_container ).
    save( ).
  ENDMETHOD.

  METHOD load_and_restore.
    load( ).
    restore_sizes( i_container = i_container ).
  ENDMETHOD.

  METHOD delete.
    DATA(id) = get_id( ).
    DELETE FROM DATABASE indx(z1) ID id.
    IF sy-subrc = 0 AND size_info IS NOT INITIAL.
      MESSAGE 'Splitter configuration deleted' TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD get_id.
    r_id = |SPLITTER-{ repid }-{ sy-uname }|.
  ENDMETHOD.
ENDCLASS.
