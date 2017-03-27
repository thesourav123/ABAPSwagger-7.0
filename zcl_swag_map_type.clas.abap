CLASS zcl_swag_map_type DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_typedescr
      IMPORTING
        !is_parm            TYPE seosubcodf
      RETURNING
        VALUE(ro_typedescr) TYPE REF TO cl_abap_typedescr .
    METHODS map
      RETURNING
        VALUE(rv_type) TYPE string .
    METHODS constructor
      IMPORTING
        !is_param  TYPE seosubcodf
        !iv_schema TYPE abap_bool DEFAULT abap_true .
  PROTECTED SECTION.

    DATA mv_schema TYPE abap_bool .
    DATA ms_param TYPE seosubcodf .

    METHODS map_element
      IMPORTING
        !io_typedescr  TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(rv_type) TYPE string .
    METHODS map_internal
      IMPORTING
        !io_typedescr  TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(rv_type) TYPE string .
    METHODS map_structure
      IMPORTING
        !io_typedescr  TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(rv_type) TYPE string .
    METHODS map_table
      IMPORTING
        !io_typedescr  TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(rv_type) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_MAP_TYPE IMPLEMENTATION.


  METHOD constructor.

    ms_param  = is_param.
    mv_schema = iv_schema.

  ENDMETHOD.


  METHOD get_typedescr.

    DATA: lv_name TYPE string.


    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = is_parm-type
      RECEIVING
        p_descr_ref    = ro_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
* try looking in the class
      ASSERT NOT is_parm-clsname IS INITIAL.
      CONCATENATE
        '\CLASS=' is_parm-clsname
        '\TYPE=' is_parm-type
        INTO lv_name.
      ro_typedescr = cl_abap_typedescr=>describe_by_name( lv_name ).
    ENDIF.

  ENDMETHOD.                    "get_typedescr


  METHOD map.
     data: lo_typedescr type ref to cl_abap_typedescr.
    lo_typedescr = get_typedescr( ms_param ) .
    rv_type = map_internal( lo_typedescr ).

  ENDMETHOD.                    "map


  METHOD map_element.

    CASE io_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_string
          OR cl_abap_typedescr=>typekind_char
          OR cl_abap_typedescr=>typekind_date
          OR cl_abap_typedescr=>typekind_time
          OR cl_abap_typedescr=>typekind_num
          OR cl_abap_typedescr=>typekind_hex.
        rv_type = '"type":"string"'.
      WHEN cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int.
        rv_type = '"type":"integer"'.
      WHEN cl_abap_typedescr=>typekind_xstring.
        rv_type = '"type":"string", "format": "binary"'.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.                    "map_element


  METHOD map_internal.

    CASE io_typedescr->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        rv_type = map_element( io_typedescr ).
      WHEN cl_abap_typedescr=>kind_struct.
        rv_type = map_structure( io_typedescr ).
      WHEN cl_abap_typedescr=>kind_table.
        rv_type = map_table( io_typedescr ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.                    "map_internal


  METHOD map_structure.

     data: lv_index      type i,
       lv_type       type string,
       lt_components type cl_abap_structdescr=>component_table,
       lo_struct     type ref to cl_abap_structdescr,
       lt_comp_tab   type cl_abap_structdescr=>component_table.

  field-symbols: <ls_component> like line of lt_components.


  lo_struct ?= io_typedescr.
  lt_components = lo_struct->get_components( ).

* todo, this only works with 1 level
  loop at lt_components assigning <ls_component> where as_include = abap_true.
    clear: lt_comp_tab[].
    lo_struct ?= <ls_component>-type.
    lt_comp_tab = lo_struct->get_components( ).
    append lines of lt_comp_tab to lt_components.
  endloop.
  delete lt_components where as_include = abap_true.

  if mv_schema = abap_true.
    rv_type = '"schema":{"type":"object", "properties":{'.
  else.
    rv_type = '"type":"object", "properties":{'.
  endif.

  loop at lt_components assigning <ls_component>.
    lv_index = sy-tabix.

    assert not <ls_component>-name is initial.

    lv_type = map_internal( <ls_component>-type ).
    "rv_type = rv_type && '"' && <ls_component>-name && '":{ ' && lv_type && ' }'.

    concatenate rv_type '"' <ls_component>-name '":{ ' lv_type ' }' into rv_type.

    if lv_index <> lines( lt_components ).
      concatenate rv_type ',' into rv_type.
      "rv_type = rv_type && ','.
    endif.
  endloop.

  concatenate rv_type '}' into rv_type.
  "rv_type = rv_type && '}'.

  if mv_schema = abap_true.
    concatenate rv_type '}' into rv_type.
    "rv_type = rv_type && '}'.
  endif.
 ENDMETHOD.   

  METHOD map_table.

   data: lv_type  type string,
          lo_table type ref to cl_abap_tabledescr,
          lo_line type ref to cl_abap_datadescr.


  lo_table ?= io_typedescr.
  lo_line = lo_table->get_table_line_type( ).
  lv_type = map_internal( lo_line ).

  if mv_schema = abap_true.
    "rv_type = '"schema":{"type":"array", "items":{' && lv_type && '}}'.
    concatenate '"schema":{"type":"array", "items":{' lv_type '}}' into rv_type.
  else.
    "rv_type = '"type":"array", "items":{' && lv_type && '}'.
    concatenate '"type":"array", "items":{' lv_type '}' into rv_type.
  endif.

  ENDMETHOD.                    "map_table
ENDCLASS.
