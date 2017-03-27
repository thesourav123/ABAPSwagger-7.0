CLASS zcl_swag_spec DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_title       TYPE clike
        !iv_description TYPE clike
        !it_meta        TYPE zcl_swag=>ty_meta_internal_tt
        !iv_base        TYPE clike .
    METHODS generate
      RETURNING
        VALUE(rv_spec) TYPE string .
  PROTECTED SECTION.

    DATA mv_title TYPE string .
    DATA mv_description TYPE string .
    DATA mt_meta TYPE zcl_swag=>ty_meta_internal_tt .
    DATA mv_base TYPE string .
    DATA mt_definitions TYPE string_table .

    METHODS definitions
      RETURNING
        VALUE(rv_defs) TYPE string .
    METHODS path
      IMPORTING
        !is_meta       TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS parameters
      IMPORTING
        !is_meta             TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_parameters) TYPE string .
    METHODS response
      IMPORTING
        !is_meta           TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_response) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_SPEC IMPLEMENTATION.


  METHOD constructor.

    mv_title       = iv_title.
    mv_description = iv_description.
    mt_meta        = it_meta.
    mv_base        = iv_base.

  ENDMETHOD.


  METHOD definitions.

     data: lv_string type string,
          lv_sep    type string,
          lt_string type standard table of string with default key.


    append '  "definitions":{' to lt_string.

    lv_sep = ',\n'."|,\n|.
    concatenate lines of mt_definitions into lv_string separated by lv_sep.
    append lv_string to lt_string.

    append '  }' to lt_string.

    concatenate lines of lt_string into rv_defs
      separated by cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD generate.

    define _add.
      concatenate rv_spec &1 cl_abap_char_utilities=>newline
        into rv_spec.
    end-of-definition.

     types: begin of ty_path,
             path type string,
             meta like mt_meta,
           end of ty_path.

    data: lv_index     type i,
          lt_paths     type table of ty_path,
          lv_last_path type abap_bool,
          lv_last_meta type abap_bool,
          lv_path      type string,
          lv_add       type string,
          lv_method    type string.

    field-symbols: <ls_path> like line of lt_paths,
                   <ls_meta> like line of mt_meta.


* handle path with multiple handlers(ie. GET and POST)
    loop at mt_meta assigning <ls_meta>.
      lv_path = path( <ls_meta> ).
      read table lt_paths assigning <ls_path> with key path = lv_path.
      if sy-subrc <> 0.
        append initial line to lt_paths assigning <ls_path>.
        <ls_path>-path = lv_path.
      endif.
      append <ls_meta> to <ls_path>-meta.
    endloop.

    _add '{'.
    _add '  "swagger":"2.0",'.
    _add '  "info":{  '.
    _add '    "version":"1.0.0",'.

    clear: lv_add.
   " lv_add = |    "title":"{ mv_title }",|.
    concatenate '    "title":"' mv_title '",' into lv_add.
    _add lv_add.

    clear: lv_add.
    "lv_add = |    "description":"{ mv_description }"|.
    concatenate '"description":"' mv_description '"' into lv_add.
    _add lv_add.
    clear: lv_add.

    _add '  },'.

    "lv_add = |  "basePath":"{ mv_base }",|.
    concatenate '  "basePath":"' mv_base '",' into lv_add.
    _add lv_add.
     clear: lv_add.

    _add '  "schemes":['.
    _add '    "http"'.
    _add '  ],'.
    _add '  "consumes":['.
    _add '    "application/json"'.
    _add '  ],'.
    _add '  "produces":['.
    _add '    "application/json"'.
    _add '  ],'.
    _add '  "paths":{'.

    loop at lt_paths assigning <ls_path>.
      if sy-tabix = lines( lt_paths ).
        lv_last_path = 'X'.
      else.
        lv_last_path = ''.
      endif.
      "lv_last_path = boolc( sy-tabix = lines( lt_paths ) ).

      "lv_add = |    "{ <ls_path>-path }":\{|.
      concatenate '    "' <ls_path>-path '":{' into lv_add.
      _add lv_add.
       clear: lv_add.

      loop at <ls_path>-meta assigning <ls_meta>.
        clear: lv_method.

        "lv_last_meta = boolc( sy-tabix = lines( <ls_path>-meta ) ).
        if sy-tabix = lines( <ls_path>-meta ).
          lv_last_meta = 'X'.
        else.
          lv_last_meta = ''.
        endif.

        lv_method = <ls_meta>-meta-method.
        translate lv_method to lower case.

        "lv_add = |      "{ to_lower( <ls_meta>-meta-method ) }":\{|.
        concatenate '      "' lv_method '":{' into lv_add.
        _add lv_add.
        clear: lv_add.

        "lv_add = |        "summary":"{ <ls_meta>-meta-summary }",|.
        concatenate '        "summary":"' <ls_meta>-meta-summary '",' into lv_add.
        _add lv_add.
        clear: lv_add.

        "lv_add = |        "description":"",|.
        lv_add = '        "description":"",'.
        _add lv_add.
        clear lv_add.

        lv_add = parameters( <ls_meta> ).
        _add lv_add.
        clear: lv_add.

        _add '        "produces":['.

        read table <ls_meta>-parameters with key
          pardecltyp = zcl_swag=>c_parm_kind-returning
          type = 'STRING' transporting no fields.
        if sy-subrc = 0.
          _add '"text/plain"'.
        else.
          _add '"application/json"'.
        endif.

        _add '        ],'.
        _add '        "responses":{'.

        lv_add = response( <ls_meta> ).
        _add lv_add.

        _add '          "500":{'.
        _add '            "description":"error"'.
        _add '          }'.
        _add '        }'.
        _add '      }'.

        if lv_last_meta = abap_false.
          _add ','.
        endif.
      endloop.

      _add '    }'.
      if lv_last_path = abap_false.
        _add ','.
      endif.
    endloop.

    _add '  },'.

    lv_add = definitions( ).
    _add lv_add.

    _add '}'.

  ENDMETHOD.


  METHOD parameters.

    data: lt_string type table of string,
          ls_string like line of lt_string,
          lv_type   type string,
          lo_map    type ref to zcl_swag_map_type,
          lv_count  type i.

    field-symbols: <ls_parameter> like line of is_meta-parameters.


    append '"parameters":[' to lt_string.

    loop at is_meta-parameters assigning <ls_parameter>
        where pardecltyp = zcl_swag=>c_parm_kind-importing.

      append '{' to lt_string.

      concatenate '"name":"' <ls_parameter>-sconame '",' into ls_string.
      append ls_string to lt_string.

      read table is_meta-meta-url-group_names from <ls_parameter>-sconame
        transporting no fields.
      if sy-subrc = 0.
        append '"in":"path",' to lt_string.
      elseif is_meta-meta-method = zcl_swag=>c_method-get.
        append '"in":"query",' to lt_string.
      else.
        append '"in":"body",' to lt_string.
      endif.

      append '"description":"",' to lt_string.

      create object lo_map
        exporting
          is_param = <ls_parameter>.
      lv_type = lo_map->map( ).
      concatenate lv_type ',' into ls_string.
      append ls_string to lt_string.

      append '"required":true'   to lt_string.
      append '},' to lt_string.
    endloop.
    if sy-subrc = 0.
      lv_count = lines( lt_string ).
* fix the comma
      "DELETE lt_string INDEX lines( lt_string ).
       delete lt_string index lv_count.
      append '}' to lt_string.
    endif.

    append '],' to lt_string.

    concatenate lines of lt_string into rv_parameters
      separated by cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD path.

    DATA: lv_name    TYPE string,
          lv_offset1 TYPE i,
          lv_offset2 TYPE i.


    rv_path = is_meta-meta-url-regex.

    REPLACE ALL OCCURRENCES OF '$' IN rv_path WITH ''.

* replace the regex groups like (\w*) with swagger identifies {IV_NAME}
    LOOP AT is_meta-meta-url-group_names INTO lv_name.
      FIND FIRST OCCURRENCE OF '(' IN rv_path MATCH OFFSET lv_offset1.
      FIND FIRST OCCURRENCE OF ')' IN rv_path MATCH OFFSET lv_offset2.
      lv_offset2 = lv_offset2 + 1.
      CONCATENATE rv_path(lv_offset1) '{' lv_name '}' rv_path+lv_offset2 INTO rv_path.
    ENDLOOP.

  ENDMETHOD.


  METHOD response.

   data: lt_string type table of string,
          lv_type   type string,
          lv_string type string,
          lo_map    type ref to zcl_swag_map_type,
          lv_temp_str type string.

    field-symbols: <ls_parameter> like line of is_meta-parameters.


    append '"200": {' to lt_string.

    read table is_meta-parameters with key pardecltyp = zcl_swag=>c_parm_kind-returning
      transporting no fields.
    if sy-subrc = 0.
      append '  "description": "successful operation",' to lt_string.
    else.
      append '  "description": "successful operation"' to lt_string.
    endif.

    loop at is_meta-parameters assigning <ls_parameter>
        where pardecltyp = zcl_swag=>c_parm_kind-returning.
      create object lo_map
        exporting
          is_param  = <ls_parameter>
          iv_schema = abap_false.
      lv_type = lo_map->map( ).

      append '"schema": {' to lt_string.

      "APPEND |"$ref": "#\\/definitions\\/{ is_meta-meta-handler }_Response"| TO lt_string.
      concatenate '"$ref": "#\/definitions\/' is_meta-meta-handler '_Response"' into lv_temp_str.
      append lv_temp_str to lt_string.


      append '}' to lt_string.


*      APPEND |"{ is_meta-meta-handler }_Response":\{"type": "object","properties": \{"DATA": \{{ lv_type }\}\}\}|
*        TO mt_definitions.


      clear: lv_temp_str.
      concatenate '"' is_meta-meta-handler '_Response":{"type": "object","properties": {"DATA": {' lv_type '}}}' into lv_temp_str.
      append lv_temp_str to mt_definitions.
    endloop.

    append '},' to lt_string.

    concatenate lines of lt_string into rv_response
      separated by cl_abap_char_utilities=>newline.

  ENDMETHOD.
ENDCLASS.
