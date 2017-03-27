CLASS zcl_swag DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_parm_kind,
        importing TYPE seopardecl VALUE '0',
        exporting TYPE seopardecl VALUE '1',
        changing  TYPE seopardecl VALUE '2',
        returning TYPE seopardecl VALUE '3',
      END OF c_parm_kind .

    TYPES:
      ty_parameters_tt TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_url,
        regex       TYPE string,
        group_names TYPE STANDARD TABLE OF seosconame WITH DEFAULT KEY,
      END OF ty_url .
    TYPES:
      BEGIN OF ty_meta,
        summary TYPE string,
        url     TYPE ty_url,
        method  TYPE string,
        handler TYPE string,
      END OF ty_meta .
    TYPES:
      BEGIN OF ty_meta_internal,
        meta       TYPE ty_meta,
        obj        TYPE REF TO object,
        parameters TYPE ty_parameters_tt,
        classname  TYPE seoclsname,
      END OF ty_meta_internal .
    TYPES:
      ty_meta_internal_tt TYPE STANDARD TABLE OF ty_meta_internal WITH DEFAULT KEY .
    TYPES:
      ty_meta_tt TYPE STANDARD TABLE OF ty_meta WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF c_method,
        get    TYPE string VALUE 'GET',
        post   TYPE string VALUE 'POST',
        put    TYPE string VALUE 'PUT',
        delete TYPE string VALUE 'DELETE',
      END OF c_method .

    METHODS constructor
      IMPORTING
        !ii_server       TYPE REF TO if_http_server
        !iv_base         TYPE string
        !iv_swagger_json TYPE string DEFAULT '/swagger.json'
        !iv_swagger_html TYPE string DEFAULT '/swagger.html'
        !iv_title        TYPE string .
    METHODS register
      IMPORTING
        !ii_handler TYPE REF TO zif_swag_handler .
    METHODS run
      RAISING
        cx_static_check .
  PROTECTED SECTION.

    DATA mv_base TYPE string .
    DATA mi_server TYPE REF TO if_http_server .
    DATA mt_meta TYPE ty_meta_internal_tt .
    DATA mv_swagger_json TYPE string .
    DATA mv_swagger_html TYPE string .
    DATA mv_title TYPE string .

    METHODS build_parameters
      IMPORTING
        !is_meta             TYPE ty_meta_internal
      RETURNING
        VALUE(rt_parameters) TYPE abap_parmbind_tab .
    METHODS create_data
      IMPORTING
        !is_meta       TYPE ty_meta_internal
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
    METHODS download
      IMPORTING
        !iv_file TYPE string .
    METHODS from_input
      IMPORTING
        !is_meta TYPE ty_meta_internal
        !ir_ref  TYPE REF TO data .
    METHODS from_path
      IMPORTING
        !is_meta TYPE ty_meta_internal
        !ir_ref  TYPE REF TO data .
    METHODS generate_spec
      IMPORTING
        !iv_title       TYPE clike
        !iv_description TYPE clike .
    METHODS generate_ui
      IMPORTING
        !iv_json_url TYPE string
        !iv_dist     TYPE string DEFAULT ''
        !iv_title    TYPE clike DEFAULT ''
      RETURNING
        VALUE(rv_ui) TYPE string .
    METHODS json_reply
      IMPORTING
        !is_meta       TYPE ty_meta_internal
        !it_parameters TYPE abap_parmbind_tab .
    METHODS text_reply
      IMPORTING
        !is_meta       TYPE ty_meta_internal
        !it_parameters TYPE abap_parmbind_tab .
    METHODS validate_parameters
      IMPORTING
        !it_parameters TYPE ty_parameters_tt .
ENDCLASS.



CLASS ZCL_SWAG IMPLEMENTATION.


  METHOD build_parameters.

    DATA: ls_parameter LIKE LINE OF rt_parameters,
          lr_dref      TYPE REF TO data.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters,
                   <lg_comp>      TYPE any,
                   <lg_struc>     TYPE any.


    lr_dref = create_data( is_meta ).
    ASSIGN lr_dref->* TO <lg_struc>.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>.
      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.
      ls_parameter-name  = <ls_parameter>-sconame.
      GET REFERENCE OF <lg_comp> INTO ls_parameter-value.
      INSERT ls_parameter INTO TABLE rt_parameters.
    ENDLOOP.

    from_path( is_meta = is_meta
               ir_ref  = lr_dref ).

    from_input( is_meta = is_meta
                ir_ref  = lr_dref ).

  ENDMETHOD.


  METHOD constructor.

    mi_server       = ii_server.
    mv_base         = iv_base.
    mv_swagger_json = iv_swagger_json.
    mv_swagger_html = iv_swagger_html.
    mv_title        = iv_title.

  ENDMETHOD.


  METHOD create_data.

     data: lo_struct     type ref to cl_abap_structdescr,
          lt_components type cl_abap_structdescr=>component_table,
          lo_typedescr  type ref to cl_abap_typedescr.

    field-symbols: <ls_component> like line of lt_components,
                   <ls_parameter> like line of is_meta-parameters.


    loop at is_meta-parameters assigning <ls_parameter>.
      append initial line to lt_components assigning <ls_component>.
      <ls_component>-name = <ls_parameter>-sconame.

      lo_typedescr = zcl_swag_map_type=>get_typedescr( <ls_parameter> ).

      <ls_component>-type ?= lo_typedescr.
    endloop.

    lo_struct = cl_abap_structdescr=>create( lt_components ).

    create data rr_data type handle lo_struct.

  ENDMETHOD.


  METHOD download.
* this is a workaround to avoid CORS problems

* todo, possibility to define url
* todo, proxy settings

    DATA: li_client   TYPE REF TO if_http_client,
          lv_response TYPE string.


    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'http://petstore.swagger.io/' && iv_file
        ssl_id             = 'ANONYM'
*        proxy_host         = lo_settings->get_proxy_url( )
*        proxy_service      = lo_settings->get_proxy_port( )
      IMPORTING
        client             = li_client ).

    li_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).

    li_client->receive( ).

    lv_response = li_client->response->get_cdata( ).

    REPLACE FIRST OCCURRENCE
      OF '(t=t.requestInterceptor(t)||t),'
      IN lv_response
      WITH '(t=t.requestInterceptor(t)||t),t.credentials="same-origin",'.

    mi_server->response->set_cdata( lv_response ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  ENDMETHOD.


  METHOD from_input.

     data: lv_cdata  type string,
          lo_writer type ref to cl_sxml_string_writer,
          lv_json   type xstring.

    field-symbols: <ls_parameter> like line of is_meta-parameters,
                   <lg_comp>      type any,
                   <lg_struc>     type any.


    assign ir_ref->* to <lg_struc>.

    loop at is_meta-parameters assigning <ls_parameter>
        where pardecltyp = c_parm_kind-importing.
      read table is_meta-meta-url-group_names from <ls_parameter>-sconame
        transporting no fields.
      if sy-subrc = 0.
* ignore parameters that are part of url
        continue.
      endif.

      assign component <ls_parameter>-sconame of structure <lg_struc> to <lg_comp>.
      assert sy-subrc = 0.

      if <ls_parameter>-type = 'STRING'.
        <lg_comp> = mi_server->request->get_cdata( ).
      elseif <ls_parameter>-type = 'XSTRING'.
        <lg_comp> = mi_server->request->get_data( ).
      else.
        lv_cdata = mi_server->request->get_cdata( ).

        "lv_cdata = '{"DATA":' && lv_cdata && '}'.
        concatenate '{"DATA":' lv_cdata '}' into lv_cdata.

        lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

        call transformation zdemo_json_xml_to_upper
          source xml lv_cdata
          result xml lo_writer.

        lv_json = lo_writer->get_output( ).

        call transformation id
          source xml lv_json
          result data = <lg_comp>.
      endif.

* multiple body input parameters not allowed
* todo, this should be validated earlier
      return.

    endloop.

  ENDMETHOD.


  METHOD from_path.

    DEFINE _store.
      READ TABLE is_meta-meta-url-group_names INDEX &1 INTO lv_component.
      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_component OF STRUCTURE <lg_struc> TO <lg_comp>.
        ASSERT sy-subrc = 0.
        <lg_comp> = lv_match&1.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lv_path      TYPE string,
          lv_component TYPE string,
          lv_match1    TYPE string,
          lv_match2    TYPE string,
          lv_match3    TYPE string,
          lv_match4    TYPE string,
          lv_match5    TYPE string.

    FIELD-SYMBOLS: <lg_comp>  TYPE any,
                   <lg_struc> TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).

    FIND REGEX is_meta-meta-url-regex IN lv_path
      SUBMATCHES lv_match1 lv_match2 lv_match3 lv_match4 lv_match5.

    _store 1.
    _store 2.
    _store 3.
    _store 4.
    _store 5.

  ENDMETHOD.


  METHOD generate_spec.

    DATA: lv_spec TYPE string,
          lo_spec TYPE REF TO zcl_swag_spec.


    CREATE OBJECT lo_spec
      EXPORTING
        iv_title       = iv_title
        iv_description = iv_description
        it_meta        = mt_meta
        iv_base        = mv_base.

    lv_spec = lo_spec->generate( ).

    mi_server->response->set_cdata( lv_spec ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  ENDMETHOD.


  METHOD generate_ui.
 define _add.
      concatenate rv_ui &1 cl_abap_char_utilities=>newline
        into rv_ui.
    end-of-definition.

    _add '<!DOCTYPE html>'.
    _add '<html>'.
    _add '<head>'.
    _add '<meta charset="UTF-8">'.

    _add '<title>'.
    if iv_title is initial.
      _add 'Swagger UI'.
    else.
      _add iv_title.
    endif.
    _add '</title>'.

* bad, temprary fix, should use read CDN instead of petstore.swagger.io
    _add '<link rel="icon" type="image/png" href="iv_dist/images/favicon-32x32.png" sizes="32x32" />'.
    _add '<link rel="icon" type="image/png" href="iv_dist/images/favicon-16x16.png" sizes="16x16" />'.
    _add '<link href="iv_dist/js/swagger-ui.css" media="screen" rel="stylesheet" type="text/css"/>'.
    _add '<script src="iv_base/js/swagger-ui-bundle.js" type="text/javascript"></script>'.
    _add '<script src="iv_base/js/swagger-ui-standalone-preset.js" type="text/javascript"></script>'.
    _add '</head>'.
    _add '<body>'.
    _add '<div id="swagger-ui"></div>'.
    _add '<script type="text/javascript">'.
    _add 'window.onload = function() {'.
    _add 'const ui = SwaggerUIBundle({'.
    _add 'url: "swagger.json",'.
    _add 'validatorUrl : null,'.
    _add 'dom_id: "#swagger-ui",'.
    _add 'presets: ['.
    _add 'SwaggerUIBundle.presets.apis,'.
    _add 'Array.isArray(SwaggerUIStandalonePreset) ? SwaggerUIStandalonePreset : SwaggerUIStandalonePreset.default'.
    _add '],'.
    _add 'plugins: ['.
    _add 'SwaggerUIBundle.plugins.DownloadUrl'.
    _add '],'.
    _add 'layout: "StandaloneLayout"'.
    _add '})'.
    _add 'window.ui = ui'.
    _add '}'.
    _add '</script>'.
    _add '</body>'.
    _add '</html>'.

    if iv_dist is initial.
      replace all occurrences of 'iv_dist'
        in rv_ui with 'http://petstore.swagger.io'.
    else.
      replace all occurrences of 'iv_dist'
        in rv_ui with iv_dist.
    endif.

    replace all occurrences of 'iv_base'
      in rv_ui with mv_base.



    replace first occurrence of 'swagger.json'
      in rv_ui with iv_json_url.

    mi_server->response->set_cdata( rv_ui ).
    mi_server->response->set_status( code = 200 reason = '200' ).


  ENDMETHOD.


  METHOD json_reply.

    DATA: lv_data   TYPE xstring,
          lo_writer TYPE REF TO cl_sxml_string_writer.

    FIELD-SYMBOLS: <ls_meta>      LIKE LINE OF is_meta-parameters,
                   <ls_parameter> LIKE LINE OF it_parameters,
                   <lg_struc>     TYPE any.


    READ TABLE is_meta-parameters ASSIGNING <ls_meta>
      WITH KEY pardecltyp = c_parm_kind-returning.
    IF sy-subrc  = 0.
      READ TABLE it_parameters ASSIGNING <ls_parameter>
        WITH KEY name = <ls_meta>-sconame.
      ASSERT sy-subrc = 0.

      lo_writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).
      ASSIGN <ls_parameter>-value->* TO <lg_struc>.
      CALL TRANSFORMATION id
        SOURCE data = <lg_struc>
        RESULT XML lo_writer.
      lv_data = lo_writer->get_output( ).

    ENDIF.

    mi_server->response->set_data( lv_data ).

  ENDMETHOD.


  METHOD register.

    DATA: ls_meta LIKE LINE OF mt_meta,
          lt_meta TYPE ty_meta_tt,
          lo_obj  TYPE REF TO cl_abap_objectdescr.


    lt_meta = ii_handler->meta( ).
    LOOP AT lt_meta INTO ls_meta-meta.
      ls_meta-obj = ii_handler.

      lo_obj ?= cl_abap_objectdescr=>describe_by_object_ref( ii_handler ).

      READ TABLE lo_obj->methods
        WITH KEY name = ls_meta-meta-handler
        visibility = cl_abap_objectdescr=>public
        TRANSPORTING NO FIELDS.
* method must exist and be public
      ASSERT sy-subrc = 0.

      ls_meta-classname = lo_obj->absolute_name+7.

      SELECT * FROM seosubcodf
        INTO TABLE ls_meta-parameters
        WHERE clsname = ls_meta-classname
        AND cmpname = ls_meta-meta-handler
        AND sconame NOT LIKE 'ZCX_%'
        ORDER BY PRIMARY KEY.                             "#EC CI_SUBRC
      ASSERT sy-subrc = 0.

      validate_parameters( ls_meta-parameters ).

      APPEND ls_meta TO mt_meta.

      CLEAR ls_meta.
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

 data: lv_path       type string,
          lv_method     type string,
          lt_parameters type abap_parmbind_tab,
          lv_dummy_html_path type string,
          lv_dummy_json_path type string,
          lv_dummy_bundle_path type string,
          lv_dummy_bundle_map_path type string,
          lv_dummy_preset_path type string,
          lv_dummy_preset_map_path type string,
          lv_dummy_title_path type string.

    field-symbols: <ls_meta> like line of mt_meta.


    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).
    lv_method = mi_server->request->get_method( ).
    concatenate mv_base mv_swagger_html into lv_dummy_html_path.
    concatenate mv_base mv_swagger_json into lv_dummy_json_path.
    concatenate mv_base '/js/swagger-ui-bundle.js' into lv_dummy_bundle_path.
    concatenate mv_base '/js/swagger-ui-standalone-preset.js' into lv_dummy_preset_path.
    concatenate mv_base '/js/swagger-ui-standalone-preset.js.map' into lv_dummy_preset_map_path.
    concatenate mv_base '/js/swagger-ui-bundle.js.map' into lv_dummy_bundle_map_path.
    lv_dummy_json_path = 'https://raw.githubusercontent.com/novastorm123/abapswaggerNW7.0/master/swagger_70.json'.
    if lv_path = lv_dummy_html_path."mv_base && mv_swagger_html
       concatenate mv_title ' - Swagger' into lv_dummy_title_path.
      generate_ui(
        iv_json_url = lv_dummy_json_path "mv_base && mv_swagger_json
        iv_title    = lv_dummy_title_path )."mv_title && ' - Swagger' ).
      return.
    elseif lv_path = lv_dummy_json_path."mv_base && mv_swagger_json.
       concatenate mv_title ' REST functions' into lv_dummy_title_path.
      generate_spec(
        iv_title       = mv_title
        iv_description = lv_dummy_title_path )."mv_title && ' REST functions' ).
      return.
    elseif lv_path = lv_dummy_bundle_path."mv_base && '/js/swagger-ui-bundle.js'.
      download( '/js/swagger-ui-bundle.js' ).
      return.
    elseif lv_path = lv_dummy_preset_path."mv_base && '/js/swagger-ui-standalone-preset.js'.
      download( '/js/swagger-ui-standalone-preset.js' ).
      return.
    elseif lv_path = lv_dummy_preset_map_path."mv_base && '/js/swagger-ui-standalone-preset.js'.
      download( '/js/swagger-ui-standalone-preset.js.map' ).
      return.
    elseif lv_path = lv_dummy_bundle_map_path."mv_base && '/js/swagger-ui-standalone-preset.js'.
      download( '/js/swagger-ui-bundle.js.map' ).
      return.
    endif.

    loop at mt_meta assigning <ls_meta>.
      if lv_method <> <ls_meta>-meta-method.
        continue.
      endif.

      find regex <ls_meta>-meta-url-regex in lv_path.
      if sy-subrc = 0.

        lt_parameters = build_parameters( <ls_meta> ).
        call method <ls_meta>-obj->(<ls_meta>-meta-handler)
          parameter-table lt_parameters.

        mi_server->response->set_compression( ).

        loop at <ls_meta>-parameters
            transporting no fields
            where pardecltyp = c_parm_kind-returning
            and ( type = 'STRING' or type = 'XSTRING' ).
          exit.
        endloop.
        if sy-subrc = 0.
          text_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        else.
          json_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        endif.

        mi_server->response->set_status( code = 200 reason = '200' ).
        return.

      endif.
    endloop.

    mi_server->response->set_cdata( '404 swagger' ).
    mi_server->response->set_status( code = 404 reason = '404' ).

  ENDMETHOD.


  METHOD text_reply.

    FIELD-SYMBOLS: <lg_any>       TYPE any,
                   <ls_meta>      LIKE LINE OF is_meta-parameters,
                   <ls_parameter> LIKE LINE OF it_parameters.


    READ TABLE is_meta-parameters ASSIGNING <ls_meta>
      WITH KEY pardecltyp = c_parm_kind-returning.
    IF sy-subrc  = 0.
      READ TABLE it_parameters ASSIGNING <ls_parameter>
        WITH KEY name = <ls_meta>-sconame.
      ASSERT sy-subrc = 0.

      ASSIGN <ls_parameter>-value->* TO <lg_any>.

      CASE <ls_meta>-type.
        WHEN 'XSTRING'.
          mi_server->response->set_data( <lg_any> ).
        WHEN 'STRING'.
          mi_server->response->set_cdata( <lg_any> ).
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD validate_parameters.

* no EXPORTING, no CHANGING
    LOOP AT it_parameters TRANSPORTING NO FIELDS
        WHERE pardecltyp = c_parm_kind-exporting
        OR pardecltyp = c_parm_kind-changing.
      ASSERT 0 = 1.
    ENDLOOP.

* no reference types
* todo

* todo, max one importing parameter? apart from path parameters?

  ENDMETHOD.
ENDCLASS.
