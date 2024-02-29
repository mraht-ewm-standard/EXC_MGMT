CLASS zcx_root DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED
  GLOBAL FRIENDS zcx_static_check
                 zcx_no_check.

  PUBLIC SECTION.
    INTERFACES if_message.

    CONSTANTS: BEGIN OF mc_log_enabled,
                 undef TYPE boolean VALUE '',
                 true  TYPE boolean VALUE 'X',
                 false TYPE boolean VALUE '-',
               END OF mc_log_enabled.

    CONSTANTS: BEGIN OF mc_obj_id,
                 generic TYPE objectname VALUE 'OBJECT',
               END OF mc_obj_id.

    METHODS constructor
      IMPORTING io_exception  TYPE REF TO zcx_if_check_class
                is_t100key    TYPE scx_t100key
                iv_obj_id     TYPE objectname
                is_message    TYPE bapiret2
                it_messages   TYPE bapiret2_t
                iv_subrc      TYPE sysubrc
                it_input_data TYPE rsra_t_alert_definition.

    METHODS log_exception_raised.

    METHODS get_message       RETURNING VALUE(rs_message)  TYPE bapiret2.
    METHODS get_messages      RETURNING VALUE(rt_messages) TYPE bapiret2_t.
    METHODS get_call_on_super RETURNING VALUE(rv_result)   TYPE abap_bool.
    METHODS reset_call_on_super.
    METHODS register_call_on_super.

  PROTECTED SECTION.
    "! Turn on/off automatic logging
    CLASS-DATA mv_is_log_enabled TYPE boolean VALUE zcx_root=>mc_log_enabled-false.
    CLASS-DATA mv_call_on_super  TYPE abap_bool.

    DATA mo_exception TYPE REF TO zcx_if_check_class.

    METHODS create_log_msgde
      IMPORTING it_input_data   TYPE rsra_t_alert_definition
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    METHODS det_bool
      IMPORTING iv_bool          TYPE abap_bool
      RETURNING VALUE(rv_result) TYPE boolean.

    METHODS det_cx_bool
      IMPORTING iv_cx_bool       TYPE boolean
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS is_log_enabled
      RETURNING VALUE(rv_result) TYPE abap_bool.

    "! <p class="shorttext synchronized"></p>
    "! <p><strong>Note:</strong>
    "! Z-Exceptions support automatic logging if the new syntax RAISE EXCEPTION NEW
    "! is being used or the exception object is being constructed either manually
    "! before being thrown or in the catching block via INTO DATA(lo_exception). As
    "! we want to handle all exception types (SAP and Non-SAP) the same way in regards
    "! to logging,  automatic logging has been turned off (LOG_ROOT_ENABLED) and one
    "! has to use ZIAL_CL_LOG=>GET( )->LOG_EXCEPTION( LO_EXCEPTION ). The parameter
    "! should be turned on again if automatic logging is to be used or everyone only
    "! works with RAISE EXCEPTION NEW as this always triggers the object constructor
    "! and thus the logging.</p>
    METHODS log_messages.

    METHODS get_text_by_super RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.


CLASS zcx_root IMPLEMENTATION.

  METHOD constructor.

    mo_exception = io_exception.

    IF    CAST cx_root( mo_exception )->textid IS INITIAL
       OR CAST cx_root( mo_exception )->textid EQ CAST cx_root( mo_exception )->cx_root.
      IF is_t100key IS NOT INITIAL.
        mo_exception->if_t100_message~t100key = is_t100key.
      ELSE.
        mo_exception->if_t100_message~t100key = if_t100_message=>default_textid.
      ENDIF.
    ENDIF.

    mo_exception->obj_id     = iv_obj_id.
    mo_exception->message    = is_message.
    mo_exception->messages   = it_messages.
    mo_exception->subrc      = iv_subrc.
    mo_exception->input_data = it_input_data.

    log_messages( ).

  ENDMETHOD.


  METHOD get_message.

    CLEAR: sy-msgid,
           sy-msgno,
           sy-msgty,
           sy-msgv1,
           sy-msgv2,
           sy-msgv3,
           sy-msgv4.

    DATA(lo_exception_as_root) = CAST cx_root( mo_exception ).

    WHILE mo_exception->message IS INITIAL.

      DATA(lv_index) = sy-index.
      CASE lv_index.
        WHEN 1.
          CHECK mo_exception->message IS NOT INITIAL.
          mo_exception->message = mo_exception->message.

        WHEN 2.
          CHECK mo_exception->messages IS NOT INITIAL.
          mo_exception->message = VALUE #( mo_exception->messages[ 1 ] OPTIONAL ).

        WHEN 3.
          CHECK lo_exception_as_root->textid IS NOT INITIAL
            AND lo_exception_as_root->textid NE lo_exception_as_root->cx_root.
          cl_message_helper=>get_otr_text_raw( EXPORTING textid = lo_exception_as_root->textid
                                               IMPORTING result = DATA(lv_msgtx) ).
          cl_message_helper=>replace_text_params( EXPORTING obj    = mo_exception
                                                  CHANGING  result = lv_msgtx ).
          mo_exception->message = zial_cl_log=>to_bapiret( iv_msgty = 'E'
                                                           iv_msgtx = CONV #( lv_msgtx ) ).

        WHEN 4.
          CHECK mo_exception->if_t100_message~t100key IS NOT INITIAL
            AND mo_exception->if_t100_message~t100key NE mo_exception->if_t100_message~default_textid.

          DATA(ls_message) = cl_message_helper=>get_t100_for_object( obj = mo_exception ).
          IF mo_exception->if_t100_dyn_msg~msgty IS NOT INITIAL.
            ls_message-msgty = mo_exception->if_t100_dyn_msg~msgty.
          ENDIF.
          IF mo_exception->if_t100_dyn_msg~msgv1 IS NOT INITIAL.
            ls_message-msgv1 = mo_exception->if_t100_dyn_msg~msgv1.
          ENDIF.
          IF mo_exception->if_t100_dyn_msg~msgv2 IS NOT INITIAL.
            ls_message-msgv2 = mo_exception->if_t100_dyn_msg~msgv2.
          ENDIF.
          IF mo_exception->if_t100_dyn_msg~msgv3 IS NOT INITIAL.
            ls_message-msgv3 = mo_exception->if_t100_dyn_msg~msgv3.
          ENDIF.
          IF mo_exception->if_t100_dyn_msg~msgv4 IS NOT INITIAL.
            ls_message-msgv4 = mo_exception->if_t100_dyn_msg~msgv4.
          ENDIF.

          mo_exception->message = zial_cl_log=>to_bapiret( iv_msgid = ls_message-msgid
                                                           iv_msgty = ls_message-msgty
                                                           iv_msgno = ls_message-msgno
                                                           iv_msgv1 = ls_message-msgv1
                                                           iv_msgv2 = ls_message-msgv2
                                                           iv_msgv3 = ls_message-msgv3
                                                           iv_msgv4 = ls_message-msgv4 ).

        WHEN 5.
          CHECK CAST cx_root( mo_exception )->previous IS BOUND.
          IF lo_exception_as_root->previous IS INSTANCE OF zcx_if_check_class.
            mo_exception->message = CAST zcx_if_check_class( lo_exception_as_root->previous )->get_message( ).
          ELSE.
            lv_msgtx = lo_exception_as_root->previous->get_text( ).
            mo_exception->message = zial_cl_log=>to_bapiret( iv_msgty = 'E'
                                                             iv_msgtx = CONV #( lv_msgtx ) ).
          ENDIF.

        WHEN 6.
          lv_msgtx = get_text_by_super( ).
          mo_exception->message = zial_cl_log=>to_bapiret( iv_msgty = 'E'
                                                           iv_msgtx = CONV #( lv_msgtx ) ).

        WHEN OTHERS.
          mo_exception->if_t100_message~t100key = mo_exception->if_t100_message~default_textid.
          ls_message = cl_message_helper=>get_t100_for_object( obj = mo_exception ).
          mo_exception->message = zial_cl_log=>to_bapiret( iv_msgid = ls_message-msgid
                                                           iv_msgty = ls_message-msgty
                                                           iv_msgno = ls_message-msgno
                                                           iv_msgv1 = ls_message-msgv1
                                                           iv_msgv2 = ls_message-msgv2
                                                           iv_msgv3 = ls_message-msgv3
                                                           iv_msgv4 = ls_message-msgv4 ).
          EXIT.

      ENDCASE.

    ENDWHILE.

    rs_message = mo_exception->message.

  ENDMETHOD.


  METHOD get_messages.

    DATA(lo_exception_as_root) = CAST cx_root( mo_exception ).

    IF lo_exception_as_root->previous IS BOUND.
      IF lo_exception_as_root->previous IS INSTANCE OF zcx_if_check_class.
        INSERT LINES OF CAST zcx_if_check_class( lo_exception_as_root->previous )->get_messages( ) INTO TABLE rt_messages.
      ELSE.
        INSERT zial_cl_log=>to_bapiret( iv_msgtx = CONV #( lo_exception_as_root->previous->get_text( ) )
                                        iv_msgty = 'E' ) INTO TABLE rt_messages.
      ENDIF.
    ENDIF.

    INSERT LINES OF mo_exception->messages INTO TABLE rt_messages.

    DATA(ls_message) = get_message( ).
    IF         ls_message IS NOT INITIAL
       AND NOT line_exists( rt_messages[ table_line = ls_message ] ).
      INSERT ls_message INTO TABLE rt_messages.
    ENDIF.

  ENDMETHOD.


  METHOD create_log_msgde.

    DATA(lv_line) = repeat( val = '-'
                            occ = 80 ).
    DATA(lo_abap_classdescr) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) ).
    DATA(lv_class_name) = lo_abap_classdescr->get_relative_name( ).

    APPEND LINES OF VALUE rsra_t_alert_definition( ( low  = lv_class_name ) ) TO rt_msgde.
    APPEND VALUE #( low = lv_line ) TO rt_msgde.
    APPEND LINES OF it_input_data TO rt_msgde.

    " Callstack is being added via ZIAL_CL_LOG=>GET( )->LOG_EXCEPTION( lo_exception ).

  ENDMETHOD.


  METHOD log_exception_raised.

    DATA(lo_abap_classdescr) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) ).
    DATA(lv_class_name) = lo_abap_classdescr->get_relative_name( ).

    DATA(lt_components) = zial_cl_log=>get_components_from_msgde( mo_exception->input_data ).
    MESSAGE e001(zial_exc_mgmt) WITH lv_class_name lt_components mo_exception->subrc INTO DATA(lv_msg) ##NEEDED.
    DATA(lt_msgde) = create_log_msgde( mo_exception->input_data ).
    zial_cl_log=>get( )->log_message( lt_msgde ).

  ENDMETHOD.


  METHOD det_bool.

    CASE iv_bool.
      WHEN abap_true.
        rv_result = zcx_root=>mc_log_enabled-true.

      WHEN abap_false.
        rv_result = zcx_root=>mc_log_enabled-false.

    ENDCASE.

  ENDMETHOD.


  METHOD det_cx_bool.

    CASE iv_cx_bool.
      WHEN zcx_root=>mc_log_enabled-true.
        rv_result = abap_true.

      WHEN zcx_root=>mc_log_enabled-false.
        rv_result = abap_false.

    ENDCASE.

  ENDMETHOD.


  METHOD is_log_enabled.

    CASE mv_is_log_enabled.
      WHEN zcx_root=>mc_log_enabled-true
        OR zcx_root=>mc_log_enabled-false.
        rv_result = det_cx_bool( mv_is_log_enabled ).

      WHEN OTHERS.
        rv_result = abap_false.

    ENDCASE.

  ENDMETHOD.


  METHOD log_messages.

    CHECK is_log_enabled( ) EQ abap_true.

    log_exception_raised( ).

    zial_cl_log=>get( )->log_bapiret( get_messages( ) ).

  ENDMETHOD.


  METHOD if_message~get_text.

    DATA(ls_message) = get_message( ).
    MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
      WITH ls_message-message_v1 ls_message-message_v2
           ls_message-message_v3 ls_message-message_v4 INTO result.

  ENDMETHOD.


  METHOD if_message~get_longtext.
    result = if_message~get_text( ).
  ENDMETHOD.


  METHOD get_text_by_super.
    register_call_on_super( ).
    rv_result = mo_exception->if_message~get_text( ).
  ENDMETHOD.


  METHOD get_call_on_super.
    rv_result = mv_call_on_super.
  ENDMETHOD.


  METHOD reset_call_on_super.
    mv_call_on_super = abap_false.
  ENDMETHOD.


  METHOD register_call_on_super.
    mv_call_on_super = abap_true.
  ENDMETHOD.

ENDCLASS.
