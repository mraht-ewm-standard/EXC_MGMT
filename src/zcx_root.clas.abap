CLASS zcx_root DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED
  GLOBAL FRIENDS zcx_no_check
                 zcx_static_check.

  PUBLIC SECTION.
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

    METHODS log_info.

    METHODS get_message
      RETURNING VALUE(rs_message) TYPE bapiret2.

    METHODS get_messages
      RETURNING VALUE(rt_messages) TYPE bapiret2_t.

    METHODS get_call_on_super
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS reset_call_on_super.
    METHODS register_call_on_super.

    METHODS display_message.

    METHODS is_dflt_message
      RETURNING VALUE(rv_result) TYPE abap_bool.

  PROTECTED SECTION.
    "! Turn on/off automatic logging
    CLASS-DATA is_auto_log_enabled TYPE abap_bool VALUE abap_false.

    DATA call_on_super TYPE abap_bool.
    DATA exception     TYPE REF TO zcx_if_check_class.

    METHODS create_log_msgde
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    "! <p class="shorttext synchronized"></p>
    "! <p><strong>Note:</strong>
    "! Z-Exceptions support automatic logging if the new syntax RAISE EXCEPTION NEW
    "! is being used or the exception object is being constructed either manually
    "! before being thrown or in the catching block via INTO DATA(lo_exception). As
    "! we want to handle all exception types (SAP and Non-SAP) the same way in regards
    "! to logging,  automatic logging has been turned off (LOG_ROOT_ENABLED) and one
    "! has to use ZIAL_CL_LOG=&gt;GET( )-&gt;LOG_EXCEPTION( LO_EXCEPTION ). The parameter
    "! should be turned on again if automatic logging is to be used or everyone only
    "! works with RAISE EXCEPTION NEW as this always triggers the object constructor
    "! and thus the logging.</p>
    METHODS log.

    METHODS get_text_by_super
      RETURNING VALUE(rv_result) TYPE string.

    METHODS get_text
      RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.


CLASS zcx_root IMPLEMENTATION.

  METHOD constructor.

    exception = io_exception.

    IF    CAST cx_root( exception )->textid IS INITIAL
       OR CAST cx_root( exception )->textid EQ CAST cx_root( exception )->cx_root.
      IF is_t100key IS NOT INITIAL.
        exception->if_t100_message~t100key = is_t100key.
      ELSE.
        exception->if_t100_message~t100key = if_t100_message=>default_textid.
      ENDIF.
    ENDIF.

    exception->obj_id     = iv_obj_id.
    exception->message    = is_message.
    exception->messages   = it_messages.
    exception->subrc      = iv_subrc.
    exception->input_data = it_input_data.

    IF is_auto_log_enabled EQ abap_true.
      log( ).
    ENDIF.

  ENDMETHOD.


  METHOD create_log_msgde.

    DATA(lo_abap_classdescr) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) ).
    DATA(lv_class_name) = lo_abap_classdescr->get_relative_name( ).

    APPEND LINES OF VALUE rsra_t_alert_definition( ( low  = lv_class_name ) ) TO rt_msgde.
    APPEND VALUE #( low = repeat( val = '-'
                                  occ = 80 ) ) TO rt_msgde.
    APPEND LINES OF exception->input_data TO rt_msgde.

    " Callstack is being added via ZIAL_CL_LOG=>GET( )->LOG_EXCEPTION( lo_exception ).

  ENDMETHOD.


  METHOD get_call_on_super.
    rv_result = call_on_super.
  ENDMETHOD.


  METHOD get_message.

    CLEAR: sy-msgid,
           sy-msgno,
           sy-msgty,
           sy-msgv1,
           sy-msgv2,
           sy-msgv3,
           sy-msgv4.

    DATA(lo_exception_as_root) = CAST cx_root( exception ).

    WHILE exception->message IS INITIAL.

      DATA(lv_index) = sy-index.

      DATA(lt_messages) = VALUE bapiret2_t( ).
      CASE lv_index.
        WHEN 1.
          CHECK exception->messages IS NOT INITIAL.
          exception->message = VALUE #( exception->messages[ 1 ] OPTIONAL ).

        WHEN 2.
          CHECK lo_exception_as_root->textid IS NOT INITIAL
            AND lo_exception_as_root->textid NE lo_exception_as_root->cx_root.
          cl_message_helper=>get_otr_text_raw( EXPORTING textid = lo_exception_as_root->textid
                                               IMPORTING result = DATA(lv_msgtx) ).
          cl_message_helper=>replace_text_params( EXPORTING obj    = exception
                                                  CHANGING  result = lv_msgtx ).
          lt_messages = zial_cl_log=>to_bapiret( iv_msgty = 'E'
                                                 iv_msgtx = CONV #( lv_msgtx ) ).
          exception->message = VALUE #( lt_messages[ 1 ] OPTIONAL ).

        WHEN 3.
          CHECK exception->if_t100_message~t100key IS NOT INITIAL
            AND exception->if_t100_message~t100key NE exception->if_t100_message~default_textid.

          DATA(ls_message) = zcl_message_helper=>get_t100_for_object( obj = exception ).
          IF exception->if_t100_dyn_msg~msgty IS NOT INITIAL.
            ls_message-msgty = exception->if_t100_dyn_msg~msgty.
          ENDIF.
          IF exception->if_t100_dyn_msg~msgv1 IS NOT INITIAL.
            ls_message-msgv1 = exception->if_t100_dyn_msg~msgv1.
          ENDIF.
          IF exception->if_t100_dyn_msg~msgv2 IS NOT INITIAL.
            ls_message-msgv2 = exception->if_t100_dyn_msg~msgv2.
          ENDIF.
          IF exception->if_t100_dyn_msg~msgv3 IS NOT INITIAL.
            ls_message-msgv3 = exception->if_t100_dyn_msg~msgv3.
          ENDIF.
          IF exception->if_t100_dyn_msg~msgv4 IS NOT INITIAL.
            ls_message-msgv4 = exception->if_t100_dyn_msg~msgv4.
          ENDIF.

          lt_messages = zial_cl_log=>to_bapiret( iv_msgid = ls_message-msgid
                                                 iv_msgty = ls_message-msgty
                                                 iv_msgno = ls_message-msgno
                                                 iv_msgv1 = ls_message-msgv1
                                                 iv_msgv2 = ls_message-msgv2
                                                 iv_msgv3 = ls_message-msgv3
                                                 iv_msgv4 = ls_message-msgv4 ).
          exception->message = VALUE #( lt_messages[ 1 ] OPTIONAL ).

        WHEN 4.
          CHECK CAST cx_root( exception )->previous IS BOUND.
          IF lo_exception_as_root->previous IS INSTANCE OF zcx_if_check_class.
            exception->message = CAST zcx_if_check_class( lo_exception_as_root->previous )->get_message( ).
          ELSE.
            lv_msgtx = lo_exception_as_root->previous->get_text( ).
            lt_messages = zial_cl_log=>to_bapiret( iv_msgty = 'E'
                                                   iv_msgtx = CONV #( lv_msgtx ) ).
            exception->message = VALUE #( lt_messages[ 1 ] OPTIONAL ).
          ENDIF.

        WHEN OTHERS.
          lv_msgtx = get_text_by_super( ).
          lt_messages = zial_cl_log=>to_bapiret( iv_msgty = 'E'
                                                 iv_msgtx = CONV #( lv_msgtx ) ).
          exception->message = VALUE #( lt_messages[ 1 ] OPTIONAL ).
          EXIT.

      ENDCASE.

    ENDWHILE.

    rs_message = exception->message.

  ENDMETHOD.


  METHOD get_messages.

    DATA(lo_exception_as_root) = CAST cx_root( exception ).

    IF lo_exception_as_root->previous IS BOUND.
      IF lo_exception_as_root->previous IS INSTANCE OF zcx_if_check_class.
        INSERT LINES OF CAST zcx_if_check_class( lo_exception_as_root->previous )->get_messages( ) INTO TABLE rt_messages.
      ELSE.
        INSERT LINES OF zial_cl_log=>to_bapiret( iv_msgtx = CONV #( lo_exception_as_root->previous->get_text( ) )
                                                 iv_msgty = 'E' ) INTO TABLE rt_messages.
      ENDIF.
    ENDIF.

    INSERT LINES OF exception->messages INTO TABLE rt_messages.

    DATA(ls_message) = get_message( ).
    IF         ls_message IS NOT INITIAL
       AND NOT line_exists( rt_messages[ table_line = ls_message ] ).
      INSERT ls_message INTO TABLE rt_messages.
    ENDIF.

  ENDMETHOD.


  METHOD get_text_by_super.
    register_call_on_super( ).
    rv_result = exception->if_message~get_text( ).
  ENDMETHOD.


  METHOD get_text.

    rv_result = get_message( )-message.

  ENDMETHOD.


  METHOD log_info.

    DATA(lo_abap_classdescr) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( exception ) ).
    DATA(lv_class_name) = lo_abap_classdescr->get_relative_name( ).

    DATA(lt_components) = zial_cl_log=>get_components_from_msgde( exception->input_data ).
    MESSAGE e001(zial_exc_mgmt) WITH lv_class_name lt_components exception->subrc INTO DATA(lv_msg) ##NEEDED.
    DATA(lt_msgde) = create_log_msgde( ).
    zial_cl_log=>get( )->log_message( lt_msgde ).

  ENDMETHOD.


  METHOD log.

    log_info( ).

    zial_cl_log=>get( )->log_bapiret( get_messages( ) ).

  ENDMETHOD.


  METHOD register_call_on_super.
    call_on_super = abap_true.
  ENDMETHOD.


  METHOD reset_call_on_super.
    call_on_super = abap_false.
  ENDMETHOD.


  METHOD display_message.
    DATA(ls_message) = get_message( ).
    MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDMETHOD.


  METHOD is_dflt_message.

    DATA(ls_message) = get_message( ).
    MESSAGE ID exception->if_t100_message~default_textid-msgid
            TYPE 'E' NUMBER exception->if_t100_message~default_textid-msgno INTO DATA(lv_msgtx).

    CHECK ls_message-message EQ lv_msgtx
       OR (     ls_message-id     EQ exception->if_t100_message~default_textid-msgid
            AND ls_message-number EQ exception->if_t100_message~default_textid-msgno ).

    rv_result = abap_true.

  ENDMETHOD.

ENDCLASS.
