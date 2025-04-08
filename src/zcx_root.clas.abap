"! <p class="shorttext synchronized">Root</p>
CLASS zcx_root DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED
  GLOBAL FRIENDS zcx_no_check
                 zcx_static_check
                 zcx_dynamic_check.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_obj_id,
                 generic TYPE objectname VALUE 'OBJECT',
               END OF mc_obj_id.

    CLASS-METHODS conv_sap_cx
      IMPORTING io_previous        TYPE REF TO cx_root
      RETURNING VALUE(ro_instance) TYPE REF TO zcx_if_check_class.

    CLASS-METHODS det_class_name
      IMPORTING io_exception         TYPE REF TO zcx_if_check_class
      RETURNING VALUE(rv_class_name) TYPE classname.

    CLASS-METHODS get_class_name
      IMPORTING io_exception         TYPE REF TO cx_root
      RETURNING VALUE(rv_class_name) TYPE classname.

    METHODS constructor
      IMPORTING io_exception        TYPE REF TO zcx_if_check_class
                is_t100key          TYPE scx_t100key
                iv_obj_id           TYPE objectname
                is_message          TYPE bapiret2
                it_messages         TYPE bapiret2_t
                iv_subrc            TYPE sysubrc
                it_input_data       TYPE rsra_t_alert_definition
                is_auto_log_enabled TYPE abap_bool.

    "! NOT supported!
    METHODS log_info.

    "! NOT supported!
    METHODS log_messages.

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
    TYPES: BEGIN OF s_dflt_textid,
             msgid TYPE msgid,
             msgno TYPE msgno,
             msgtx TYPE bapi_msg,
           END OF s_dflt_textid,
           t_dflt_textids TYPE SORTED TABLE OF s_dflt_textid WITH UNIQUE KEY msgid msgno.

    CONSTANTS: BEGIN OF default_textid,
                 msgid TYPE symsgid      VALUE 'ZIAL_EXC_MGMT',
                 msgno TYPE symsgno      VALUE '000',
                 attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF default_textid.

    CLASS-DATA mt_dflt_textids TYPE t_dflt_textids.

    DATA call_on_super TYPE abap_bool.
    DATA exception     TYPE REF TO zcx_if_check_class.

    "! Callstack is being added via zial_cl_log_msg=>GET( )->LOG_EXCEPTION( lo_exception ).
    "! @parameter rt_msgde | Message details
    METHODS create_log_msgde
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    "! <p class="shorttext synchronized"></p>
    "! <p>NOT supported in NO_LOGGING version!</p>
    "! <p><strong>Usage:</strong>
    "! Z-Exceptions support automatic logging if the new syntax RAISE EXCEPTION NEW
    "! is being used or the exception object is being constructed either manually
    "! before being thrown or in the catching block via INTO DATA(lo_exception). As
    "! we want to handle all exception types (SAP and Non-SAP) the same way in regards
    "! to logging,  automatic logging has been turned off (LOG_ROOT_ENABLED) and one
    "! has to use zial_cl_log_msg=&gt;GET( )-&gt;LOG_EXCEPTION( LO_EXCEPTION ). The parameter
    "! should be turned on again if automatic logging is to be used or everyone only
    "! works with RAISE EXCEPTION NEW as this always triggers the object constructor
    "! and thus the logging.</p>
    METHODS log
      IMPORTING iv_with_info TYPE abap_bool DEFAULT abap_true.

    METHODS get_text_by_super
      RETURNING VALUE(rv_result) TYPE string.

    METHODS get_text
      RETURNING VALUE(rv_result) TYPE string.

    METHODS init_dflt_textids.

ENDCLASS.


CLASS zcx_root IMPLEMENTATION.

  METHOD constructor.

    exception = io_exception.

    IF    CAST cx_root( exception )->textid IS INITIAL
       OR CAST cx_root( exception )->textid EQ CAST cx_root( exception )->cx_root.
      IF is_t100key IS NOT INITIAL.
        exception->if_t100_message~t100key = is_t100key.
      ELSEIF exception->class_name IS NOT INITIAL.
        exception->if_t100_message~t100key = default_textid.
      ELSE.
        exception->if_t100_message~t100key = if_t100_message=>default_textid.
      ENDIF.
    ENDIF.

    init_dflt_textids( ).

    exception->obj_id              = iv_obj_id.
    exception->message             = is_message.
    exception->messages            = it_messages.
    exception->subrc               = iv_subrc.
    exception->input_data          = it_input_data.
    exception->is_auto_log_enabled = is_auto_log_enabled.

  ENDMETHOD.


  METHOD create_log_msgde.

    DATA(lo_abap_classdescr) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) ).
    DATA(lv_class_name) = lo_abap_classdescr->get_relative_name( ).

    APPEND LINES OF VALUE rsra_t_alert_definition( ( low  = lv_class_name ) ) TO rt_msgde.
    APPEND VALUE #( low = repeat( val = '-'
                                  occ = 80 ) ) TO rt_msgde.
    APPEND LINES OF exception->input_data TO rt_msgde.

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
          exception->message = zial_cl_log_msg=>to_bapiret( iv_msgty = 'E'
                                                        iv_msgtx = lv_msgtx ).

        WHEN 3.
          CHECK exception->if_t100_message~t100key IS NOT INITIAL
            AND exception->if_t100_message~t100key NE exception->if_t100_message~default_textid
            AND exception->if_t100_message~t100key NE default_textid.

          DATA(ls_message) = zcl_message_statement_helper=>get_t100_for_object( exception ).
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

          exception->message = zial_cl_log_msg=>to_bapiret( iv_msgid = ls_message-msgid
                                                        iv_msgty = ls_message-msgty
                                                        iv_msgno = ls_message-msgno
                                                        iv_msgv1 = ls_message-msgv1
                                                        iv_msgv2 = ls_message-msgv2
                                                        iv_msgv3 = ls_message-msgv3
                                                        iv_msgv4 = ls_message-msgv4 ).

        WHEN 4.
          CHECK lo_exception_as_root->previous IS BOUND.
          IF lo_exception_as_root->previous IS INSTANCE OF zcx_if_check_class.
            exception->message = CAST zcx_if_check_class( lo_exception_as_root->previous )->get_message( ).
          ELSE.
            lv_msgtx = lo_exception_as_root->previous->get_text( ).
            exception->message = zial_cl_log_msg=>to_bapiret( iv_msgty = 'E'
                                                          iv_msgtx = lv_msgtx ).
          ENDIF.

        WHEN OTHERS.
          lv_msgtx = get_text_by_super( ).
          exception->message = zial_cl_log_msg=>to_bapiret( iv_msgty = 'E'
                                                        iv_msgtx = lv_msgtx ).
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
        INSERT LINES OF zial_cl_log_msg=>to_bapirets( iv_msgtx = CONV #( lo_exception_as_root->previous->get_text( ) )
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

    CHECK exception->root IS NOT INITIAL.
    register_call_on_super( ).
    rv_result = exception->if_message~get_text( ).

  ENDMETHOD.


  METHOD get_text.

    rv_result = get_message( )-message.

  ENDMETHOD.


  METHOD log_info.
  ENDMETHOD.


  METHOD log.

    IF iv_with_info EQ abap_true.
      log_info( ).
    ENDIF.

    log_messages( ).

  ENDMETHOD.


  METHOD log_messages.
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


  METHOD init_dflt_textids.

    CHECK mt_dflt_textids IS INITIAL.

    mt_dflt_textids = VALUE #( ( msgid = exception->if_t100_message~default_textid-msgid
                                 msgno = exception->if_t100_message~default_textid-msgno )
                               ( msgid = default_textid-msgid
                                 msgno = default_textid-msgno ) ).

    LOOP AT mt_dflt_textids ASSIGNING FIELD-SYMBOL(<ls_dflt_textid>).
      MESSAGE ID <ls_dflt_textid>-msgid TYPE 'E' NUMBER <ls_dflt_textid>-msgno INTO <ls_dflt_textid>-msgtx.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_dflt_message.

    DATA(ls_message) = get_message( ).
    CHECK line_exists( mt_dflt_textids[ msgid = ls_message-id
                                        msgno = ls_message-number ] )
       OR (     ls_message-message IS NOT INITIAL
            AND line_exists( mt_dflt_textids[ msgtx = ls_message-message ] ) ).

    rv_result = abap_true.

  ENDMETHOD.


  METHOD conv_sap_cx.

    CASE TYPE OF io_previous.
      WHEN TYPE zcx_if_check_class.
        ro_instance ?= io_previous.

      WHEN OTHERS.
        TRY.
            RAISE EXCEPTION TYPE zcx_sap_cx
              EXPORTING previous = io_previous.

          CATCH zcx_static_check INTO DATA(lo_instance).
            ro_instance ?= lo_instance.

        ENDTRY.

    ENDCASE.

  ENDMETHOD.


  METHOD det_class_name.

    DATA(lo_exception_as_root) = CAST cx_root( io_exception ).
    rv_class_name = get_class_name( lo_exception_as_root ).
    IF    rv_class_name EQ 'ZCX_STATIC_CHECK'
       OR rv_class_name EQ 'ZCX_NO_CHECK'
       OR rv_class_name EQ 'ZCX_SAP_CX'.
      rv_class_name = get_class_name( lo_exception_as_root->previous ).
    ENDIF.

  ENDMETHOD.


  METHOD get_class_name.

    DATA(lv_class_name) = cl_abap_classdescr=>get_class_name( io_exception ).
    SPLIT lv_class_name AT '\CLASS=' INTO DATA(lv_ignore) rv_class_name ##NEEDED.

  ENDMETHOD.

ENDCLASS.
