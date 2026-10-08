"! <p class="shorttext synchronized">Root</p>
CLASS zcx_root DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED
  GLOBAL FRIENDS zcx_no_check
                 zcx_static_check
                 zcx_dynamic_check.

  PUBLIC SECTION.
    INTERFACES zcx_if_apack_dep_logging.

    ALIASES get_message       FOR zcx_if_apack_dep_logging~get_message.
    ALIASES get_messages      FOR zcx_if_apack_dep_logging~get_messages.
    ALIASES get_messages_ext  FOR zcx_if_apack_dep_logging~get_messages_ext.
    ALIASES get_messages_prev FOR zcx_if_apack_dep_logging~get_messages_prev.
    ALIASES log_info          FOR zcx_if_apack_dep_logging~log_info.
    ALIASES log_messages      FOR zcx_if_apack_dep_logging~log_messages.

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

    "! Callstack is being added via ZIAL_CL_LOG=>GET( )->LOG_EXCEPTION( lo_exception ).
    "! @parameter rt_msgde | Message details
    METHODS create_log_msgde
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    "! <p class="shorttext synchronized"></p>
    "! <p>NOT supported in NO_LOGGING version!</p>
    "! <p><strong>Usage:</strong>
    "! Customer-specific exceptions support automatic logging if the new syntax RAISE
    "! EXCEPTION NEW is being used or the exception object is being constructed either
    "! manually before being thrown or in the catching block via RAISE EXCEPTION TYPE
    "! ... INTO DATA(lo_exception). As we want to handle all exception types (SAP and
    "! Non-SAP) the same way in regards to logging, automatic logging has been turned
    "! off (LOG_ROOT_ENABLED). One has to use ZIAL_CL_LOG=>GET( )->LOG_EXCEPTION
    "! ( LO_EXCEPTION ). The parameter should be turned on again if automatic logging
    "! is to be used or everyone only works with RAISE EXCEPTION NEW as this always
    "! triggers the object constructor and thus the logging.</p>
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

    ##NOT_SUPPORTED. " Package LOGGING missing!

  ENDMETHOD.


  METHOD get_messages.

    ##NOT_SUPPORTED. " Package LOGGING missing!

  ENDMETHOD.


  METHOD get_messages_prev.

    ##NOT_SUPPORTED. " Package LOGGING missing!

  ENDMETHOD.


  METHOD get_messages_ext.

    ##NOT_SUPPORTED. " Package LOGGING missing!

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

    ##NOT_SUPPORTED. " Package LOGGING missing!

  ENDMETHOD.


  METHOD log.

    IF iv_with_info EQ abap_true.
      log_info( ).
    ENDIF.

    log_messages( ).

  ENDMETHOD.


  METHOD log_messages.

    ##NOT_SUPPORTED. " Package LOGGING missing!

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
    IF     NOT line_exists( mt_dflt_textids[ msgid = ls_message-id
                                             msgno = ls_message-number ] )
       AND (        ls_message-message IS INITIAL
             OR NOT line_exists( mt_dflt_textids[ msgtx = ls_message-message ] ) ).
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD conv_sap_cx.

    CASE TYPE OF io_previous.
      WHEN TYPE zcx_if_check_class.
        ro_instance ?= io_previous.

      WHEN OTHERS.
        TRY.
            RAISE EXCEPTION NEW zcx_sap_cx( previous = io_previous ).

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
