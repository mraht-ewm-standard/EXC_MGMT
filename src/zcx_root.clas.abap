CLASS zcx_root DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

**********************************************************************
* Notes regarding Logging:
* Please use the new syntax RAISE EXCEPTION NEW to avoid logging
* messages being cleaned up by SAP after RAISE EXCEPTION TYPE. If
* one doesn't use INTO DATA in the CATCH block the object and all
* changes to external objects are being rolled back too! This is
* not the case if you create exception objects explicitly!
**********************************************************************

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    TYPES: cx_bool TYPE i.
    CONSTANTS: undef TYPE cx_bool VALUE 0,
               true  TYPE cx_bool VALUE 1,
               false TYPE cx_bool VALUE 2.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL.

    CLASS-METHODS enable_log_root
      IMPORTING
        iv_log_enabled TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-DATA: log_root_enabled TYPE cx_bool VALUE zcx_root=>true.

    DATA: object_type TYPE char50,
          subrc       TYPE sysubrc,
          input_data  TYPE rsra_t_alert_definition.

    DATA log_instance_enabled TYPE cx_bool VALUE zcx_root=>undef.

    CLASS-METHODS det_bool
      IMPORTING
        iv_bool          TYPE abap_bool
      RETURNING
        VALUE(rv_result) TYPE i.
    CLASS-METHODS det_cx_bool
      IMPORTING
        iv_cx_bool       TYPE i
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS log_input
      IMPORTING
        action TYPE string OPTIONAL.
    METHODS create_log_msgde
      IMPORTING
        it_input_data   TYPE rsra_t_alert_definition
      RETURNING
        VALUE(rt_msgde) TYPE rsra_t_alert_definition.
    METHODS is_log_instance_enabled
      RETURNING
        VALUE(rv_log) TYPE cx_bool.
    METHODS enable_log_instance
      IMPORTING
        log_enabled TYPE abap_bool.
    METHODS is_log_enabled
      RETURNING
        VALUE(rv_log) TYPE abap_bool.
    METHODS reset_enable_log_instance.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_ROOT IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.


  METHOD create_log_msgde.

    DATA(lv_line) = repeat( val = '-' occ = 80 ).
    DATA(lv_class_name) = cl_abap_classdescr=>get_class_name( me ).

    APPEND LINES OF VALUE rsra_t_alert_definition( ( low  = lv_class_name ) ) TO rt_msgde.
    APPEND VALUE #( low = lv_line ) TO rt_msgde.

    IF it_input_data IS NOT INITIAL.
      APPEND LINES OF it_input_data TO rt_msgde.
      APPEND VALUE #( low = lv_line ) TO rt_msgde.
    ENDIF.

    ziot_cl_session=>get_callstack(
      IMPORTING
        et_callstack = DATA(lt_callstack) ).
    DELETE lt_callstack WHERE mainprogram CS 'ZCX_'.

    APPEND LINES OF VALUE rsra_t_alert_definition( FOR <s_callstack> IN lt_callstack
                                                     ( low  = |{ <s_callstack>-mainprogram }=>| &&
                                                              |{ <s_callstack>-event }, { ziot_cl_text=>get_by_enc_text( |LINE| ) } { <s_callstack>-line }| ) ) TO rt_msgde.

  ENDMETHOD.


  METHOD det_bool.

    CASE iv_bool.
      WHEN abap_true.
        rv_result = true.

      WHEN abap_false.
        rv_result = false.

    ENDCASE.

  ENDMETHOD.


  METHOD det_cx_bool.

    CASE iv_cx_bool.
      WHEN true.
        rv_result = abap_true.

      WHEN false.
        rv_result = abap_false.

    ENDCASE.

  ENDMETHOD.


  METHOD enable_log_instance.
    log_instance_enabled = zcx_root=>det_bool( log_enabled ).
  ENDMETHOD.


  METHOD enable_log_root.
    log_root_enabled = zcx_root=>det_bool( iv_log_enabled ).
  ENDMETHOD.


  METHOD is_log_enabled.

    CONSTANTS: lc_log_instance_method TYPE string VALUE 'IS_LOG_INSTANCE_ENABLED',
               lc_log_class_method    TYPE string VALUE 'IS_LOG_CLASS_ENABLED',
               lc_log_parent_method   TYPE string VALUE 'IS_LOG_PARENT_ENABLED'.

    DATA: lo_classdescr TYPE REF TO cl_abap_classdescr.
    lo_classdescr ?= cl_abap_typedescr=>describe_by_object_ref( me ).

    IF line_exists( lo_classdescr->methods[ name = lc_log_instance_method ] ).

      DATA(log_instance_enabled) = VALUE zcx_root=>cx_bool( ).
      CALL METHOD me->(lc_log_instance_method)
        RECEIVING
          rv_log = log_instance_enabled.
      CASE log_instance_enabled.
        WHEN zcx_root=>true
          OR zcx_root=>false.
          rv_log = zcx_root=>det_cx_bool( log_instance_enabled ).
          RETURN.

      ENDCASE.

    ENDIF.

    IF line_exists( lo_classdescr->methods[ name = lc_log_class_method ] ).

      DATA(lv_class_name) = lo_classdescr->absolute_name.
      DATA(log_class_enabled) = VALUE zcx_root=>cx_bool( ).
      CALL METHOD (lv_class_name)=>(lc_log_class_method)
        RECEIVING
          rv_log = log_class_enabled.
      CASE log_class_enabled.
        WHEN zcx_root=>true
          OR zcx_root=>false.
          rv_log = zcx_root=>det_cx_bool( log_class_enabled ).
          RETURN.

      ENDCASE.

    ENDIF.

    DATA(lo_parent_classdescr) = lo_classdescr->get_super_class_type( ).

    IF lo_parent_classdescr IS BOUND.

      IF line_exists( lo_parent_classdescr->methods[ name = lc_log_parent_method ] ).

        DATA(lv_parent_name) = lo_parent_classdescr->absolute_name.
        DATA(log_parent_enabled) = VALUE zcx_root=>cx_bool( ).
        CALL METHOD (lv_parent_name)=>(lc_log_parent_method)
          RECEIVING
            rv_log_enabled = log_parent_enabled.
        CASE log_parent_enabled.
          WHEN zcx_root=>true
            OR zcx_root=>false.
            rv_log = zcx_root=>det_cx_bool( log_parent_enabled ).
            RETURN.

        ENDCASE.

      ENDIF.

    ENDIF.

    CASE log_root_enabled.
      WHEN zcx_root=>true
        OR zcx_root=>false.
        rv_log = zcx_root=>det_cx_bool( log_root_enabled ).
        RETURN.

    ENDCASE.

  ENDMETHOD.


  METHOD is_log_instance_enabled.
    rv_log = log_instance_enabled.
  ENDMETHOD.


  METHOD log_input.

    CHECK is_log_enabled( ) EQ abap_true.

    DATA(lv_action) = action.
    IF lv_action IS INITIAL.
      lv_action = |[UNKOWN_ACTION]|.
    ENDIF.

    DATA(components) = ziot_cl_log=>get_components_from_msgde( input_data ).
    MESSAGE e002(ziot_basis) WITH object_type to_lower( lv_action ) components subrc INTO DATA(lv_msg).
    DATA(msgde) = create_log_msgde( input_data ).
    ziot_cl_log=>get_instance( )->log_message( msgde ).

  ENDMETHOD.


  METHOD reset_enable_log_instance.
    log_instance_enabled = zcx_root=>undef.
  ENDMETHOD.

ENDCLASS.
