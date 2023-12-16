CLASS zcx_no_check DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check ABSTRACT
  CREATE PUBLIC.

  " ---------------------------------------------------------------------
  " Notes regarding Logging:
  " Please use the new syntax RAISE EXCEPTION NEW to avoid logging
  " messages being cleaned up by SAP after RAISE EXCEPTION TYPE. If
  " one doesn't use INTO DATA in the CATCH block the object and all
  " changes to external objects are being rolled back too! This is
  " not the case if you create exception objects explicitly!
  " ---------------------------------------------------------------------

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.
    INTERFACES zif_cx_root.

    TYPES de_bool TYPE n LENGTH 1.

    CONSTANTS: BEGIN OF mc_log_enabled,
                 undef TYPE de_bool VALUE 0,
                 true  TYPE de_bool VALUE 1,
                 false TYPE de_bool VALUE 2,
               END OF mc_log_enabled.

    CONSTANTS: BEGIN OF mc_obj_id,
                 generic TYPE objectname VALUE 'OBJECT',
               END OF mc_obj_id.

    CLASS-METHODS is_root_log_enabled
      RETURNING VALUE(rv_is_enabled) TYPE abap_bool.

    METHODS constructor
      IMPORTING iv_textid     LIKE if_t100_message=>t100key OPTIONAL
                !previous     LIKE previous                 OPTIONAL
                obj_id        TYPE objectname               DEFAULT mc_obj_id-generic
                !log          TYPE abap_bool                OPTIONAL
                is_message    TYPE bapiret2                 OPTIONAL
                it_messages   TYPE bapirettab               OPTIONAL
                iv_subrc      TYPE sysubrc                  DEFAULT sy-subrc
                it_input_data TYPE rsra_t_alert_definition  OPTIONAL.

    METHODS get_message
      RETURNING VALUE(rs_message) TYPE bapiret2.

    METHODS get_messages
      RETURNING VALUE(rt_messages) TYPE bapirettab.

    METHODS get_obj_id
      RETURNING VALUE(rv_obj_id) TYPE objectname.

    METHODS get_input_data
      RETURNING VALUE(rt_input_data) TYPE rsra_t_alert_definition.

    METHODS if_message~get_text REDEFINITION.

  PROTECTED SECTION.
    CLASS-DATA log_root_enabled TYPE de_bool VALUE mc_log_enabled-true.

    CLASS-METHODS det_bool
      IMPORTING iv_bool          TYPE abap_bool
      RETURNING VALUE(rv_result) TYPE de_bool.

    CLASS-METHODS det_cx_bool
      IMPORTING iv_cx_bool       TYPE de_bool
      RETURNING VALUE(rv_result) TYPE abap_bool.

    DATA obj_id               TYPE objectname.
    DATA ms_message           TYPE bapiret2.
    DATA mt_messages          TYPE bapirettab.
    DATA mv_subrc             TYPE sysubrc.
    DATA mt_input_data        TYPE rsra_t_alert_definition.

    DATA log_instance_enabled TYPE de_bool VALUE mc_log_enabled-undef.

    METHODS log_messages.

    METHODS create_log_msgde
      IMPORTING it_input_data   TYPE rsra_t_alert_definition
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    METHODS is_log_instance_enabled
      RETURNING VALUE(rv_log_enabled) TYPE de_bool.

    METHODS enable_log_instance
      IMPORTING mc_log_enabled TYPE abap_bool.

    METHODS is_log_enabled
      RETURNING VALUE(rv_is_enabled) TYPE abap_bool.

    METHODS reset_enable_log_instance.
    METHODS on_construction.

ENDCLASS.


CLASS zcx_no_check IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    CLEAR me->textid.
    IF iv_textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = iv_textid.
    ENDIF.

    IF log IS SUPPLIED.
      enable_log_instance( log ).
    ELSE.
      reset_enable_log_instance( ).
    ENDIF.

    ms_message    = is_message.
    mt_messages   = it_messages.
    mv_subrc      = iv_subrc.
    mt_input_data = it_input_data.

    log_messages( ).

    on_construction( ).

  ENDMETHOD.


  METHOD on_construction.

    " Customer-specific construction as redefinition of constructor is not
    " allowed and one would have to define the whole constructor over and
    " over again in each sub class. Thus wouldn't have to change all sub
    " classes if the signature of the constructor would change in ROOT class.

  ENDMETHOD.


  METHOD create_log_msgde.

    DATA(lv_line) = repeat( val = '-'
                            occ = 80 ).
    DATA(lv_class_name) = cl_abap_classdescr=>get_class_name( me ).

    APPEND LINES OF VALUE rsra_t_alert_definition( ( low  = lv_class_name ) ) TO rt_msgde.
    APPEND VALUE #( low = lv_line ) TO rt_msgde.

    IF it_input_data IS NOT INITIAL.
      APPEND LINES OF it_input_data TO rt_msgde.
      APPEND VALUE #( low = lv_line ) TO rt_msgde.
    ENDIF.

    zial_cl_session=>get_callstack( IMPORTING et_callstack = DATA(lt_callstack) ).
    DELETE lt_callstack WHERE mainprogram CS 'ZCX_'.

    APPEND LINES OF VALUE rsra_t_alert_definition( FOR <s_callstack> IN lt_callstack
                                                   ( low = |{ <s_callstack>-mainprogram }=>| &&
                                                           |{ <s_callstack>-event }, | &&
                                                           |Line { <s_callstack>-line }| ) ) TO rt_msgde.

  ENDMETHOD.


  METHOD enable_log_instance.

    log_instance_enabled = det_bool( mc_log_enabled ).

  ENDMETHOD.


  METHOD is_log_instance_enabled.

    rv_log_enabled = log_instance_enabled.

  ENDMETHOD.


  METHOD is_log_enabled.

    CONSTANTS lc_log_instance_method TYPE string VALUE 'ZIF_CX_ROOT~IS_LOG_INSTANCE_ENABLED'.
    CONSTANTS lc_log_class_method    TYPE string VALUE 'ZIF_CX_CLASS~IS_LOG_ENABLED'.
    CONSTANTS lc_log_group_method    TYPE string VALUE 'ZIF_CX_GROUP~IS_LOG_ENABLED'.

    DATA(lo_classdescr) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) ).
    IF line_exists( lo_classdescr->methods[ name = lc_log_instance_method ] ).

      DATA(lv_log_instance_enabled) = VALUE de_bool( ).
      CALL METHOD (lc_log_instance_method)
        RECEIVING rv_is_enabled = lv_log_instance_enabled.
      CASE lv_log_instance_enabled.
        WHEN mc_log_enabled-true
          OR mc_log_enabled-false.
          rv_is_enabled = det_cx_bool( lv_log_instance_enabled ).
          RETURN.

      ENDCASE.

    ENDIF.

    IF line_exists( lo_classdescr->methods[ name = lc_log_class_method ] ).

      DATA(lv_class_name) = lo_classdescr->absolute_name.
      DATA(lv_log_class_enabled) = VALUE de_bool( ).
      CALL METHOD (lv_class_name)=>(lc_log_class_method)
        RECEIVING rv_is_enabled = lv_log_class_enabled.
      CASE lv_log_class_enabled.
        WHEN mc_log_enabled-true
          OR mc_log_enabled-false.
          rv_is_enabled = det_cx_bool( lv_log_class_enabled ).
          RETURN.

      ENDCASE.

    ENDIF.

    DATA(lo_group_classdescr) = lo_classdescr->get_super_class_type( ).
    IF     lo_group_classdescr IS BOUND
       AND line_exists( lo_group_classdescr->methods[ name = lc_log_group_method ] ).

      DATA(lv_group_name) = lo_group_classdescr->absolute_name.
      DATA(lv_log_group_enabled) = VALUE de_bool( ).
      CALL METHOD (lv_group_name)=>(lc_log_group_method)
        RECEIVING rv_is_enabled = lv_log_group_enabled.
      CASE lv_log_group_enabled.
        WHEN mc_log_enabled-true
          OR mc_log_enabled-false.
          rv_is_enabled = det_cx_bool( lv_log_group_enabled ).
          RETURN.

      ENDCASE.

    ENDIF.

    rv_is_enabled = is_root_log_enabled( ).

  ENDMETHOD.


  METHOD log_messages.

    CHECK is_log_enabled( ) EQ abap_true.

    zial_cl_log=>get( )->log_exception( previous ).
    zial_cl_log=>get( )->log_exception( me ).

    DATA(lv_class_name) = cl_abap_classdescr=>get_class_name( me ).
    DATA(lt_components) = zial_cl_log=>get_components_from_msgde( mt_input_data ).
    MESSAGE e001(zial_exc_mgmt) WITH lv_class_name lt_components mv_subrc INTO DATA(lv_msg) ##NEEDED.
    DATA(lt_msgde) = create_log_msgde( mt_input_data ).
    zial_cl_log=>get( )->log_message( lt_msgde ).

  ENDMETHOD.


  METHOD reset_enable_log_instance.
    log_instance_enabled = mc_log_enabled-undef.
  ENDMETHOD.


  METHOD if_message~get_text.

    CLEAR: sy-msgid,
           sy-msgno,
           sy-msgty,
           sy-msgv1,
           sy-msgv2,
           sy-msgv3,
           sy-msgv4.

    IF ms_message IS NOT INITIAL.
      result = zial_cl_log=>to_string( is_bapiret = ms_message ).
    ELSEIF mt_messages IS NOT INITIAL.
      result = zial_cl_log=>to_string( is_bapiret = mt_messages[ 1 ] ).
    ELSE.
      result = super->get_text( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_message.

    IF ms_message IS INITIAL.
      IF mt_messages IS INITIAL.
        ms_message = VALUE #( id         = if_t100_message~t100key-msgid
                              number     = if_t100_message~t100key-msgno
                              type       = if_t100_dyn_msg~msgty
                              message_v1 = if_t100_dyn_msg~msgv1
                              message_v2 = if_t100_dyn_msg~msgv2
                              message_v3 = if_t100_dyn_msg~msgv3
                              message_v4 = if_t100_dyn_msg~msgv4 ).
      ELSE.
        ms_message = VALUE #( mt_messages[ 1 ] OPTIONAL ).
      ENDIF.
    ENDIF.

    ms_message-message = zial_cl_log=>to_string( iv_msgid = ms_message-id
                                                 iv_msgty = ms_message-type
                                                 iv_msgno = ms_message-number
                                                 iv_msgv1 = ms_message-message_v1
                                                 iv_msgv2 = ms_message-message_v2
                                                 iv_msgv3 = ms_message-message_v3
                                                 iv_msgv4 = ms_message-message_v4 ).

    rs_message = ms_message.

  ENDMETHOD.


  METHOD get_messages.

    IF         ms_message IS NOT INITIAL
       AND NOT line_exists( mt_messages[ table_line = ms_message ] ).
      INSERT ms_message INTO TABLE mt_messages.
    ENDIF.

    rt_messages = mt_messages.

  ENDMETHOD.


  METHOD get_obj_id.

    rv_obj_id = obj_id.

  ENDMETHOD.


  METHOD get_input_data.

    rt_input_data = mt_input_data.

  ENDMETHOD.


  METHOD det_bool.

    CASE iv_bool.
      WHEN abap_true.
        rv_result = mc_log_enabled-true.

      WHEN abap_false.
        rv_result = mc_log_enabled-false.

    ENDCASE.

  ENDMETHOD.


  METHOD det_cx_bool.

    CASE iv_cx_bool.
      WHEN mc_log_enabled-true.
        rv_result = abap_true.

      WHEN mc_log_enabled-false.
        rv_result = abap_false.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_cx_root~enable_log.

    log_root_enabled = det_bool( iv_enable ).

  ENDMETHOD.


  METHOD zif_cx_root~is_log_enabled.

    rv_is_enabled = log_root_enabled.

  ENDMETHOD.


  METHOD is_root_log_enabled.

    CASE log_root_enabled.
      WHEN mc_log_enabled-true
        OR mc_log_enabled-false.
        rv_is_enabled = det_cx_bool( log_root_enabled ).
        RETURN.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
