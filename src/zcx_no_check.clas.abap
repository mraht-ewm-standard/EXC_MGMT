CLASS zcx_no_check DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check ABSTRACT
  CREATE PUBLIC
  GLOBAL FRIENDS zcx_root.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.
    INTERFACES zif_cx_custom.

    ALIASES get_previous   FOR zif_cx_custom~get_previous.
    ALIASES get_obj_id     FOR zif_cx_custom~get_obj_id.
    ALIASES set_obj_id     FOR zif_cx_custom~set_obj_id.
    ALIASES get_message    FOR zif_cx_custom~get_message.
    ALIASES set_message    FOR zif_cx_custom~set_message.
    ALIASES get_messages   FOR zif_cx_custom~get_messages.
    ALIASES set_messages   FOR zif_cx_custom~set_messages.
    ALIASES get_subrc      FOR zif_cx_custom~get_subrc.
    ALIASES set_subrc      FOR zif_cx_custom~set_subrc.
    ALIASES get_input_data FOR zif_cx_custom~get_input_data.
    ALIASES set_input_data FOR zif_cx_custom~set_input_data.
    ALIASES get_super_text FOR zif_cx_custom~get_super_text.

    METHODS constructor
      IMPORTING is_textid     LIKE if_t100_message=>t100key OPTIONAL
                io_previous   LIKE previous                 OPTIONAL
                iv_obj_id     TYPE objectname               DEFAULT zcx_root=>mc_obj_id-generic
                is_message    TYPE bapiret2                 OPTIONAL
                it_messages   TYPE bapiret2_t               OPTIONAL
                iv_subrc      TYPE sysubrc                  DEFAULT sy-subrc
                it_input_data TYPE rsra_t_alert_definition  OPTIONAL.

    METHODS if_message~get_text REDEFINITION.

  PROTECTED SECTION.
    DATA mo_cx_root TYPE REF TO zcx_root.

    "! <p class="shorttext synchronized">Custom code on construction</p>
    "! <p>Customer-specific construction as redefinition of constructor is not
    "! allowed and one would have to define the whole constructor over and
    "! over again in each sub class. Thus wouldn't have to change all sub
    "! classes if the signature of the constructor would change in ROOT class.</p>
    METHODS on_construction.

ENDCLASS.


CLASS zcx_no_check IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = io_previous ).

    CLEAR me->textid.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.

    mo_cx_root = NEW #( io_exception  = me
                        io_previous   = io_previous
                        iv_obj_id     = iv_obj_id
                        is_message    = is_message
                        it_messages   = it_messages
                        iv_subrc      = iv_subrc
                        it_input_data = it_input_data ).

    on_construction( ).

  ENDMETHOD.


  METHOD on_construction.

    " Redefine if needed

  ENDMETHOD.


  METHOD zif_cx_custom~get_previous.
    ro_previous = me->previous.
  ENDMETHOD.


  METHOD zif_cx_custom~get_obj_id.
    rv_obj_id = mo_cx_root->get_obj_id( ).
  ENDMETHOD.


  METHOD zif_cx_custom~set_obj_id.
    mo_cx_root->set_obj_id( iv_obj_id ).
  ENDMETHOD.


  METHOD zif_cx_custom~get_message.
    rs_message = mo_cx_root->get_message( ).
  ENDMETHOD.


  METHOD zif_cx_custom~set_message.
    mo_cx_root->set_message( is_message ).
  ENDMETHOD.


  METHOD zif_cx_custom~get_messages.
    rt_messages = mo_cx_root->get_messages( ).
  ENDMETHOD.


  METHOD zif_cx_custom~set_messages.
    mo_cx_root->set_messages( it_messages ).
  ENDMETHOD.


  METHOD zif_cx_custom~get_subrc.
    rv_subrc = mo_cx_root->get_subrc( ).
  ENDMETHOD.


  METHOD zif_cx_custom~set_subrc.
    mo_cx_root->set_subrc( iv_subrc ).
  ENDMETHOD.


  METHOD zif_cx_custom~get_input_data.
    rt_input_data = mo_cx_root->get_input_data( ).
  ENDMETHOD.


  METHOD zif_cx_custom~set_input_data.
    mo_cx_root->set_input_data( it_input_data ).
  ENDMETHOD.


  METHOD if_message~get_text.

    CASE mo_cx_root->get_call_on_super( ).
      WHEN abap_true.
        result = super->get_text( ).
        mo_cx_root->reset_call_on_super( ).

      WHEN abap_false.
        result = mo_cx_root->get_text( ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_cx_custom~get_super_text.
    mo_cx_root->register_call_on_super( ).
    rv_result = CAST cx_no_check( me )->get_text( ).
  ENDMETHOD.

ENDCLASS.
