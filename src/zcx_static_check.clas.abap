CLASS zcx_static_check DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check ABSTRACT
  CREATE PUBLIC
  GLOBAL FRIENDS zcx_root.

  PUBLIC SECTION.
    INTERFACES zcx_if_check_class.

    ALIASES input_data   FOR zcx_if_check_class~input_data.
    ALIASES message      FOR zcx_if_check_class~message.
    ALIASES messages     FOR zcx_if_check_class~messages.
    ALIASES obj_id       FOR zcx_if_check_class~obj_id.
    ALIASES subrc        FOR zcx_if_check_class~subrc.

    ALIASES get_message  FOR zcx_if_check_class~get_message.
    ALIASES get_messages FOR zcx_if_check_class~get_messages.
    ALIASES log          FOR zcx_if_check_class~log.
    ALIASES log_info     FOR zcx_if_check_class~log_info.

    METHODS constructor
      IMPORTING textid     TYPE sotr_conc               OPTIONAL
                t100key    TYPE scx_t100key             OPTIONAL
                !previous  LIKE previous                OPTIONAL
                obj_id     TYPE objectname              DEFAULT zcx_root=>mc_obj_id-generic
                !message   TYPE bapiret2                OPTIONAL
                !messages  TYPE bapiret2_t              OPTIONAL
                !subrc     TYPE sysubrc                 DEFAULT sy-subrc
                input_data TYPE rsra_t_alert_definition OPTIONAL.

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


CLASS zcx_static_check IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid   = textid
                        previous = previous ).

    mo_cx_root = NEW #( io_exception  = me
                        is_t100key    = t100key
                        iv_obj_id     = obj_id
                        is_message    = message
                        it_messages   = messages
                        iv_subrc      = subrc
                        it_input_data = input_data ).

    on_construction( ).

  ENDMETHOD.


  METHOD on_construction.
  ENDMETHOD.


  METHOD zcx_if_check_class~get_message.
    rs_message = mo_cx_root->get_message( ).
  ENDMETHOD.


  METHOD zcx_if_check_class~get_messages.
    rt_messages = mo_cx_root->get_messages( ).
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


  METHOD zcx_if_check_class~log.
    mo_cx_root->log( ).
  ENDMETHOD.


  METHOD zcx_if_check_class~log_info.
    mo_cx_root->log_info( ).
  ENDMETHOD.

ENDCLASS.
