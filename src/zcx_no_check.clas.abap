CLASS zcx_no_check DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check ABSTRACT
  CREATE PUBLIC
  GLOBAL FRIENDS zcx_root.

  PUBLIC SECTION.
    INTERFACES zcx_if_check_class.

    ALIASES get_message  FOR zcx_if_check_class~get_message.
    ALIASES get_messages FOR zcx_if_check_class~get_messages.

    METHODS constructor
      IMPORTING iv_textid     TYPE sotr_conc               OPTIONAL
                is_t100key    TYPE scx_t100key             OPTIONAL
                io_previous   LIKE previous                OPTIONAL
                iv_obj_id     TYPE objectname              DEFAULT zcx_root=>mc_obj_id-generic
                is_message    TYPE bapiret2                OPTIONAL
                it_messages   TYPE bapiret2_t              OPTIONAL
                iv_subrc      TYPE sysubrc                 DEFAULT sy-subrc
                it_input_data TYPE rsra_t_alert_definition OPTIONAL.

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

    super->constructor( textid   = iv_textid
                        previous = io_previous ).

    mo_cx_root = NEW #( io_exception  = me
                        is_t100key    = is_t100key
                        iv_obj_id     = iv_obj_id
                        is_message    = is_message
                        it_messages   = it_messages
                        iv_subrc      = iv_subrc
                        it_input_data = it_input_data ).

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
        result = super->if_message~get_text( ).
        mo_cx_root->reset_call_on_super( ).

      WHEN abap_false.
        result = mo_cx_root->if_message~get_text( ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
