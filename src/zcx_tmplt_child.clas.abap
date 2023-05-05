" Please adjust:
" 1) Replace [MEANING OF EXCEPTION]
" 2) INHERITING FROM [PARENT_CLASSNAME]
" 3) Adjust action name in LOG_INPUT( 'READ' )
" 4) Delete this paragraph

"! <p class="shorttext synchronized" lang="en">[MEANING OF EXCEPTION]</p>
CLASS zcx_tmplt_child DEFINITION
  PUBLIC
  INHERITING FROM zcx_tmplt_root
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid     LIKE if_t100_message=>t100key OPTIONAL
        previous   LIKE previous OPTIONAL
        log        TYPE abap_bool OPTIONAL
        message    TYPE bapiret2 OPTIONAL
        subrc      TYPE sysubrc DEFAULT sy-subrc
        input_data TYPE rsra_t_alert_definition OPTIONAL .

    CLASS-METHODS is_log_class_enabled
      RETURNING
        VALUE(rv_log) TYPE cx_bool.
    CLASS-METHODS enable_log_class
      IMPORTING
        log_enabled TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-DATA log_class_enabled TYPE cx_bool VALUE zcx_root=>undef.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_tmplt_child IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid     = textid
                        previous   = previous
                        log        = log
                        message    = message
                        subrc      = subrc
                        input_data = input_data ).

    object_type = mc_object_type.

    log_input( 'READ' ).

  ENDMETHOD.


  METHOD enable_log_class.
    log_class_enabled = zcx_root=>det_bool( log_enabled ).
  ENDMETHOD.


  METHOD is_log_class_enabled.
    rv_log = log_class_enabled.
  ENDMETHOD.

ENDCLASS.
