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
    CLASS-METHODS is_log_class_enabled
      RETURNING
        VALUE(rv_log) TYPE cx_bool.
    CLASS-METHODS enable_log_class
      IMPORTING
        log_enabled TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-DATA log_class_enabled TYPE cx_bool VALUE zcx_root=>undef.

    METHODS on_construction REDEFINITION.

ENDCLASS.


CLASS zcx_tmplt_child IMPLEMENTATION.

  METHOD on_construction.

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
