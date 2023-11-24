" Please adjust:
" 1) Replace [MEANING OF EXCEPTION]
" 2) INHERITING FROM [PARENT_CLASSNAME]
" 3) Adjust action name in LOG_INPUT( 'READ' )
" 4) Delete this paragraph

"! <p class="shorttext synchronized" lang="en">[MEANING OF EXCEPTION]</p>
CLASS zcx_tmplt_class DEFINITION
  PUBLIC
  INHERITING FROM zcx_tmplt_group
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zial_if_exc_class.

  PROTECTED SECTION.
    CLASS-DATA log_class_enabled TYPE cx_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_tmplt_class IMPLEMENTATION.

  METHOD zial_if_exc_class~enable_log_class.
    log_class_enabled = det_bool( log_enabled ).
  ENDMETHOD.


  METHOD zial_if_exc_class~is_log_class_enabled.
    rv_log = log_class_enabled.
  ENDMETHOD.

ENDCLASS.
