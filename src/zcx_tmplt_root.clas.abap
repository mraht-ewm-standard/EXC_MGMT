" Please adjust:
" 1) Replace [OBJECT_NAME] and [CLASSNAME]
" 2) Delete this paragraph

"! <p class="shorttext synchronized" lang="en">[OBJECT_NAME]-specific exceptions: {@link [CLASSNAME]}</p>
CLASS zcx_tmplt_root DEFINITION
  PUBLIC
  INHERITING FROM zcx_root
  CREATE PUBLIC
  ABSTRACT .

  PUBLIC SECTION.
    CLASS-METHODS enable_log_parent
      IMPORTING
        iv_log_enabled TYPE abap_bool.
    CLASS-METHODS is_log_parent_enabled
      RETURNING
        VALUE(rv_log_enabled) TYPE cx_bool.

  PROTECTED SECTION.
    CONSTANTS: mc_object_type TYPE char50 VALUE '[OBJECT_NAME]'.

    CLASS-DATA log_parent_enabled TYPE cx_bool VALUE zcx_root=>undef.

ENDCLASS.


CLASS zcx_tmplt_root IMPLEMENTATION.

  METHOD enable_log_parent.
    log_parent_enabled = zcx_root=>det_bool( iv_log_enabled ).
  ENDMETHOD.


  METHOD is_log_parent_enabled.
    rv_log_enabled = log_parent_enabled.
  ENDMETHOD.

ENDCLASS.
