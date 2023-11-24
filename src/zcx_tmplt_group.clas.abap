"! <p class="shorttext synchronized" lang="en">{@link [CLASSNAME]}: Exceptions</p>
CLASS zcx_tmplt_group DEFINITION
  PUBLIC
  INHERITING FROM zcx_static_check
  CREATE PUBLIC
  ABSTRACT .

  PUBLIC SECTION.
    INTERFACES: zial_if_exc_group.

  PROTECTED SECTION.
    CLASS-DATA log_parent_enabled TYPE cx_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_tmplt_group IMPLEMENTATION.

  METHOD zial_if_exc_group~enable_log_group.
    log_parent_enabled = det_bool( iv_log_enabled ).
  ENDMETHOD.


  METHOD zial_if_exc_group~is_log_group_enabled.
    rv_log_enabled = log_parent_enabled.
  ENDMETHOD.

ENDCLASS.
