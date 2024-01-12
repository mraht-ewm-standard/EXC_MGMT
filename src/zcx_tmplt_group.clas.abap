"! <p class="shorttext synchronized" lang="en">{@link [CLASSNAME]}: Exceptions</p>
CLASS zcx_tmplt_group DEFINITION
  PUBLIC
  INHERITING FROM zcx_static_check
  CREATE PUBLIC
  ABSTRACT .

  PUBLIC SECTION.
    INTERFACES: zif_cx_group.

  PROTECTED SECTION.
    CLASS-DATA log_group_enabled TYPE de_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_tmplt_group IMPLEMENTATION.

  METHOD zif_cx_group~enable_log.
    log_group_enabled = det_bool( iv_enable ).
  ENDMETHOD.


  METHOD zif_cx_group~is_log_enabled.
    rv_is_enabled = log_group_enabled.
  ENDMETHOD.

ENDCLASS.
