"! <p class="shorttext synchronized" lang="en">Dynamic error</p>
CLASS zcx_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_cx_class.

  PROTECTED SECTION.
    CLASS-DATA log_child_enabled TYPE de_bool VALUE mc_log_enabled-undef.

ENDCLASS.



CLASS zcx_error IMPLEMENTATION.

  METHOD zif_cx_class~enable_log.
    log_child_enabled = det_bool( iv_enable ).
  ENDMETHOD.


  METHOD zif_cx_class~is_log_enabled.
    rv_is_enabled = log_child_enabled.
  ENDMETHOD.

ENDCLASS.
