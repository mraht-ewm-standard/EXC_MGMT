"! <p class="shorttext synchronized" lang="en">Missing parameter</p>
CLASS zcx_missing_parameter DEFINITION
  PUBLIC
  INHERITING FROM zcx_no_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_cx_class.

  PROTECTED SECTION.
    CLASS-DATA log_child_enabled TYPE cx_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_missing_parameter IMPLEMENTATION.

  METHOD zif_cx_class~enable_log.
    log_child_enabled = det_bool( iv_enable ).
  ENDMETHOD.


  METHOD zif_cx_class~is_log_enabled.
    rv_is_enabled = log_child_enabled.
  ENDMETHOD.

ENDCLASS.
