"! <p class="shorttext synchronized" lang="en">Missing parameter</p>
CLASS zcx_missing_parameter DEFINITION
  PUBLIC
  INHERITING FROM zcx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zial_if_exc_class.

  PROTECTED SECTION.
    CLASS-DATA log_child_enabled TYPE cx_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_missing_parameter IMPLEMENTATION.

  METHOD zial_if_exc_class~enable_log_class.
    log_child_enabled = det_bool( log_enabled ).
  ENDMETHOD.


  METHOD zial_if_exc_class~is_log_class_enabled.
    rv_log = log_child_enabled.
  ENDMETHOD.

ENDCLASS.
