INTERFACE zial_if_exc_class
  PUBLIC.

  CLASS-METHODS is_log_class_enabled
    RETURNING
      VALUE(rv_log) TYPE cx_bool.
  CLASS-METHODS enable_log_class
    IMPORTING
      log_enabled TYPE abap_bool.

ENDINTERFACE.
